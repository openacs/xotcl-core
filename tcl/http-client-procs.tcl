xo::library doc {

  XOTcl implementation for synchronous and asynchronous
  HTTP and HTTPS requests

  @author Gustaf Neumann, Stefan Sobernig
  @creation-date 2007-10-05
  @cvs-id $Id$
}


#
# The XOTcl HTTP client procs are deprecated.
# Use util::http::get/post/... etc. instead
#

if {![ad_with_deprecated_code_p]} {
    ns_log notice "http-client-procs: skip deprecated code"
    return
}
ns_log notice "http-client-procs include deprecated code"


namespace eval ::xo {
  #
  # Defined classes
  #  1) HttpCore (common base class)
  #  2) HttpRequest (for blocking requests + timeout support)
  #  3) AsyncHttpRequest (for nonblocking requests + timeout support)
  #  4) HttpRequestTrace (mixin class)
  #  5) Tls (mixin class, applicable to various protocols)
  #
  ######################
  #
  # 1 HttpRequest
  #
  # HttpRequest is a class to implement the client side
  # for the HTTP methods GET and POST.
  #
  # Example of a GET request:
  #
  #  set r [::xo::HttpRequest new -url http://www.openacs.org/]
  #
  # The resulting object $r contains all information
  # about the requests, such as e.g. status_code or
  # data (the response body from the server). For details
  # look into the output of [$r serialize]. The result
  # object $r is automatically deleted at cleanup of
  # a connection thread.
  #
  # Example of a POST request with a form with var1 and var2
  # (providing post_data causes the POST request).
  #
  #  set r [::xo::HttpRequest new \
                         #             -url http://yourhost.yourdomain/yourpath \
                         #             -post_data [export_vars {var1 var2}] \
                         #             -content_type "application/x-www-form-urlencoded; charset=UTF-8"]
  #
  # More recently, we added timeout support for blocking http
  # requests. By passing a timeout parameter, you gain control
  # on the total roundtrip time (in milliseconds, ms):
  #
  #  set r [::xo::HttpRequest new \
                         #          -url http://www.openacs.org/ \
                         #          -timeout 1500]
  #
  # Please, make sure that you use a recent distribution of tclthread
  # ( > 2.6.5 ) to have the blocking-timeout feature working
  # safely. This newly introduced feature makes use of advanced thread
  # synchronization offered by tclthread that needed to be fixed in
  # tclthread <= 2.6.5. At the time of this writing, there was no
  # post-2.6.5 release of tclthread, hence, you are required to obtain a
  # CVS snapshot, dating at least 2008-05-23. E.g.:
  #
  # cvs -z3 -d:pserver:anonymous@tcl.cvs.sourceforge.net:/cvsroot/tcl co \
                         #         -D 20080523 -d thread2.6.5~20080523 thread
  #
  # Provided that the Tcl module tls (see e.g. http://tls.sourceforge.net/)
  # is available and can be loaded via "package require tls" into
  # the AOLserver, you can use both TLS/SSL secured or insecured requests
  # in the synchronous/ asynchronous mode by using an
  # https url.
  #
  #  set r [::xo::HttpRequest new -url https://learn.wu-wien.ac.at/]
  #
  ######################
  #
  # 2 AsyncHttpRequest
  #
  # AsyncHttpRequest is a subclass for HttpCore implementing
  # asynchronous HTTP requests without vwait (vwait causes
  # stalls on AOLserver). AsyncHttpRequest requires to provide a listener
  # or callback object that will be notified upon success or failure of
  # the request.
  #
  # Asynchronous requests are much more complex to handle, since
  # an application (a connection thread) can submit multiple
  # asynchronous requests in parallel, which are likely to
  # finish after the current request is done. The advantages
  # are that the spooling of data can be delegated to a spooling
  # thead and the connection thread is available for handling more
  # incoming connections. The disadvantage is the higher
  # complexity, one needs means to collect the received data.
  #
  #
  # The following example uses the background delivery thread for
  # spooling and defines in this thread a listener. This generic
  # listener can be subclasses in applications.
  #
  # When using asynchronous requests, make sure to specify a listener
  # for the callbacks and delete finally the request object in the
  # bgdelivery thread.
  #
  #  ::bgdelivery do ::xo::AsyncHttpRequest new \
                         #     -url "https://oacs-dotlrn-conf2007.wu-wien.ac.at/conf2007/" \
                         #     -mixin ::xo::AsyncHttpRequest::SimpleListener
  #     -proc finalize {obj status value} { :destroy }
  #
  ######################
  #
  # 3 HttpRequestTrace
  #
  # HttpRequestTrace can be used to trace one or all requests.
  # If activated, the class writes protocol data into
  # [ad_tmpdir]/req-<somenumber>.
  #
  # Use
  #
  #  ::xo::HttpCore instmixin add ::xo::HttpRequestTrace
  #
  # to activate trace for all requests,
  # or mixin the class into a single request to trace it.
  #

  Class create HttpCore \
      -slots {
        Attribute create host
        Attribute create protocol -default "http"
        Attribute create port
        Attribute create path -default "/"
        Attribute create url
        Attribute create method
        Attribute create post_data -default ""
        Attribute create content_type \
            -default "text/plain; charset=[ns_config ns/parameters OutputCharset iso-8859-1]"
        Attribute create request_header_fields -default {}
        Attribute create user_agent -default "xohttp/0.2"
      }

  HttpCore instproc set_default_port {protocol} {
    switch -- $protocol {
      http  {set :port 80}
      https {set :port 443}
    }
  }

  HttpCore instproc parse_url {} {
    :instvar protocol url host port path
    if {[regexp {^(http|https)://([^/]+)(/.*)?$} $url _ protocol host path]} {
      # Be friendly and allow strictly speaking invalid URLs
      # like "http://www.openacs.org"  (no trailing slash)
      if {$path eq ""} {set path /}
      :set_default_port $protocol
      regexp {^([^:]+):(.*)$} $host _ host port
    } else {
      error "unsupported or invalid url '$url'"
    }
  }

  HttpCore instproc open_connection {} {
    :instvar host port S
    set S [socket -async $host $port]
  }

  HttpCore instproc get_channel_settings {
    {-text_translation {auto binary}}
    content_type
  } {
    #
    # 1. NOTE: We have to treat translation and encoding settings
    # separately. "Defaulting" to "binary" translation would imply a
    # "binary" encoding: [fconfigure -translation binary] "[...] sets
    # the encoding to binary (which disables encoding filtering)",
    # i.e. it is idempotent to [fconfigure -translation binary
    # -encoding binary].
    #
    # see also http://docs.activestate.com/activetcl/8.5/tcl/TclCmd/fconfigure.htm
    #
    # 2. Note: I would claim here that we could stick with binary
    # translations, effectively deactivating any eol/eof
    # interpretations. As we use the byte-oriented [read] rathen than
    # the line-oriented [gets] in the processing of HTTP bodies of replies
    # ([gets] is only applied for header processing), this should be
    # fine. Anyways, I leave it as is for the moment ...
    #
    set content_type [string tolower $content_type]
    set trl [expr {[string match "text/*" $content_type] ? $text_translation : "binary"}]

    #
    # 3. In the following, an IANA/MIME charset resolution scheme is
    # implemented which is compliant with RFC 3023 which deals with
    # treating XML media types properly.
    #
    # see http://tools.ietf.org/html/rfc3023
    #
    # This makes the use of [ns_encodingfortype] obsolete as this
    # helper proc does not consider RFC 3023 at all. In the future,
    # RFC 3023 support should enter a revised [ns_encodingfortype],
    # for now, we fork.
    #
    # The mappings between Tcl encoding names (as shown by [encoding
    # names]) and IANA/MIME charset names (i.e., names and aliases in
    # the sense of http://www.iana.org/assignments/character-sets) is
    # provided by ...
    #
    # i. A static, built-in correspondence map: see nsd/encoding.c
    # ii. An extensible correspondence map (i.e., the ns/charsets
    # section in config.tcl).
    #
    # For mapping charset to encoding names, I use
    # [ns_encodingforcharset].
    #
    # Note, there are also alternatives for resolving IANA/MIME
    # charset names to Tcl encoding names, however, they all have
    # issues (non-extensibility from standard configuration sites,
    # incompleteness, redundant thread-local storing, scripted
    # implementation):
    # 1. Tcllib/mime package: ::mime::reversemapencoding()
    # 2. Tdom: tDOM::IANAEncoding2TclEncoding(); see lib/tdom.tcl

    #
    # RFC 3023 support (at least in my reading) demands the following
    # resolution order (see also Section 3.6 in RFC 3023), when
    # applied along with RFC 2616 (see especially Section 3.7.1 in RFC 2616)
    #
    # (A) Check for the "charset" parameter on certain (!) media types:
    # an explicitly stated, yet optional "charset" parameter is
    # permitted for all text/* media subtypes (RFC 2616) and selected
    # the XML media type classes listed by RFC 3023 (beyond the text/*
    # media type; e.g. "application/xml*", "*/*+xml", etc.).
    #
    # (B) If the "charset" is omitted, certain default values apply (!):
    #
    #    (B.1) RFC 3023 text/* registrations default to us-ascii (!),
    #    and not iso-8859-1 (overruling RFC 2616).
    #
    #   (B.2) RFC 3023 application/* and non-text "+xml" registrations
    #    are to be left untreated (in our context, no encoding
    #    filtering is to be applied -> "binary")
    #
    #   (B.3) RFC 2616 text/* registration (if not covered by B.1)
    #   default to iso-8859-1
    #
    # (C) If neither A or B apply (e.g., because an invalid charset
    # name was given to the charset parameter), we default to
    # "binary". This corresponds to the behavior of
    # [ns_encodingfortype].  Also note that the RFCs 3023 and 2616 do
    # not state any procedure when "invalid" charsets etc. are
    # identified. I assume, RFC-compliant clients have to ignore them
    # which means keep the channel in- and output unfiltered (encoding
    # = "binary"). This requires the client of the *HttpRequest* to
    # treat the data accordingly.
    #

    set enc ""
    if {[regexp {^text/.*$|^.*/xml.*$|^.*\+xml.*$} $content_type]} {
      # Case (A): Check for an explicitly provided charset parameter
      if {[regexp {;\s*charset\s*=([^;]*)} $content_type _ charset]} {
        set enc [ns_encodingforcharset [string trim $charset]]
      }
      # Case (B.1)
      if {$enc eq "" && [regexp {^text/xml.*$|text/.*\+xml.*$} $content_type]} {
        set enc [ns_encodingforcharset us-ascii]
      }

      # Case (B.3)
      if {$enc eq "" && [string match "text/*" $content_type]} {
        set enc [ns_encodingforcharset iso-8859-1]
      }
    }

    # Cases (C) and (B.2) are covered by the [expr] below.
    return [list encoding [expr {$enc eq ""?"binary":$enc}] translation $trl]
  }





  HttpCore instproc init {} {
    :instvar S post_data host port protocol
    :destroy_on_cleanup

    set :meta [list]
    set :data ""
    if {![info exists :method]} {
      set :method [expr {$post_data eq "" ? "GET" : "POST"}]
    }
    if {[info exists :url]} {
      :parse_url
    } else {
      if {![info exists port]} {:set_default_port $protocol}
      if {![info exists host]} {
        error "either host or url must be specified"
      }
    }
    if {$protocol eq "https"} {
      package require tls
      if {[info commands ::tls::import] eq ""} {
        error "https request require the Tcl module TLS to be installed\n\
             See e.g. http://tls.sourceforge.net/"
      }
      #
      # Add HTTPs handling
      #
      :mixin add ::xo::Tls
    }
    if {[catch {:open_connection} err]} {
      :cancel "error during open connection via $protocol to $host $port: $err"
    }
  }

  HttpCore instproc send_request {} {
    :instvar S post_data host method
    if {[catch {
      puts $S "$method [:path] HTTP/1.0"
      puts $S "Host: $host"
      puts $S "User-Agent: [:user_agent]"
      foreach {tag value} [:request_header_fields] {
        #regsub -all \[\n\r\] $value {} value
        #set tag [string trim $tag]
        puts $S "$tag: $value"
      }
      :$method
    } err]} {
      :cancel "error send $host [:port]: $err"
      return
    }
  }

  HttpCore instproc GET {} {
    :instvar S
    puts $S ""
    :request_done
  }

  HttpCore instproc POST {} {
    :instvar S post_data
    array set "" [:get_channel_settings [:content_type]]
    if {$(encoding) ne "binary"} {
      set post_data [encoding convertto $(encoding) $post_data]
    }
    puts $S "Content-Length: [string length $post_data]"
    puts $S "Content-Type: [:content_type]"
    puts $S ""
    fconfigure $S -translation $(translation) -encoding binary
    :send_POST_data
  }
  HttpCore instproc send_POST_data {} {
    :instvar S post_data
    puts -nonewline $S $post_data
    :request_done
  }
  HttpCore instproc request_done {} {
    :instvar S
    flush $S
    :reply_first_line
  }

  HttpCore instproc close {} {
    catch {close ${:S}} errMsg
    :debug "--- closing socket socket?[info exists :S] => $errMsg"
  }

  HttpCore instproc cancel {reason} {
    set :status canceled
    set :cancel_message $reason
    :debug "--- canceled for $reason"
    :close
  }

  HttpCore instproc finish {} {
    set :status finished
    :close
    :debug "--- [:host] [:port] [:path] has finished"
  }
  HttpCore instproc getLine {var} {
    :upvar $var response
    :instvar S
    set n [gets $S response]
    if {[eof $S]} {
      :debug "--premature eof"
      return -2
    }
    if {$n == -1} {:debug "--input pending, no full line"; return -1}
    return $n
  }
  HttpCore instproc reply_first_line {} {
    :instvar S status_code
    fconfigure $S -translation crlf
    set n [:getLine response]
    switch -exact -- $n {
      -2 {:cancel premature-eof; return}
      -1 {:finish; return}
    }
    if {[regexp {^HTTP/([0-9.]+) +([0-9]+) *} $response _ \
             responseHttpVersion status_code]} {
      :reply_first_line_done
    } else {
      :cancel "unexpected-response '$response'"
    }
  }
  HttpCore instproc reply_first_line_done {} {
    :header
  }
  HttpCore instproc header {} {
    while {1} {
      set n [:getLine response]
      switch -exact -- $n {
        -2 {:cancel premature-eof; return}
        -1 {continue}
        0 {break}
        default {
          #:debug "--header $response"
          if {[regexp -nocase {^content-length:(.+)$} $response _ length]} {
            set :content_length [string trim $length]
          } elseif {[regexp -nocase {^content-type:(.+)$} $response _ type]} {
            set :content_type [string trim $type]
          }
          if {[regexp -nocase {^([^:]+): *(.+)$} $response _ key value]} {
            lappend :meta [string tolower $key] $value
          }
        }
      }
    }
    :reply_header_done
  }
  HttpCore instproc reply_header_done {} {
    :instvar S
    # we have received the header, including potentially the
    # content_type of the returned data
    array set "" [:get_channel_settings [:content_type]]
    fconfigure $S -translation $(translation) -encoding $(encoding)
    if {[info exists :content_length]} {
      set :data [read ${:S} ${:content_length}]
    } else {
      set :data [read ${:S}]
    }
    :finish
  }

  HttpCore instproc set_status {key newStatus {value ""}} {
    nsv_set bgdelivery $key [list $newStatus $value]
  }

  HttpCore instproc unset_status {key} {
    nsv_unset bgdelivery $key
  }

  HttpCore instproc exists_status {key} {
    return [nsv_exists bgdelivery $key]
  }

  HttpCore instproc get_status {key} {
    return [lindex [nsv_get bgdelivery $key] 0]
  }

  HttpCore instproc get_value_for_status {key} {
    return [lindex [nsv_get bgdelivery $key] 1]
  }



  #
  # Synchronous (blocking) requests
  #

  Class create HttpRequest -superclass HttpCore -slots {
    Attribute create timeout -type integer
  }

  HttpRequest instproc init {} {
    # :log "[info exists :timeout]"
    if {[info exists :timeout] && [:timeout] > 0} {
      # create a cond and mutex
      set cond  [thread::cond create]
      set mutex [thread::mutex create]

      thread::mutex lock $mutex

      # start the asynchronous request
      :debug "--a create new  ::xo::AsyncHttpRequest"
      set req [bgdelivery do -async ::xo::AsyncHttpRequest new \
                   -mixin ::xo::AsyncHttpRequest::RequestManager \
                   -url [:url] \
                   -timeout [:timeout] \
                   -post_data [:post_data] \
                   -request_header_fields [:request_header_fields] \
                   -content_type [:content_type] \
                   -user_agent [:user_agent] \
                   -condition $cond]

      while {1} {
        :set_status $cond COND_WAIT_TIMEOUT
        thread::cond wait $cond $mutex [:timeout]

        set status [:get_status $cond]
        :debug "status after cond-wait $status"

        if {$status ne "COND_WAIT_REFRESH"} break
      }
      if {$status eq "COND_WAIT_TIMEOUT"} {
        :set_status $cond "COND_WAIT_CANCELED"
      }
      set status_value [:get_value_for_status $cond]
      if {$status eq "JOB_COMPLETED"} {
        set :data $status_value
      } else {
        set msg "Timeout-constraint, blocking HTTP request failed. Reason: '$status'"
        if {$status_value ne ""} {
          append msg " ($status_value)"
        }
        error $msg
      }
      thread::cond destroy $cond
      thread::mutex unlock $mutex
      thread::mutex destroy $mutex
      :unset_status $cond
    } else {
      next    ;# HttpCore->init
      #
      # test whether open_connection yielded
      # a socket ...
      #
      # :log "after core init, S?[info exists :S]"
      if {[info exists :S]} {
        :send_request
      }
    }
  }

  #
  # Asynchronous (nonblocking) requests
  #

  Class create AsyncHttpRequest -superclass HttpCore -slots {
    Attribute create timeout -type integer -default 10000 ;# 10 seconds
    Attribute create request_manager
  }
  AsyncHttpRequest instproc set_timeout {} {
    :cancel_timeout
    :debug "--- setting socket timeout: ${:timeout}"
    set :timeout_handle [after ${:timeout} [self] cancel timeout]
  }
  AsyncHttpRequest instproc cancel_timeout {} {
    if {[info exists :timeout_handle]} {
      after cancel ${:timeout_handle}
    }
  }
  AsyncHttpRequest instproc send_request {} {
    # remove fileevent handler explicitly
    fileevent ${:S} writable {}
    next
  }
  AsyncHttpRequest instproc init {} {
    :notify start_request
    :set_timeout
    next
    #
    # test whether open_connection yielded
    # a socket ...
    #
    if {[info exists :S]} {
      fileevent ${:S} writable [list [self] send_request]
    }
  }
  AsyncHttpRequest instproc notify {method {arg ""}} {
    if {[info exists :request_manager]} {
      [:request_manager] $method $arg [self]
    }
  }
  AsyncHttpRequest instproc POST {} {
    if {[info exists :S]} {fconfigure ${:S} -blocking false}
    fileevent ${:S} writable [list [self] send_POST_data]
    set :bytes_sent 0
    next
  }
  AsyncHttpRequest instproc send_POST_data {} {
    :instvar S post_data bytes_sent
    :set_timeout
    set total_bytes [string length $post_data]
    if {$bytes_sent < $total_bytes} {
      set to_send [expr {$total_bytes - $bytes_sent}]
      set block_size [expr {$to_send < 4096 ? $to_send : 4096}]
      set next_block_size [expr {$bytes_sent + $block_size}]
      set block [string range $post_data $bytes_sent $next_block_size-1]
      :notify request_data $block
      puts -nonewline $S $block
      set bytes_sent $next_block_size
    } else {
      fileevent $S writable ""
      :request_done
    }
  }
  AsyncHttpRequest instproc cancel {reason} {
    if {$reason ne "timeout"} {
      :cancel_timeout
    }
    next
    :notify failure $reason
  }
  AsyncHttpRequest instproc finish {} {
    :cancel_timeout
    next
    :debug "--- finished data ${:data}"
    :notify success ${:data}
  }
  AsyncHttpRequest instproc request_done {} {
    :notify start_reply
    :set_timeout
    :instvar S
    flush $S
    fconfigure $S -blocking false
    fileevent $S readable [list [self] reply_first_line]
  }
  AsyncHttpRequest instproc reply_first_line_done {} {
    :set_timeout
    :instvar S
    fileevent $S readable [list [self] header]
  }
  AsyncHttpRequest instproc reply_header_done {} {
    :instvar S
    :set_timeout
    # we have received the header, including potentially the
    # content_type of the returned data
    array set "" [:get_channel_settings [:content_type]]
    fconfigure $S -translation $(translation) -encoding $(encoding)
    fileevent ${:S} readable [list [self] receive_reply_data]
  }
  AsyncHttpRequest instproc receive_reply_data {} {
    :instvar S
    :debug "JOB receive_reply_data eof=[eof $S]"
    if {[eof $S]} {
      :finish
    } else {
      :set_timeout
      set block [read $S]
      :notify reply_data $block
      append :data $block
      #:debug "received [string length $block] bytes"
    }
  }

  #
  # SimpleListener defines a mixin class for providing a stub
  # implementation for callbacks of the asynchrous HTTP requests.
  # This class is typically run in the scope of bgdelivery
  #

  Class create AsyncHttpRequest::SimpleListener \
      -instproc init {} {
        :debug "INIT- NEXT=[self next]"
        # register request object as its own request_manager
        :request_manager [self]
        next

      } -instproc start_request {payload obj} {
        :debug "request $obj started"

      } -instproc request_data {payload obj} {
        :debug "partial or complete post"

      } -instproc start_reply {payload obj} {
        :debug "reply $obj started"

      } -instproc reply_data {payload obj} {
        :debug "partial or complete delivery"

      } -instproc finalize {obj status value} {
        :debug "finalize $obj $status"
        # this is called as a single method after success or failure
        next

      } -instproc success {payload obj} {
        :debug "[string length $payload] bytes payload"
        #if {[string length $payload]<600} {:log payload=$payload}
        # this is called as after a successful request
        :finalize $obj "JOB_COMPLETED" $payload

      } -instproc failure {reason obj} {
        :log "[self proc] [self args]"
        :log "failed for '$reason'"
        # this is called as after an unsuccessful request
        :finalize $obj "JOB_FAILED" $reason

      } -instproc unknown {method args} {
        :log "[self proc] [self args]"
        :log "UNKNOWN $method"
      }

  # Mixin class, used to turn instances of
  # AsyncHttpRequest into result callbacks
  # in the scope of bgdelivery, realising
  # the blocking-timeout feature ...
  #

  Class create AsyncHttpRequest::RequestManager \
      -superclass AsyncHttpRequest::SimpleListener \
      -slots {
        Attribute create condition
      } -instproc finalize {obj status value} {
        # set the result and do the notify
        :instvar condition
        # If a job was canceled, the status variable might not exist
        # anymore, the condition might be already gone as well.  In
        # this case, we do not have to perform the cond-notify.
        if {[:exists_status $condition] &&
            [:get_status $condition] eq "COND_WAIT_REFRESH"} {
        }
        if {[:exists_status $condition] &&
            (  [:get_status $condition] eq "COND_WAIT_REFRESH"
               || [:get_status $condition] eq "COND_WAIT_TIMEOUT")
          } {
          # Before, we had here one COND_WAIT_TIMEOUT, and once
          # COND_WAIT_REFRESH
          :set_status $condition $status $value
          catch {thread::cond notify $condition}
          $obj debug "--- destroying after finish"
          $obj destroy
        }

      } -instproc set_cond_timeout {} {
        :instvar condition
        if {[:exists_status $condition] &&
            [:get_status $condition] eq "COND_WAIT_TIMEOUT"} {
          :set_status $condition COND_WAIT_REFRESH
          catch {thread::cond notify $condition}
        }

      } -instproc start_request {payload obj} {
        :debug "JOB start request $obj"
        :set_cond_timeout

      } -instproc request_data {payload obj} {
        :debug "JOB request data $obj [string length $payload]"
        :set_cond_timeout

      } -instproc start_reply {payload obj} {
        :debug "JOB start reply $obj"
        :set_cond_timeout

      } -instproc reply_data {payload obj} {
        :debug "JOB reply data $obj [string length $payload]"
        :set_cond_timeout

      }

  #
  # TLS/SSL support
  #
  # Perform HTTPS requests via TLS (does not require nsopenssl)
  # - requires tls 1.5.0 to be compiled into <AOLserver>/lib/ ...
  # - - - - - - - - - - - - - - - - - -
  # - see http://www.ietf.org/rfc/rfc2246.txt
  # - http://wp.netscape.com/eng/ssl3/3-SPEC.HTM
  # - - - - - - - - - - - - - - - - - -

  Class create Tls
  Tls instproc open_connection {} {
    :instvar S
    #
    # first perform regular initialization of the socket
    #
    next
    #
    # then import tls (could configure it here in more detail)
    #
    ::tls::import $S
  }


  #
  # Trace Requests
  #

  Class create HttpRequestTrace
  nsv_set HttpRequestTrace count 0

  HttpRequestTrace instproc init {} {
    :instvar F post_data
    set :meta [list]
    set :requestCount [nsv_incr HttpRequestTrace count]  ;# make it an instvar to find it in the log file
    set F [open [ad_tmpdir]/req-[format %.4d ${:requestCount}] w]

    set method [expr {$post_data eq "" ? "GET" : "POST"}]
    puts $F "$method [:path] HTTP/1.0"
    puts $F "Host: [:host]"
    puts $F "User-Agent: [:user_agent]"
    foreach {tag value} [:request_header_fields] { puts $F "$tag: $value" }
    next
  }

  HttpRequestTrace instproc POST {} {
    :instvar F post_data
    puts $F "Content-Length: [string length $post_data]"
    puts $F "Content-Type: [:content_type]"
    puts $F ""
    fconfigure $F -translation {auto binary}
    puts -nonewline $F $post_data
    next
  }

  HttpRequestTrace instproc cancel {reason} {
    catch {close ${:F}}
    next
  }
  HttpRequestTrace instproc finish {} {
    catch {close ${:F}}
    next
  }

  #
  # To activate trace for all requests, uncomment the following line.
  # To trace a single request, mixin ::xo::HttpRequestTrace into the request.
  #
  # HttpCore instmixin add ::xo::HttpRequestTrace
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
