ad_library {
  
  XOTcl implementation for synchronous and asynchronous 
  HTTP and HTTPS requests

  @author Gustaf Neumann, Stefan Sobernig
  @creation-date 2007-10-05
  @cvs-id $Id$
}

namespace eval ::xo {
  #
  # Defined classes
  #  1) HttpCore (common base class)
  #  2) HttpRequest (for blocking requests + timeout support)
  #  3) AsyncHttpRequest (for non-blocking requests + timeout support)
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
  #  		-url http://www.openacs.org/ \
  #  		-timeout 1500]
  #
  # Please, make sure that you use a recent distribution of tclthread
  # ( > 2.6.5 ) to have the blocking-timeout feature working
  # safely. This newly introduced feature makes use of advanced thread
  # synchronisation offered by tclthread that needed to be fixed in
  # tclthread <= 2.6.5. At the time of this writing, there was no
  # post-2.6.5 release of tclthread, hence, you are required to obtain a
  # CVS snapshot, dating at least 2008-05-23. E.g.:
  # 
  # cvs -z3 -d:pserver:anonymous@tcl.cvs.sourceforge.net:/cvsroot/tcl co \
  #		 -D 20080523 -d thread2.6.5~20080523 thread
  #
  # Provided that the Tcl module tls (see e.g. http://tls.sourceforge.net/)
  # is available and can be loaded via "package require tls" into 
  # the aolserver, you can use both TLS/SSL secured or unsecured requests 
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
  # stalls on aolserver). AsyncHttpRequest requires to provide a listener 
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
  #     -proc finalize {obj status value} { my destroy }
  #
  ######################
  #
  # 3 HttpRequestTrace
  #
  # HttpRequestTrace can be used to trace one or all requests.
  # If activated, the class writes protocol data into 
  # /tmp/req-<somenumber>.
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
        Attribute create content_type -default "text/plain"
        Attribute create request_header_fields -default {}
        Attribute create user_agent -default "xohttp/0.2"
      }

  HttpCore instproc set_default_port {protocol} {
    switch -- $protocol {
      http  {my set port 80}
      https {my set port 443}
    }
  }

  HttpCore instproc parse_url {} {
    my instvar protocol url host port path
    if {[regexp {^(http|https)://([^/]+)(/.*)?$} $url _ protocol host path]} {
      # Be friendly and allow strictly speaking invalid urls 
      # like "http://www.openacs.org"  (no trailing slash)
      if {$path eq ""} {set path /}
      my set_default_port $protocol
      regexp {^([^:]+):(.*)$} $host _ host port
    } else {
      error "unsupported or invalid url '$url'"
    }
  }

  HttpCore instproc open_connection {} {
    my instvar host port S
    set S [socket -async $host $port]
  }

  HttpCore instproc set_encoding {{-text_translation {auto binary}} content_type} {
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
    # 2. TODO: I would claim here that we could stick with binary
    # translations, effectively deactivating any eol/eof
    # interpretations. As we use the byte-oriented [read] rathen than
    # the line-oriented [gets] in the processing of HTTP bodies of replies
    # ([gets] is only applied for header processing), this should be
    # fine. Anyways, I leave it as is for the moment ...
    #

    set trl [expr {[string match "text/*" $content_type] ? $text_translation : "binary"}]

    # 3. Resolve the corresponding Tcl encoding for a given IANA/MIME
    # charset name (or alias); the main resolution scheme is
    # implemented by [ns_encodingfortype] which is available bother
    # under AOLserver and NaviServer (see tcl/charsets.tcl). The
    # mappings between Tcl encoding names (as shown by [encoding
    # names]) and IANA/MIME charset names (i.e., names and aliases in
    # the sense of http://www.iana.org/assignments/character-sets) is
    # provided by ...
    # 
    # i. a static, built-in correspondence map: see nsd/encoding.c
    # ii. an extensible correspondence map (i.e., the ns/charsets
    # section in config.tcl).
    # 
    # [ns_encodingfortype] introduces several levels of precedence when
    # resolving the actual IANA/MIME charset and the corresponding Tcl
    # encoding to use:
    #
    # i. The "content_type" string contains a charset specification,
    # e.g.: "text/xml; charset=UTF-8". This spec fragment takes the
    # highest precedence.
    #
    # ii. The "content_type" string points to a "text/*" media
    # subtype, but does not specify a charset (e.g., "text/xml"). In
    # this case, the charset defined by ns/parameters/OutputCharset
    # (see config.tcl) applies. If this parameter is missing, the
    # general default is "iso-8859-1" (see tcl/charsets.tcl; this
    # follows from http://tools.ietf.org/html/rfc2616; Section 3.7.1).
    #
    # iii. If neither case i) or case ii) become effective, the encoding is
    # resolved to "binary".

    set enc [ns_encodingfortype $content_type]

    #
    # 4. We provide for a general fallback: For cases where
    # [ns_encodingfortype] cannot resolve a valid Tcl encoding (e.g.,
    # when an invalid, unknown or empty IANA/MIME charset is specified
    # in the content_type string), we default to "iso8859-1" for text/*
    # media subtypes and "binary" for all the other types. In addition,
    # we report the incidence.
    #

    if {$enc eq ""} {
      set enc [expr {[string match "text/*" $content_type] ? "iso8859-1" : "binary"}]
      my log "--- Resolving a Tcl encoding for the CONTENT-TYPE '$content_type' failed; falling back to '$enc'."
    }

    # Note, there are also alternatives for resolving IANA/MIME
    # charset names to Tcl encoding names, however, they all have
    # issues (non-extensibility from standard configuration sites,
    # incompleteness, redundant thread-local storing, scripted
    # implementation):
    # 1. tcllib/mime package: ::mime::reversemapencoding()
    # 2. tdom: tDOM::IANAEncoding2TclEncoding(); see lib/tdom.tcl

    fconfigure [my set S] -translation $trl -encoding $enc
  }



  HttpCore instproc init {} {
    my instvar S post_data host port protocol
    my destroy_on_cleanup

    my set meta [list]
    my set data ""
    if {![my exists method]} {
      my set method [expr {$post_data eq "" ? "GET" : "POST"}]
    }
    if {[my exists url]} {
      my parse_url
    } else {
      if {![info exists port]} {my set_default_port $protocol}
      if {![info exists host]} {
        error "either host or url must be specified"
      }
    }
    if {$protocol eq "https"} {
      package require tls
      if {[info command ::tls::import] eq ""} {
        error "https request require the Tcl module TLS to be installed\n\
             See e.g. http://tls.sourceforge.net/"
      }
      # 
      # Add HTTPs handling
      #
      my mixin add ::xo::Tls
    }
    if {[catch {my open_connection} err]} {
      my cancel "error during open connection via $protocol to $host $port: $err"
    }
  }

  HttpCore instproc send_request {} {
    my instvar S post_data host method
    if {[catch {
      puts $S "$method [my path] HTTP/1.0"
      puts $S "Host: $host"
      puts $S "User-Agent: [my user_agent]"
      foreach {tag value} [my request_header_fields] {
	#regsub -all \[\n\r\] $value {} value
	#set tag [string trim $tag]
        puts $S "$tag: $value"
      }
      my $method
    } err]} {
      my cancel "error send $host [my port]: $err"
      return
    }
  }

  HttpCore instproc GET {} {
    my instvar S
    puts $S ""
    my request_done
  }

  HttpCore instproc POST {} {
    my instvar S post_data
    if {[string match "text/*" [my content_type]]} {
      # Make sure, "string range" and "string length" return the right
      # values for UTF-8 and other encodings.
      set post_data [encoding convertto $post_data]
    }
    puts $S "Content-Length: [string length $post_data]"
    puts $S "Content-Type: [my content_type]"
    puts $S ""
    my set_encoding [my content_type]
    my send_POST_data
  }
  HttpCore instproc send_POST_data {} {
    my instvar S post_data
    puts -nonewline $S $post_data
    my request_done
  }
  HttpCore instproc request_done {} {
    my instvar S
    flush $S
    my reply_first_line
  }

  HttpCore instproc close {} {
    catch {close [my set S]} errMsg
    my debug "--- closing socket socket?[my exists S] => $errMsg"
  }

  HttpCore instproc cancel {reason} {
    my set status canceled
    my set cancel_message $reason
    my debug "--- canceled for $reason"
    my close
  }

  HttpCore instproc finish {} {
    my set status finished
    my close
    my debug "--- [my host] [my port] [my path] has finished"
  }
  HttpCore instproc getLine {var} {
    my upvar $var response
    my instvar S
    set n [gets $S response]
    if {[eof $S]} {
      my debug "--premature eof"
      return -2
    }
    if {$n == -1} {my debug "--input pending, no full line"; return -1}
    return $n
  }
  HttpCore instproc reply_first_line {} {
    my instvar S status_code
    fconfigure $S -translation crlf
    set n [my getLine response]
    switch -exact -- $n {
      -2 {my cancel premature-eof; return}
      -1 {my finish; return}
    }
    if {[regexp {^HTTP/([0-9.]+) +([0-9]+) *} $response _ \
	     responseHttpVersion status_code]} {
      my reply_first_line_done
    } else {
      my cancel "unexpected-response '$response'"
    }
  }
  HttpCore instproc reply_first_line_done {} {
    my header
  }
  HttpCore instproc header {} {
    while {1} {
      set n [my getLine response]
      switch -exact -- $n {
	-2 {my cancel premature-eof; return}
	-1 {continue}
	0 {break}
	default {
	  #my debug "--header $response"
	  if {[regexp -nocase {^content-length:(.+)$} $response _ length]} {
	    my set content_length [string trim $length]
	  } elseif {[regexp -nocase {^content-type:(.+)$} $response _ type]} {
	    my set content_type [string trim $type]
	  }
	  if {[regexp -nocase {^([^:]+): *(.+)$} $response _ key value]} {
	    my lappend meta [string tolower $key] $value
	  }
	}
      }
    }
    my reply_header_done
  }
  HttpCore instproc reply_header_done {} {
    # we have received the header, including potentially the 
    # content_type of the returned data
    my set_encoding [my content_type]
    if {[my exists content_length]} {
      my set data [read [my set S] [my set content_length]]
    } else {
      my set data [read [my set S]]
    }
    my finish
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

  Class HttpRequest -superclass HttpCore -slots {
    Attribute create timeout -type integer
  }

  HttpRequest instproc init {} {
    # my log "[my exists timeout]"
    if {[my exists timeout] && [my timeout] > 0} {
      # create a cond and mutex
      set cond  [thread::cond create]
      set mutex [thread::mutex create]
      
      thread::mutex lock $mutex
     
      # start the asynchronous request
      my debug "--a create new  ::xo::AsyncHttpRequest"
      set req [bgdelivery do -async ::xo::AsyncHttpRequest new \
		   -mixin ::xo::AsyncHttpRequest::RequestManager \
		   -url [my url] \
		   -timeout [my timeout] \
		   -post_data [encoding convertto [my post_data]] \
		   -request_header_fields [my request_header_fields] \
		   -content_type [my content_type] \
		   -user_agent [my user_agent] \
		   -condition $cond]

      while {1} {
	my set_status $cond COND_WAIT_TIMEOUT
	thread::cond wait $cond $mutex [my timeout]

	set status [my get_status $cond]
	my debug "status after cond-wait $status"

	if {$status ne "COND_WAIT_REFRESH"} break
      }
      if {$status eq "COND_WAIT_TIMEOUT"} {
	my set_status $cond "COND_WAIT_CANCELED"
      }
      set status_value [my get_value_for_status $cond]
      if {$status eq "JOB_COMPLETED"} {
	my set data $status_value
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
      my unset_status $cond
    } else {
      next    ;# HttpCore->init
      #
      # test whether open_connection yielded
      # a socket ...
      #
      # my log "after core init, S?[my exists S]"
      if {[my exists S]} {
	my send_request
      }
    }
  }
    
  #
  # Asynchronous (non-blocking) requests
  #

  Class AsyncHttpRequest -superclass HttpCore -slots {
    Attribute create timeout -type integer -default 10000 ;# 10 seconds
    Attribute create request_manager
  }
  AsyncHttpRequest instproc set_timeout {} {
    my cancel_timeout
    my debug "--- setting socket timeout: [my set timeout]"
    my set timeout_handle [after [my set timeout] [self] cancel timeout]
  }
  AsyncHttpRequest instproc cancel_timeout {} {
    if {[my exists timeout_handle]} {
      after cancel [my set timeout_handle]
    }
  }
  AsyncHttpRequest instproc send_request {} {
    # remove fileevent handler explicitly
    fileevent [my set S] writable {}
    next
  }
  AsyncHttpRequest instproc init {} {
    my notify start_request
    my set_timeout
    next
    #
    # test whether open_connection yielded
    # a socket ...
    #
    if {[my exists S]} {
      fileevent [my set S] writable [list [self] send_request]
    }
  }
  AsyncHttpRequest instproc notify {method {arg ""}} {
    if {[my exists request_manager]} {
      [my request_manager] $method $arg [self]
    }
  }
  AsyncHttpRequest instproc POST {} {
    if {[my exists S]} {fconfigure [my set S] -blocking false}
    fileevent [my set S] writable [list [self] send_POST_data]
    my set bytes_sent 0
    next
  }
  AsyncHttpRequest instproc send_POST_data {} {
    my instvar S post_data bytes_sent
    my set_timeout
    set l [string length $post_data]
    if {$bytes_sent < $l} {
      set to_send [expr {$l - $bytes_sent}]
      set block_size [expr {$to_send < 4096 ? $to_send : 4096}]
      set bytes_sent_1 [expr {$bytes_sent + $block_size}]
      set block [string range $post_data $bytes_sent $bytes_sent_1]
      my notify request_data $block
      puts -nonewline $S $block
      set bytes_sent $bytes_sent_1
    } else {
      fileevent $S writable ""
      my request_done
    }
  }
  AsyncHttpRequest instproc cancel {reason} {
    if {$reason ne "timeout"} {
      my cancel_timeout
    }
    next
    my notify failure $reason
  }
  AsyncHttpRequest instproc finish {} {
    my cancel_timeout
    next
    my debug "--- finished data [my set data]"
    my notify success [my set data]
  }
  AsyncHttpRequest instproc request_done {} {
    my notify start_reply
    my set_timeout
    my instvar S
    flush $S
    fconfigure $S -blocking false
    fileevent $S readable [list [self] reply_first_line]
  }
  AsyncHttpRequest instproc reply_first_line_done {} {
    my set_timeout
    my instvar S
    fileevent $S readable [list [self] header]      
  }
  AsyncHttpRequest instproc reply_header_done {} {
    my set_timeout
    # we have received the header, including potentially the 
    # content_type of the returned data
    my set_encoding [my content_type]
    fileevent [my set S] readable [list [self] receive_reply_data]
  }
  AsyncHttpRequest instproc receive_reply_data {} {
    my instvar S
    my debug "JOB receive_reply_data eof=[eof $S]"
    if {[eof $S]} {
      my finish
    } else {
      my set_timeout
      set block [read $S]
      my notify reply_data $block
      my append data $block
      #my debug "reveived [string length $block] bytes"
    }
  }

  #
  # SimpleListener defines a mixin class for providing a stub
  # implementaton for callbacks of the asynchrous HTTP requests. 
  # This class is typically run in the scope of bgdelivery
  #

  Class create AsyncHttpRequest::SimpleListener \
      -instproc init {} {
	my debug "INIT- NEXT=[self next]"
	# register request object as its own request_manager
	my request_manager [self]
	next

      } -instproc start_request {payload obj} {
	my debug "request $obj started"

      } -instproc request_data {payload obj} {
	my debug "partial or complete post"

      } -instproc start_reply {payload obj} {
	my debug "reply $obj started"

      } -instproc reply_data {payload obj} {
	my debug "partial or complete delivery"

      } -instproc finalize {obj status value} {
	my debug "finalize $obj $status"
	# this is called as a single method after success or failure
	next

      } -instproc success {payload obj} {
	my debug "[string length $payload] bytes payload"
	#if {[string length $payload]<600} {my log payload=$payload}
	# this is called as after a succesful request
	my finalize $obj "JOB_COMPLETED" $payload

      } -instproc failure {reason obj} {
	my log "[self proc] [self args]"
	my log "failed for '$reason'"
	# this is called as after an unsuccesful request
	my finalize $obj "JOB_FAILED" $reason

      } -instproc unknown {method args} {
	my log "[self proc] [self args]"
	my log "UNKNOWN $method"
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
	my instvar condition
	# If a job was canceled, the status variable might not exist
	# anymore, the condition might be already gone as well.  In
	# this case, we do not have to perform the cond-notify.
	if {[my exists_status $condition] && 
	    [my get_status $condition] eq "COND_WAIT_REFRESH"} {
          # Before, we had here COND_WAIT_TIMEOUT instead of 
          # COND_WAIT_REFRESH
	  my set_status $condition $status $value
	  catch {thread::cond notify $condition}
	  $obj debug "--- destroying after finish"
	  $obj destroy
	}

      } -instproc set_cond_timeout {} {
	my instvar condition
	if {[my exists_status $condition] && 
	    [my get_status $condition] eq "COND_WAIT_TIMEOUT"} {
	  my set_status $condition COND_WAIT_REFRESH
	  catch {thread::cond notify $condition}
	}
	
      } -instproc start_request {payload obj} {
	my debug "JOB start request $obj"
	my set_cond_timeout

      } -instproc request_data {payload obj} {
	my debug "JOB request data $obj [string length $payload]"
	my set_cond_timeout

      } -instproc start_reply {payload obj} {
	my debug "JOB start reply $obj"
	my set_cond_timeout

      } -instproc reply_data {payload obj} {
	my debug "JOB reply data $obj [string length $payload]"
	my set_cond_timeout

      }
  
  # 
  # TLS/SSL support
  #
  # Perform HTTPS requests via TLS (does not require nsopenssl)
  # - requires tls 1.5.0 to be compiled into <aolsever>/lib/ ...
  # - - - - - - - - - - - - - - - - - - 
  # - see http://www.ietf.org/rfc/rfc2246.txt
  # - http://wp.netscape.com/eng/ssl3/3-SPEC.HTM
  # - - - - - - - - - - - - - - - - - - 
  
  Class Tls
  Tls instproc open_connection {} {
    my instvar S
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

  Class HttpRequestTrace 
  nsv_set HttpRequestTrace count 0

  HttpRequestTrace instproc init {} {
    my instvar F post_data
    my set meta [list]
    my set requestCount [nsv_incr HttpRequestTrace count]  ;# make it an instvar to find it in the log file
    set F [open /tmp/req-[format %.4d [my set requestCount]] w]
    
    set method [expr {$post_data eq "" ? "GET" : "POST"}]
    puts $F "$method [my path] HTTP/1.0"
    puts $F "Host: [my host]"
    puts $F "User-Agent: [my user_agent]"
    foreach {tag value} [my request_header_fields] { puts $F "$tag: $value" }
    next 
  }

  HttpRequestTrace instproc POST {} {
    my instvar F post_data
    puts $F "Content-Length: [string length $post_data]"
    puts $F "Content-Type: [my content_type]"
    puts $F ""
    fconfigure $F -translation {auto binary}
    puts -nonewline $F $post_data
    next
  }

  HttpRequestTrace instproc cancel {reason} {
    catch {close [my set F]}
    next
  }
  HttpRequestTrace instproc finish {} {
    catch {close [my set F]}
    next
  }
   
  #
  # To activate trace for all requests, uncomment the following line.
  # To trace a single request, mixin ::xo::HttpRequestTrace into the request.
  #                           
  # HttpCore instmixin add ::xo::HttpRequestTrace
}
