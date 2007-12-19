ad_library {
  XOTcl implementation for synchronous and asynchronous HTTP and HTTPs requests

  @author Gustaf Neumann, Stefan Sobernig
  @creation-date 2007-10-05
  @cvs-id $Id$
}

namespace eval ::xo {
  #
  # Defined classes
  #  1) HttpRequest
  #  2) AsyncHttpRequest
  #  3) HttpRequestTrace (mixin class)
  #  4) Tls (mixin class, applicable to various protocols)
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
  # object in $r is automatically deleted at cleanup of
  # a connection thread.
  #
  # Example of a POST request with a form with var1 and var2
  # (providing post_data causes the POST request).
  #    
  #  set r [::xo::HttpRequest new \
  #             -url http://yourhost.yourdomain/yourpath \
  #             -post_data [export_vars {var1 var2}] \
  #             -content_type application/x-www-form-urlencoded \ 
  #         ]
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
  # AsyncHttpRequest is a subclass for HttpRequest implementing
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
  # The following example uses the background delivery thread for
  # spooling and defines in this thread a listener object (a). 
  # Then in the second step, the listener object is used in te
  # asynchronous request (b).
  #
  # (a) Create a listener/callback object in the background. Provide
  # the two needed methods, one being invoked upon success (deliver),
  # the other upon failure or cancellation (done).
  #
  #  ::bgdelivery do Object ::listener \
  #     -proc deliver {payload obj} {
  #       my log "Asynchronous request suceeded!"
  #     } -proc done {reason obj} {
  #       my log "Asynchronous request failed: $reason"
  #     }
  #
  # (b)  Create the actual asynchronous request object in the background. 
  # Make sure that you specify the previously created listener/callback 
  # object as "request_manager" to the request object.
  #
  #  ::bgdelivery do ::xo::AsyncHttpRequest new \
  #     -url "https://oacs-dotlrn-conf2007.wu-wien.ac.at/conf2007/" \
  #     -request_manager ::listener
  #
  ######################
  #
  # 3 HttpRequestTrace
  #
  # HttpRequestTrace can be used to trace the one or all requests.
  # If activated, the class writes protocol data into 
  # /tmp/req-<somenumber>.
  #
  # Use 
  #
  #  ::xo::HttpRequest instmixin add ::xo::HttpRequestTrace
  #
  # to activate trace for all requests, 
  # or mixin the class into a single request to trace it.
  #

  Class create HttpRequest \
      -parameter {
	{host}
	{protocol http} 
	{port} 
	{path /}
        {url}
	{post_data ""} 
	{content_type text/plain}
	{request_manager}
	{request_header_fields {}}
        {user_agent xohttp/0.1}
      }

  HttpRequest instproc set_default_port {protocol} {
    switch $protocol {
      http  {my set port 80}
      https {my set port 443}
    }
  }

  HttpRequest instproc parse_url {} {
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

  HttpRequest instproc open_connection {} {
    my instvar host port S
    set S [socket $host $port]
  }

  HttpRequest instproc set_encoding {{-text_translation auto} content_type} {
    #
    # for text, use translation with optional encodings, else set translation binary
    #
    if {[string match text/* $content_type]} {
      if {[regexp {*charset=([^ ]+)$} $content_type _ encoding]} {
	fconfigure [my set S] -encoding $encoding -translation $text_translation
      } else {
	fconfigure [my set S] -translation $text_translation
      }
    } else {
      fconfigure [my set S] -translation binary
    }
  }

  HttpRequest instproc init {} {
    my instvar S post_data host port protocol
    my destroy_on_cleanup
    my set meta [list]
    my set data ""
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
      return
    }
    if {[catch {
      set method [expr {$post_data eq "" ? "GET" : "POST"}]
      puts $S "$method [my path] HTTP/1.0"
      puts $S "Accept: */*"
      puts $S "Host: $host"
      puts $S "User-Agent: [my user_agent]"
      foreach {tag value} [my request_header_fields] {
	#regsub -all \[\n\r\] $value {} value
	#set tag [string trim $tag]
        puts $S "$tag: $value"
      }
      my $method
    } err]} {
      my cancel "error send $host $port: $err"
      return
    }
  }

  HttpRequest instproc GET {} {
    my instvar S
    puts $S ""
    my query_done
  }

  HttpRequest instproc POST {} {
    my instvar S post_data
    puts $S "Content-Length: [string length $post_data]"
    puts $S "Content-Type: [my content_type]"
    puts $S ""
    #fconfigure $S -translation {auto binary}
    my set_encoding [my content_type]
    puts -nonewline $S $post_data
    my query_done
  }
  HttpRequest instproc query_done {} {
    my instvar S
    flush $S
    my received_first_line
  }
  HttpRequest instproc notify {method arg} {
    if {[my exists request_manager]} {
      [my request_manager] $method $arg [self]
    }
  }
  HttpRequest instproc cancel {reason} {
    my log "--- $reason"
    catch {close [my set S]}
    my notify done $reason
  }

  HttpRequest instproc finish {} {
    catch {close [my set S]}
    my log "--- [my host] [my port] [my path] has finished"
    my notify deliver [my set data]
  }
  HttpRequest instproc getLine {var} {
    my upvar $var response
    my instvar S
    set n [gets $S response]
    if {[eof $S]} {
      my log "--premature eof"
      return -2
    }
    if {$n == -1} {my log "--input pending, no full line"; return -1}
    #my log "got $response"
    return $n
  }
  HttpRequest instproc received_first_line {} {
    my instvar S status_code
    fconfigure $S -translation crlf
    set n [my getLine response]
    switch -exact -- $n {
      -2 {my cancel premature-eof; return}
      -1 {return}
    }
    if {[regexp {^HTTP/([0-9.]+) +([0-9]+) *} $response _ \
	     responseHttpVersion status_code]} {
      my received_first_line_done
    } else {
      my log "--unexpected response '$response'"
      my cancel unexpected-response
    }
  }
  HttpRequest instproc received_first_line_done {} {
    my header
  }
  HttpRequest instproc header {} {
    while {1} {
      set n [my getLine response]
      switch -exact -- $n {
	-2 {my cancel premature-eof; return}
	-1 {continue}
	0 {break}
	default {
	  #my log "--header $response"
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
    my received_header_done
  }
  HttpRequest instproc received_header_done {} {
    # we have received the header, including potentially the content_type of the returned data
    my set_encoding [my content_type]
    if {[my exists content_length]} {
      my set data [read [my set S] [my set content_length]]
    } else {
      my set data [read [my set S]]
    }
  }

  #
  # Asynchronous requests
  #

  Class AsyncHttpRequest -superclass HttpRequest -parameter {
    {timeout 10000} 
  }
  AsyncHttpRequest instproc init {} {
    my set to_identifier [after [my set timeout] [self] cancel timeout]
    next
  }
  AsyncHttpRequest instproc POST {} {
    if {[my exists S]} {fconfigure [my set S] -blocking false}
    next
  }
  AsyncHttpRequest instproc cancel {reason} {
    if {$reason ne "timeout"} {
      after cancel [my set to_identifier]
    }
    next
  }
  AsyncHttpRequest instproc finish {} {
    after cancel [my set to_identifier]
    next
  }
  AsyncHttpRequest instproc query_done {} {
    my instvar S
    flush $S
    fconfigure $S -blocking false
    fileevent $S readable [list [self] received_first_line]
  }
  AsyncHttpRequest instproc received_first_line_done {} {
    fileevent [my set S] readable [list [self] header]
  }
  AsyncHttpRequest instproc received_header_done {} {
    # we have received the header, including potentially the content_type of the returned data
    my set_encoding [my content_type]
    fileevent [my set S] readable [list [self] received_data]
  }
  AsyncHttpRequest instproc received_data {} {
    my instvar S
    if {[eof $S]} {
      my finish
    } else {
      set block [read $S]
      my append data $block
      #my log "reveived [string length $block] bytes"
    }
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
    puts $F "Accept: */*"
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
  # HttpRequest instmixin add ::xo::HttpRequestTrace
}
