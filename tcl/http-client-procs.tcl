namespace eval ::xo {
  #
  #
  #
  Class create HttpRequest \
      -parameter {
	{host}
	{port 80} 
	{path /}
        {url}
	{post_data ""} 
	{content_type text/plain}
	{request_manager}
	{request_header_fields {}}
        {user_agent xohttp/0.1}
      }

  HttpRequest instproc url {url} {
    my instvar host url path
    if {[regexp {http://([^/]*)(/.*)} $url _ host path]} {
      set port 80
      regexp {^([^:]+):(.*)$} $host _ host port
    } else {
      error "unsupported or invalid url '[my url]'"
    }
  }

  HttpRequest instproc init {} {
    my instvar S post_data host port
    my set meta [list]
    my set data ""
    
    if {[catch {set S [socket $host $port]} err]} {
      my cancel "error socket $host $port: $err"
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
    fconfigure $S -translation {auto binary}
    if {[regexp {; *charset=([^ ]+)$} [my content_type] _ encoding]} {
      fconfigure $S -encoding $encoding
    }
    #my log "--post data blocking=[fconfigure $S -blocking]"
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
    #my log "--- [my host] [my port] [my path] has finished"
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
    my instvar S statusCode
    fconfigure $S -translation crlf
    set n [my getLine response]
    switch -exact -- $n {
      -2 {my cancel premature-eof; return}
      -1 {return}
    }
    if {[regexp {^HTTP/([0-9.]+) +([0-9]+) *} $response _ \
	     responseHttpVersion statusCode]} {
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
    fconfigure [my set S] -translation binary
    if {[my exists content_length]} {
      my set data [read [my set S] [my set content_length]]
    } else {
      my set data [read [my set S]]
    }
  }


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
    fconfigure [my set S] -translation binary
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

  Class HttpRequestTrace 
  nsv_set HttpRequestTrace count 0

  HttpRequestTrace instproc init {} {

    #TODO remove me
#     my instvar host path
#     if {[my exists endpoint]} {
#       # soap specificities
#       my url [my set endpoint]
#       my post_data [my payload]
#       my content_type "text/xml"
#       if {[my action] eq ""} {
#         my headers [list SOAPAction [my set endpoint]]
#       } else {
#         my headers [list SOAPAction [my action]]
#       }
#     }

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

  #HttpRequest instmixin add HttpRequestTrace
}
