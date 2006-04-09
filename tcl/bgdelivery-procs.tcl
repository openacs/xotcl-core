ad_library {

    Routines for background delivery of files

    @author Gustaf Neumann (neumann@wu-wien.ac.at)
    @creation-date 19 Nov 2005
    @cvs-id $Id$
}

::xotcl::THREAD create bgdelivery {
  ###############
  # File delivery
  ###############
  set ::delivery_count 0

  proc deliver {ch filename context} {
    set fd [open $filename]
    fconfigure $fd -translation binary
    fconfigure $ch -translation binary
    #ns_log notice "--- start of delivery of $filename (running:[array size ::running])"
    fcopy $fd $ch -command [list end-delivery $filename $fd $ch]
    set ::running($ch,$filename) $context
    incr ::delivery_count
  }

  proc end-delivery {filename fd ch bytes args} {
    #ns_log notice "--- end of delivery of $filename, $bytes bytes written $args"
    if {[catch {close $ch} e]} {ns_log notice "bgdelivery, closing channel for $filename, error: $e"}
    if {[catch {close $fd} e]} {ns_log notice "bgdelivery, closing file $filename, error: $e"}
    unset ::running($ch,$filename)
  }

  ###############
  # Subscriptions
  ###############
  set ::subscription_count 0
  set ::message_count 0

  ::xotcl::Class Subscriber -parameter {key channel user_id mode}
  Subscriber proc current {-key } {
    my instvar subscriptions
    set result [list]
    if {[info exists key]} {
      if {[info exists subscriptions($key)]} {
	return [list $key $subscriptions($key)]
      }
    } elseif {[info exists subscriptions]} {
      foreach key [array names subscriptions] {
	lappend result $key $subscriptions($key)
      }
    }
  }

  Subscriber proc broadcast {key msg} {
    my instvar subscriptions
    if {[info exists subscriptions($key)]} {
      set subs1 [list]
      foreach s $subscriptions($key) {
	if {[catch {
	  if {[$s mode] eq "scripted"} {
	    set smsg "<script type='text/javascript' language='javascript'>\nvar data = $msg;\n\
            parent.getData(data);</script>\n"
	  } else {
	    set smsg $msg
	  }
	  my log "-- sending to subscriber for $key $smsg ch=[$s channel] \
		mode=[$s mode], user_id [$s user_id]"
	  puts -nonewline [$s channel] $smsg
	  flush [$s channel]
	} errmsg]} {
	  ns_log notice "error in send to subscriber (key=$key): $errmsg"
	  catch {close [$s channel]}
	  $s destroy
	} else {
	  lappend subs1 $s
	}
      }
      set subscriptions($key) $subs1
    }
    incr ::message_count
  }
  Subscriber instproc init {} {
    [my info class] instvar subscriptions
    lappend subscriptions([my key]) [self]
    #my log "-- cl=[my info class], subscriptions([my key]) = $subscriptions([my key])"
    fconfigure [my channel] -translation binary
    incr ::subscription_count
  }
} -persistent 1

bgdelivery ad_forward running {
  Interface to the background delivery thread to query the currently running deliveries.
  @return list of key value pairs of all currently running background processes
} %self do array get running


bgdelivery ad_forward nr_running {
  Interface to the background delivery thread to query the number of currently running deliveries.
  @return number of currently running background deliveries
} %self do array size running

if {[ns_info name] eq "NaviServer"} {
  bgdelivery forward write_headers ns_headers
} else {
  bgdelivery forward write_headers ns_headers DUMMY
}


bgdelivery ad_proc returnfile {statuscode mime_type filename} {
  Deliver the given file to the requestor in the background. This proc uses the
  background delivery thread to send the file in an event-driven manner without
  blocking a request thread. This is especially important when large files are 
  requested over slow (e.g. dial-ip) connections.
} {
  #ns_log notice "statuscode = $statuscode, filename=$filename"
  set size [file size $filename]
  if {[my write_headers $statuscode $mime_type $size]} {
    set ch [ns_conn channel]
    thread::transfer [my get_tid] $ch
    throttle get_context
    my do -async deliver $ch $filename \
	[list [throttle set requestor],[throttle set url] [ns_conn start]]
    ns_conn contentsentlength $size       ;# maybe overly optimistic
  }
}

ad_proc -public ad_returnfile_background {statuscode mime_type filename} {
  Deliver the given file to the requestor in the background. This proc uses the
  background delivery thread to send the file in an event-driven manner without
  blocking a request thread. This is especially important when large files are 
  requested over slow (e.g. dial-ip) connections.
} {
  bgdelivery returnfile $statuscode $mime_type $filename
}

#####################################
bgdelivery proc subscribe {key {initmsg ""} {mode default} } {
  set content_type [expr {$mode eq "scripted" ? "text/html" : "text/plain"}]
  ns_write "HTTP/1.0 200 OK\r\nContent-type: $content_type\r\n\r\n[string repeat { } 1024]"
  set ch [ns_conn channel]
  thread::transfer [my get_tid] $ch
  my do -async ::Subscriber new -channel $ch -key $key -user_id [ad_conn user_id] -mode $mode
  my send_to_subscriber $key $initmsg
}

bgdelivery proc send_to_subscriber {key msg} {
  my do -async ::Subscriber broadcast $key $msg
}