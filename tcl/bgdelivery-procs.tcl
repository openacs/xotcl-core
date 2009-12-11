ad_library {

    Routines for background delivery of files

    @author Gustaf Neumann (neumann@wu-wien.ac.at)
    @creation-date 19 Nov 2005
    @cvs-id $Id$
}

if {[info command ::thread::mutex] eq ""} {
  ns_log notice "libthread does not appear to be available, NOT loading bgdelivery"
  return
}
#return ;# DONT COMMIT

# catch {ns_conn contentsentlength} alone does not work, since we do not have
# a connection yet, and the bgdelivery won't be activated
catch {ns_conn xxxxx} msg
if {![string match *contentsentlength* $msg]} {
  ns_log notice "AOLserver is not patched for bgdelivery, NOT loading bgdelivery"

  ad_proc -public ad_returnfile_background {-client_data status_code mime_type filename} {
    Deliver the given file to the requestor in the background. This proc uses the
    background delivery thread to send the file in an event-driven manner without
    blocking a request thread. This is especially important when large files are 
    requested over slow (e.g. dial-ip) connections.
  } {
    ns_returnfile $status_code $mime_type $filename
  }
  return
}

::xotcl::THREAD create bgdelivery {
  ###############
  # File delivery
  ###############
  set ::delivery_count 0

  Object fileSpooler
  fileSpooler set tick_interval 60000 ;# 1 min
  fileSpooler proc spool {-channel -filename -context {-client_data ""}} {
    set fd [open $filename]
    fconfigure $fd -translation binary
    fconfigure $channel -translation binary
    #ns_log notice "--- start of delivery of $filename (running:[array size ::running])"
    fcopy $fd $channel -command [list [self] end-delivery -client_data $client_data $filename $fd $channel]
    set ::running($channel,$fd,$filename) $context
    incr ::delivery_count
  }
  fileSpooler proc end-delivery {{-client_data ""} filename fd channel bytes args} {
    #ns_log notice "--- end of delivery of $filename, $bytes bytes written $args"
    if {[catch {close $channel} e]} {ns_log notice "bgdelivery, closing channel for $filename, error: $e"}
    if {[catch {close $fd} e]} {ns_log notice "bgdelivery, closing file $filename, error: $e"}
    unset ::running($channel,$fd,$filename)
  }
  
  fileSpooler proc cleanup {} {
    # This method should not be necessary. However, under unclear conditions,
    # some fcopies seem to go into a stasis. After 2000 seconds, we will kill it.
    foreach {index entry} [array get ::running] {
      foreach {key elapsed} $entry break
      set t [ns_time diff [ns_time get] $elapsed]
      if {[ns_time seconds $t] > 2000} {
        if {[regexp {^([^,]+),([^,]+),(.+)$} $index _ channel fd filename]} {
          ns_log notice "bgdelivery, fileSpooler cleanup after [ns_time seconds $t] seconds, $key"
          my end-delivery $filename $fd $channel -1
        }
      }
    }
  }
  fileSpooler proc tick {} {
    if {[catch {my cleanup} errorMsg]} {ns_log notice "Error during filespooler cleanup: $errorMsg"}
    my set to [after [my set tick_interval] [list [self] tick]]
  }
  fileSpooler tick

  # 
  # A first draft of a h264 pseudo streaming spooler.
  # Like for the fileSpooler, we create a single spooler object
  # that handles spooling for all active streams. The per-stream context
  # is passed via argument lists.
  #

  Object h264Spooler
  h264Spooler set blockCount 0
  h264Spooler proc spool {-channel -filename -context {-client_data ""} -query} {
    #ns_log notice "h264 SPOOL gets filename '$filename'"
    incr ::delivery_count
    fconfigure $channel -translation binary -blocking false
    set handle [h264open $filename $query]
    fileevent $channel writable [list [self] writeBlock $client_data $filename $handle $channel]
    set ::running($channel,$handle,$filename) $context
  }
  h264Spooler proc writeBlock {client_data filename handle channel} {
    h264Spooler incr blockCount
    #ns_log notice "h264 WRITE BLOCK $channel $handle"
    if {[eof $channel] || [h264eof $handle]} {
      my finish $client_data $filename $handle $channel
    } else {
      if {[catch {puts -nonewline $channel [h264read $handle]} errorMsg]} {
        ns_log notice "h264: error on writing to channel $channel: $errorMsg"
        my finish $client_data $filename $handle $channel
      }
    }
  }
  h264Spooler proc finish {client_data filename handle channel} {
    ns_log notice "h264 FINISH $channel $handle"
    if {[catch {close $channel} e]} {ns_log notice "bgdelivery, closing h264 for $filename, error: $e"}
    if {[catch {h264close $handle} e]} {ns_log notice "bgdelivery, closing h264 $filename, error: $e"}
    unset ::running($channel,$handle,$filename)
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
  
  Class ::HttpSpooler -parameter {channel {timeout 10000} {counter 0}}
  ::HttpSpooler instproc init {} {
    my set running 0
    my set release 0
    my set spooling 0
    my set queue [list]
  }
  ::HttpSpooler instproc all_done {} {
    catch {close [my channel]}
    my log ""
    my destroy
  }
  ::HttpSpooler instproc release {} {
    # release indicates the when running becomes 0, the spooler is finished
    my set release 1
    if {[my set running] == 0} {my all_done}
  }
  ::HttpSpooler instproc done {reason request} {
    my instvar running release
    incr running -1
    my log "--running $running"
    $request destroy
    if {$running == 0 && $release} {my all_done}
  }
  ::HttpSpooler instproc deliver {data request {encoding binary}} {
    my instvar spooling 
    my log "-- spooling $spooling"
    if {$spooling} {
      my log "--enqueue"
      my lappend queue $data $request $encoding
    } else {
      #my log "--send"
      set spooling 1
      # puts -nonewline [my channel] $data
      # my done
      set filename [ns_tmpnam]
      set fd [open $filename w]
      fconfigure $fd -translation binary -encoding $encoding
      puts -nonewline $fd $data
      close $fd
      set fd [open $filename]
      fconfigure $fd -translation binary -encoding $encoding
      fconfigure [my channel] -translation binary  -encoding $encoding
      fcopy $fd [my channel] -command \
	  [list [self] end-delivery $filename $fd [my channel] $request]
    }
  }
  ::HttpSpooler instproc end-delivery {filename fd ch request bytes args} {
    my instvar queue
    my log "--- end of delivery of $filename, $bytes bytes written $args"
    if {[catch {close $fd} e]} {ns_log notice "httpspool, closing file $filename, error: $e"}
    my set spooling 0
    if {[llength $queue]>0} {
      my log "--dequeue"
      set data [lindex $queue 0]
      set req  [lindex $queue 1]
      set enc  [lindex $queue 2]
      set queue [lreplace $queue 0 2]
      my deliver $data $req $enc
    }
    my done delivered $request
  }
  ::HttpSpooler instproc add {-request {-post_data ""}} {
    if {[regexp {http://([^/]*)(/.*)} $request _ host path]} {
      set port 80
      regexp {^([^:]+):(.*)$} $host _ host port
      my incr running
      xo::AsyncHttpRequest [self]::[my incr counter] \
	  -host $host -port $port -path $path \
	  -timeout [my timeout] -post_data $post_data -request_manager [self]
    }
  }
} -persistent 1 ;# -lightweight 1

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

bgdelivery ad_proc returnfile {{-client_data ""} status_code mime_type filename} {
  Deliver the given file to the requestor in the background. This proc uses the
  background delivery thread to send the file in an event-driven manner without
  blocking a request thread. This is especially important when large files are 
  requested over slow (e.g. dial-ip) connections.
} {
  #ns_log notice "status_code = $status_code, filename=$filename"
  set size [file size $filename]
  #ns_setexpires 1000000
  #ns_log notice "expires-set $filename"

  #
  # Make sure to set "connection close" for the reqests (in other
  # words, don't allow keep-alive, which is does not make sense, when
  # we close the connections manually in the bgdeliverfy thread).
  #
  if {[ns_info name] eq "NaviServer"} {
    ns_conn keepalive 0
  }

  if {[my write_headers $status_code $mime_type $size]} {

    if {$size == 0} {
      # Tcl behaves different, when one tries to send 0 bytes via
      # file_copy. So, we handle this special case here...
      # There is actualy nothing to deliver....
      return
    }

    set errorMsg ""
    # Get the thread id and make sure the bgdelivery thread is already
    # running.
    set tid [my get_tid]

    # my log "+++ lock [my set bgmutex]"
    ::thread::mutex lock [my set mutex]
    #ns_mutex lock [my set bgmutex]

    catch {
      set ch [ns_conn channel]
      if {[catch {thread::transfer $tid $ch} innerError]} {
        set channels_in_use "??"
        catch {set channels_in_use [bgdelivery do file channels]}
        ns_log error "thread transfer failed, channel=$ch, channels_in_use=$channels_in_use"
        error $innerError
      }
    } errorMsg

    ::thread::mutex unlock [my set mutex]
    #ns_mutex unlock [my set bgmutex]
    # my log "+++ unlock [my set bgmutex]"

    if {$errorMsg ne ""} {
      error ERROR=$errorMsg
    }
    if {![my isobject ::xo:cc]} {
      ::xo::ConnectionContext require
    }
    #my log [::xo::cc serialize]

    set query [::xo::cc actual_query]
    set contentType [ns_set iget [ns_conn outputheaders] content-type]
    if {[string match video/mp4* $contentType] && $query ne "" 
        && [info command h264open] ne ""
      } {
      #my log "MP4 q=[::xo::cc actual_query], h=[ns_set array [ns_conn outputheaders]]"
      my do -async ::h264Spooler spool -channel $ch -filename $filename \
          -context [list [::xo::cc requestor],[::xo::cc url] [ns_conn start]] \
          -query $query \
          -client_data $client_data
    } else {
      #my log "FILE SPOOL $filename"
      my do -async ::fileSpooler spool -channel $ch -filename $filename \
          -context [list [::xo::cc requestor],[::xo::cc url] [ns_conn start]] \
          -client_data $client_data
    }
    ns_conn contentsentlength $size       ;# maybe overly optimistic
  }
}

ad_proc -public ad_returnfile_background {{-client_data ""} status_code mime_type filename} {
  Deliver the given file to the requestor in the background. This proc uses the
  background delivery thread to send the file in an event-driven manner without
  blocking a request thread. This is especially important when large files are 
  requested over slow (e.g. dial-ip) connections.
} {
  #my log "driver=[ns_conn driver]"
  if {[ns_conn driver] ne "nssock"} {
    ns_returnfile $status_code $mime_type $filename
  } else {
    bgdelivery returnfile -client_data $client_data $status_code $mime_type $filename
  }
}

#####################################
bgdelivery proc subscribe {key {initmsg ""} {mode default} } {
  set content_type [expr {$mode eq "scripted" ? "text/html" : "text/plain"}]
  ns_write "HTTP/1.0 200 OK\r\nContent-type: $content_type\r\n\r\n[string repeat { } 1024]"
  set ch [ns_conn channel]
  thread::transfer [my get_tid] $ch
  my do ::Subscriber new -channel $ch -key $key -user_id [ad_conn user_id] -mode $mode
}

bgdelivery proc send_to_subscriber {key msg} {
  my do -async ::Subscriber broadcast $key $msg
}
#####################################
bgdelivery proc create_spooler {{-content_type text/plain} {-timeout 10000}} {
  ns_write "HTTP/1.0 200 OK\r\nContent-type: $content_type\r\n\r\n"
  set ch [ns_conn channel]
  thread::transfer [my get_tid] $ch
  my do ::HttpSpooler new -channel $ch -timeout $timeout
}

bgdelivery proc spooler_add_request {spooler request {post_data ""}} {
  my log "-- do -async $spooler add -request $request"
  my do -async $spooler add -request $request -post_data $post_data
}
bgdelivery proc spooler_release {spooler} {
  my do -async $spooler release
}
