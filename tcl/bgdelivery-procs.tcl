ad_library {

  Routines for background delivery of files

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @creation-date 19 Nov 2005
  @cvs-id $Id$
}

if {[info commands ::thread::mutex] eq ""} {
  ns_log notice "libthread does not appear to be available, NOT loading bgdelivery"
  return
}
#return ;# DONT COMMIT

# catch {ns_conn contentsentlength} alone does not work, since we do not have
# a connection yet, and the bgdelivery won't be activated
catch {ns_conn xxxxx} msg
if {![string match "*contentsentlength*" $msg]} {
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
  # FileSpooler 
  ###############  
  # Class FileSpooler makes it easier to overload the 
  # per-object methods of the concrete file spoolers
  # (such has fileSpooler or h264Spooler)

  Class create FileSpooler

  ###############
  # File delivery
  ###############
  set ::delivery_count 0

  FileSpooler create fileSpooler
  fileSpooler set tick_interval 60000 ;# 1 min
  fileSpooler proc deliver_ranges {ranges client_data filename fd channel} {
    set first_range [lindex $ranges 0]
    set remaining_ranges [lrange $ranges 1 end]
    lassign $first_range from to size
    if {$remaining_ranges eq ""} {
      # A single delivery, which is as well the last; when finished
      # with this chunk, terminate delivery
      set cmd [list [self] end-delivery -client_data $client_data $filename $fd $channel]
    } else {
      #
      # For handling multiple ranges, HTTP/1.1 requires multipart
      # messages (multipart media type: multipart/byteranges);
      # currenty these are not implemented (missing test cases). The
      # code handling the range tag switches currently to full
      # delivery, when multiple ranges are requested.
      #
      set cmd [list [self] deliver_ranges $remaining_ranges $client_data $filename $fd $channel]
    }
    seek $fd $from
    #ns_log notice "Range seek $from $filename // $first_range"
    fcopy $fd $channel -size $size -command $cmd
  }
  fileSpooler proc spool {{-ranges ""} {-delete false} -channel -filename -context {-client_data ""}} {
    set fd [open $filename]
    fconfigure $fd -translation binary
    fconfigure $channel -translation binary

    set k ::runningBgJob([lindex $context 0])
    if {[info exists $k]} {
      set value [set $k]
      ns_log notice "resubmit: canceling currently running request $context  // closing $value"
      lassign $value fd0 channel0 client_data0 filename0
      my end-delivery -client_data $client_data0 $filename0 $fd0 $channel0 -1 
    }
    set $k [list $fd $channel $client_data $filename]

    if {$ranges eq ""} {
      ns_log notice "no Range spool for $filename"
      fcopy $fd $channel -command [list [self] end-delivery -client_data $client_data $filename $fd $channel]
    } else {
      my deliver_ranges $ranges $client_data $filename $fd $channel
    }
    #ns_log notice "--- start of delivery of $filename (running:[array size ::running])"
    set key $channel,$fd,$filename
    set ::running($key) $context
    if {$delete} {set ::delete_file($key) 1}
    incr ::delivery_count
  }
  fileSpooler proc end-delivery {{-client_data ""} filename fd channel bytes args} {
    #ns_log notice "--- end of delivery of $filename, $bytes bytes written $args"
    if {[catch {fconfigure $channel -blocking false} e]} {ns_log notice "bgdelivery, fconfigure for channel, error: $e"}
    if {[catch {close $channel} e]} {ns_log notice "bgdelivery, closing channel for $filename, error: $e"}
    if {[catch {close $fd} e]} {ns_log notice "bgdelivery, closing file $filename, error: $e"}
    set key $channel,$fd,$filename
    unset -nocomplain ::runningBgJob([lindex $::running($key) 0])
    unset ::running($key)
    if {[info exists ::delete_file($key)]} {
      file delete $filename
      unset ::delete_file($key)
    }
  }
  
  fileSpooler proc cleanup {} {
    # This method should not be necessary. However, under unclear conditions,
    # some fcopies seem to go into a stasis. After 2000 seconds, we will kill it.
    foreach {index entry} [array get ::running] {
      lassign $entry key elapsed
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


  ###############
  # h264Spooler
  ###############
  # 
  # A first draft of a h264 pseudo streaming spooler.
  # Like for the fileSpooler, we create a single spooler object
  # that handles spooling for all active streams. The per-stream context
  # is passed via argument lists.
  #

  FileSpooler create h264Spooler
  h264Spooler set blockCount 0
  h264Spooler set byteCount 0
  h264Spooler proc spool {{-delete false} -channel -filename -context {-client_data ""} -query} {
    #ns_log notice "h264 SPOOL gets filename '$filename'"
    if {[catch {
      set handle [h264open $filename $query]
    } errorMsg]} {
      ns_log error "h264: error opening h264 channel for $filename $query: $errorMsg"
      if {[catch {close $channel} e]} {ns_log notice "bgdelivery, closing h264 for $filename, error: $e"}
      return
    }
    # set up book-keeping info
    incr ::delivery_count
    set key $channel,$handle,$filename
    set ::bytes($key) 0
    set ::running($key) $context
    if {$delete} {set ::delete_file($key) 1}
    #
    # h264open is quite expensive; in order to output the HTTP headers
    # in the connection thread, we would have to use h264open in the
    # connection thread as well to determine the proper size. To avoid
    # this overhead, we don't write the headers in the connection
    # thread and write it here instead (note that this is different to
    # the fileSpooler above).
    #
    if {[catch {
      set length [h264length $handle] 
      puts $channel "HTTP/1.0 200 OK\nContent-Type: video/mp4\nContent-Length: $length\n"
      flush $channel
    } errorMsg]} {
      ns_log notice "h264: error writing headers in h264 channel for $filename $query: $errorMsg"
      my end-delivery -client_data $client_data $filename $handle $channel 0
    }
    # setup async delivery
    fconfigure $channel -translation binary -blocking false
    fileevent $channel writable [list [self] writeBlock $client_data $filename $handle $channel]
  }
  h264Spooler proc writeBlock {client_data filename handle channel} {
    h264Spooler incr blockCount
    set bytesVar ::bytes($channel,$handle,$filename)
    #ns_log notice "h264 WRITE BLOCK $channel $handle"
    if {[eof $channel] || [h264eof $handle]} {
      my end-delivery -client_data $client_data $filename $handle $channel [set $bytesVar]
    } else {
      set block [h264read $handle]
      # one should not use "bytelength" on binary data: http://wiki.tcl.tk/8455
      set len [string length $block]
      incr $bytesVar $len
      h264Spooler incr byteCount $len
      if {[catch {puts -nonewline $channel $block} errorMsg]} {
        ns_log notice "h264: error on writing to channel $channel: $errorMsg"
        my end-delivery -client_data $client_data $filename $handle $channel [set $bytesVar]
      }
    }
  }
  h264Spooler proc end-delivery {{-client_data ""} filename handle channel bytes args} {
    ns_log notice "h264 FINISH $channel $handle"
    if {[catch {close $channel} e]} {ns_log notice "bgdelivery, closing h264 for $filename, error: $e"}
    if {[catch {h264close $handle} e]} {ns_log notice "bgdelivery, closing h264 $filename, error: $e"}
    set key $channel,$handle,$filename
    unset ::running($key)
    unset ::bytes($key)
    if {[info exists ::delete_file($key)]} {
      file delete $filename
      unset ::delete_file($key)
    }
  }

  #################
  # AsyncDiskWriter
  #################
  ::xotcl::Class create ::AsyncDiskWriter -parameter {
    {blocksize 4096} 
    {autoflush false}
    {verbose false}
  }
  ::AsyncDiskWriter instproc log {msg} {
    if {[my verbose]} {ns_log notice "[self] --- $msg"}
  }
  ::AsyncDiskWriter instproc open {-filename {-mode w}} {
    my set channel [open $filename $mode]
    my set content ""
    my set filename $filename
    fconfigure [my set channel] -translation binary -blocking false
    my log "open [my set filename]"
  }

  ::AsyncDiskWriter instproc close {{-sync false}} {
    my instvar content channel
    if {$sync || $content eq ""} {
      my log "close sync"
      if {$content ne ""} {
        fconfigure $channel -translation binary -blocking true
        puts -nonewline $channel $content
      }
      close $channel
      my destroy
    } else {
      my log "close async"
      my set finishWhenDone 1
    }
  }
  ::AsyncDiskWriter instproc async_write {block} {
    my append content $block
    fileevent [my set channel] writable [list [self] writeBlock]
  }
  ::AsyncDiskWriter instproc writeBlock {} {
    my instvar content blocksize channel
    if {[string length $content] < $blocksize} {
      puts -nonewline $channel $content
      my log "write [string length $content] bytes"
      fileevent [my set channel] writable ""
      set content ""
      if {[my autoflush]} {flush $channel}
      if {[my exists finishWhenDone]} {
        my close -sync true
      }
    } else {
      set chunk [string range $content 0 $blocksize-1]
      set content [string range $content $blocksize end]
      puts -nonewline $channel $chunk
      my log "write [string length $chunk] bytes ([string length $content] buffered)"
    }
  }
  

  ###############
  # Subscriptions
  ###############
  set ::subscription_count 0
  set ::message_count 0

  ::xotcl::Class create Subscriber -parameter {key channel user_id mode}
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

  Subscriber instproc close {} {
    set channel [my channel]
    #
    # It is important to make the channel non-blocking for the close,
    # since otherwise the close operation might block and bring all of
    # bgdelivery to a halt.
    #
    catch {fconfigure $channel -blocking false}
    catch {close $channel}
  }

  Subscriber instproc sweep {args} {
    #
    # when the sweeper raises an error the caller (foreachSubscriber)
    # destroys the instance. In this step the peer connection is close
    # as well.
    #
    set channel [my channel]
    if {[catch {set eof [eof $channel]}]} {set eof 1}
    my log "sweep [my channel] EOF $eof"
    if {$eof} {
      error "connection $channel closed by peer"
    }
    # make an io-attempt to trigger EOF
    if {[catch {
      set blocking [fconfigure $channel -blocking]
      fconfigure $channel -blocking false
      set x [read $channel]
      fconfigure $channel -blocking $blocking
    } errorMsg]} {
      error "connection $channel closed due to IO error"
    }
  }

  Subscriber instproc send {msg} {
    my log ""
    if {[my mode] eq "scripted"} {
      set smsg "<script type='text/javascript'>\nvar data = $msg;\n\
            parent.getData(data);</script>\n"
      set smsg [format %x [string length $smsg]]\r\n$smsg\r\n
    } else {
      set smsg $msg
    }
    #my log "-- sending to subscriber for [my key] $smsg ch=[my channel] \
        #        mode=[my mode], user_id [my user_id]"
    puts -nonewline [my channel] $smsg
    flush [my channel]
  }

  Subscriber proc foreachSubscriber {key method {argument ""}} {
    my msg "$key $method '$argument'"
    my instvar subscriptions
    if {[info exists subscriptions($key)]} {
      set subs1 [list]
      foreach s $subscriptions($key) {
        if {[catch {$s $method $argument} errMsg]} {
          ns_log notice "error in $method to subscriber $s (key=$key): $errMsg"
          $s destroy
        } else {
          lappend subs1 $s
        }
      }
      set subscriptions($key) $subs1
    }
  }

  Subscriber proc broadcast {key msg} {
    my foreachSubscriber $key send $msg
    incr ::message_count
  }

  Subscriber proc sweep {key} {
    my foreachSubscriber $key sweep 
  }

  Subscriber instproc destroy {} {
    my close
    next
  }

  Subscriber instproc init {} {
    [my info class] instvar subscriptions
    lappend subscriptions([my key]) [self]
    incr ::subscription_count
    #my log "-- cl=[my info class], subscriptions([my key]) = $subscriptions([my key])"
    fconfigure [my channel] -translation binary

    if {[my mode] eq "scripted"} {
      set content_type text/html
      set encoding "Cache-Control: no-cache\r\nTransfer-Encoding: chunked\r\n"
      set body "<html><body>[string repeat { } 1024]\r\n"
      set body [format %x [string length $body]]\r\n$body\r\n
    } else {
      #set content_type text/plain 
      # Chrome refuses to expose partial response to ajax unless we
      # set content_type to octet stream.  Drawback is we now need to
      # treat special characters on the client side.
      set content_type "application/octet-stream"
      set encoding ""
      set body ""
    }

    puts -nonewline [my channel] "HTTP/1.1 200 OK\r\nContent-type: $content_type\r\n$encoding\r\n$body"
    flush [my channel]
  }
  

  ###############
  # HttpSpooler
  ###############

  Class create ::HttpSpooler -parameter {channel {timeout 10000} {counter 0}}
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
      set filename [ad_tmpnam]
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
      lassign $queue data req enc
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

  #
  # Add an exit handler to close all AsyncDiskWriter, when this thread goes
  # down.
  #
  ::xotcl::Object setExitHandler {
    ns_log notice "--- exit handler"
    foreach writer [::AsyncDiskWriter info instances -closure] {
      ns_log notice "close AsyncDiskWriter $writer"
      $writer close
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

if {$::xo::naviserver} {
  bgdelivery forward write_headers ns_headers
} else {
  bgdelivery forward write_headers ns_headers DUMMY
}

bgdelivery ad_proc returnfile {
  {-client_data ""} 
  {-delete false} 
  {-content_disposition} 
  status_code mime_type filename} {
    Deliver the given file to the requestor in the background. This proc uses the
    background delivery thread to send the file in an event-driven manner without
    blocking a request thread. This is especially important when large files are 
    requested over slow (e.g. dial-ip) connections.
  } {

    #ns_setexpires 1000000
    #ns_log notice "expires-set $filename"
    #ns_log notice "status_code = $status_code, filename=$filename"

    if {![my isobject ::xo::cc]} {
      ::xo::ConnectionContext require
    }
    set query [::xo::cc actual_query]
    set use_h264 [expr {[string match "video/mp4*" $mime_type] && $query ne "" 
                        && ([string match {*start=[1-9]*} $query] || [string match {*end=[1-9]*} $query])
                        && [info commands h264open] ne ""}]

    if {[info commands ns_driversection] ne ""} {
      set use_writerThread [ns_config [ns_driversection] writerthreads 0]
    } else {
      set use_writerThread 0
    }

    if {[info exists content_disposition]} {
      set fn [xo::backslash_escape \" $content_disposition]
      ns_set put [ns_conn outputheaders] Content-Disposition "attachment;filename=\"$fn\""
    }

    if {$use_h264} {
      if {0} {
        # we have to obtain the size from the file; unfortunately, this
        # requires a duplicate open+close of the h264 stream. If the
        # application is performance sensitive, one might consider to use
        # the possibly incorrect size form the file system instead (works
        # perfectly for e.g. flowplayer)
        if {[catch {set handle [h264open $filename $query]} errorMsg]} {
          ns_log error "h264: error opening h264 channel for $filename $query: $errorMsg"
          return
        }
        set size [h264length $handle]
        h264close $handle
      } else {
        set size [file size $filename]
      }
    } else {
      set size [file size $filename]
    }

    # Make sure to set "connection close" for the reqests (in other
    # words, don't allow keep-alive, which is does not make sense, when
    # we close the connections manually in the bgdelivery thread).
    #
    if {$::xo::naviserver && !$use_writerThread} {
      ns_conn keepalive 0
    }

    set range [ns_set iget [ns_conn headers] range]
    if {[regexp {bytes=(.*)$} $range _ range]} {
      set ranges [list]
      set bytes 0
      set pos 0
      foreach r [split $range ,] {
        regexp {^(\d*)-(\d*)$} $r _ from to
        if {$from eq ""} {
          # The last $to bytes, $to must be specified; 'to' is
          # differently interpreted as in the case, where from is
          # non-empty
          set from [expr {$size - $to}]
        } else {
          if {$to eq ""} {set to [expr {$size-1}]}
        }
        set rangeSize [expr {1 + $to - $from}]
        lappend ranges [list $from $to $rangeSize]
        set pos [expr {$to + 1}]
        incr bytes $rangeSize
      }
    } else {
      set ranges ""
      set bytes $size
    }

    #ns_log notice "Range=$range bytes=$bytes // $ranges"

    #
    # For the time being, we write the headers in a simplified version
    # directly in the spooling thread to avoid the overhead of double
    # h264opens.
    #
    if {!$use_h264} {
      #
      # Add content-range header for range requests.
      #
      if {[llength $ranges] == 1 && $status_code == 200} {
        lassign [lindex $ranges 0] from to
        if {$from <= $to && $size > $to} {
          ns_set put [ns_conn outputheaders] Content-Range "bytes $from-$to/$size"
          ns_log notice "given range <$range>, added header-field Content-Range: bytes $from-$to/$size // $ranges"
          set status_code 206
        } else {
          # A byte-content-range-spec with a byte-range-resp-spec whose
          # last-byte-pos value is less than its first-byte-pos value,
          # or whose instance-length value is less than or equal to its
          # last-byte-pos value, is invalid. The recipient of an invalid
          # byte-content-range-spec MUST ignore it and any content
          # transferred along with it.
          #
          # See http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html (14.16)
          #
          ns_log notice "### ignore invalid range <$range>, pos > size-1, Content-Range: bytes $from-$to/$size // $ranges"
        }
      } elseif {[llength $ranges]>1} {
        ns_log warning "Multiple ranges are currently not supported, ignoring range request"
      }
      if {$::xo::naviserver && ![string match text/* $mime_type]} {
        my write_headers -binary -- $status_code $mime_type $bytes
      } else {
        my write_headers $status_code $mime_type $bytes
      }
    }

    if {$bytes == 0} {
      # Tcl behaves different, when one tries to send 0 bytes via
      # file_copy. So, we handle this special case here...
      # There is actualy nothing to deliver....
      ns_set put [ns_conn outputheaders] "Content-Length" 0
      ns_return 200 $mime_type {}
      return
    }

    if {$use_writerThread && !$use_h264} {
      if {$status_code == 206} {
        ns_log notice "ns_writer submitfile -offset $from -size $bytes $filename"
        ns_writer submitfile -offset $from -size $bytes $filename
      } else {
        ns_log notice "ns_writer submitfile $filename"
        ns_writer submitfile $filename
      }
      return
    }

    set errorMsg ""
    # Get the thread id and make sure the bgdelivery thread is already
    # running.
    set tid [my get_tid]
    
    # my log "+++ lock [my set bgmutex]"
    ::thread::mutex lock [my set mutex]

    #
    # Transfer the channel to the bgdelivery thread and report errors
    # in detail. 
    #
    # Notice, that Tcl versions up to 8.5.4 have a bug in this area.
    # If one uses an earlier version of Tcl, please apply:
    # http://tcl.cvs.sourceforge.net/viewvc/tcl/tcl/generic/tclIO.c?r1=1.61.2.29&r2=1.61.2.30&pathrev=core-8-4-branch
    #

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
    
    if {$use_h264} {
      #my log "MP4 q=[::xo::cc actual_query], h=[ns_set array [ns_conn outputheaders]]"
      my do -async ::h264Spooler spool -delete $delete -channel $ch -filename $filename \
          -context [list [::xo::cc requestor],[::xo::cc url],$query [ns_conn start]] \
          -query $query \
          -client_data $client_data
    } else {
      #my log "FILE SPOOL $filename"
      my do -async ::fileSpooler spool -ranges $ranges -delete $delete -channel $ch -filename $filename \
          -context [list [::xo::cc requestor],[::xo::cc url],$query [ns_conn start]] \
          -client_data $client_data
    }
    #
    # set the length for the access log (which is written when the
    # connection thread is done)
    ns_conn contentsentlength $size       ;# maybe overly optimistic
  }

ad_proc -public ad_returnfile_background {{-client_data ""} status_code mime_type filename} {
  Deliver the given file to the requestor in the background. This proc uses the
  background delivery thread to send the file in an event-driven manner without
  blocking a request thread. This is especially important when large files are 
  requested over slow (e.g. dial-ip) connections.
} {
  #ns_log notice "driver=[ns_conn driver]"
  if {[ns_conn driver] ne "nssock"} {
    ns_returnfile $status_code $mime_type $filename
  } else {
    bgdelivery returnfile -client_data $client_data $status_code $mime_type $filename
  }
}

#####################################
bgdelivery proc subscribe {key {initmsg ""} {mode default} } {
  set ch [ns_conn channel]
  thread::transfer [my get_tid] $ch
  #my do ::Subscriber sweep $key
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

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
