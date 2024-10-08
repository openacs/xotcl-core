::xo::library doc {

  Routines for background delivery of files

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @creation-date 19 Nov 2005
  @cvs-id $Id$
}

if {[info commands ::thread::mutex] eq ""} {
  ns_log notice "libthread does not appear to be available, NOT loading bgdelivery"

  ad_proc -public ad_returnfile_background {{-client_data ""} status_code mime_type filename} {

    This function is a stub-function to be used, when no libthread is
    available.  When used with NaviServer, ns_returnfile uses the
    writer threads, which will be even better than the solution via
    the bgdelivery thread, since it works as well via https and uses
    fewer resources.

    One reason for still using the bgdelivery thread is h264 streaming
    (when the module is in use).

  } {
    security::csp::add_static_resource_header -mime_type $mime_type
    ns_returnfile $status_code $mime_type $filename
  }

  return
}
#return ;# DON'T COMMIT

#
# Old style bgdelivery requires "ns_conn contentsentlength"
#
if {![::acs::icanuse "ns_conn contentsentlength"]} {
  ns_log notice "AOLserver is not patched for bgdelivery, NOT loading bgdelivery"

  ad_proc -public ad_returnfile_background {-client_data status_code mime_type filename} {
    Deliver the given file to the requester in the background. This proc uses the
    background delivery thread to send the file in an event-driven manner without
    blocking a request thread. This is especially important when large files are
    requested over slow (e.g. dial-ip) connections.
  } {
    security::csp::add_static_resource_header -mime_type $mime_type
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
      # currently these are not implemented (missing test cases). The
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
      :end-delivery -client_data $client_data0 $filename0 $fd0 $channel0 -1
    }
    set $k [list $fd $channel $client_data $filename]

    if {$ranges eq ""} {
      ns_log notice "no Range spool for $filename"
      fcopy $fd $channel -command [list [self] end-delivery -client_data $client_data $filename $fd $channel]
    } else {
      :deliver_ranges $ranges $client_data $filename $fd $channel
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
      file delete -- $filename
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
          :end-delivery $filename $fd $channel -1
        }
      }
    }
  }
  fileSpooler proc tick {} {
    if {[catch {:cleanup} errorMsg]} {ns_log error "Error during filespooler cleanup: $errorMsg"}
    set :to [after ${:tick_interval} [list [self] tick]]
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
      :end-delivery -client_data $client_data $filename $handle $channel 0
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
      :end-delivery -client_data $client_data $filename $handle $channel [set $bytesVar]
    } else {
      set block [h264read $handle]
      # one should not use "bytelength" on binary data: https://wiki.tcl-lang.org/8455
      set len [string length $block]
      incr $bytesVar $len
      h264Spooler incr byteCount $len
      if {[catch {puts -nonewline $channel $block} errorMsg]} {
        ns_log notice "h264: error on writing to channel $channel: $errorMsg"
        :end-delivery -client_data $client_data $filename $handle $channel [set $bytesVar]
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
      file delete -- $filename
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
    if {[:verbose]} {ns_log notice "[self] --- $msg"}
  }
  ::AsyncDiskWriter instproc open {-filename {-mode w}} {
    set :channel [open $filename $mode]
    set :content ""
    set :filename $filename
    fconfigure ${:channel} -translation binary -blocking false
    :log "open ${:filename}"
  }

  ::AsyncDiskWriter instproc close {{-sync false}} {
    if {$sync || ${:content} eq ""} {
      :log "close sync"
      if {${:content} ne ""} {
        fconfigure ${:channel} -translation binary -blocking true
        puts -nonewline ${:channel} ${:content}
      }
      close ${:channel}
      :destroy
    } else {
      :log "close async"
      set :finishWhenDone 1
    }
  }
  ::AsyncDiskWriter instproc async_write {block} {
    append :content $block
    fileevent ${:channel} writable [list [self] writeBlock]
  }
  ::AsyncDiskWriter instproc writeBlock {} {
    if {[string length ${:content}] < ${:blocksize}} {
      puts -nonewline ${:channel} ${:content}
      :log "write [string length ${:content}] bytes"
      fileevent ${:channel} writable ""
      set :content ""
      if {[:autoflush]} {flush ${:channel}}
      if {[info exists :finishWhenDone]} {
        :close -sync true
      }
    } else {
      set chunk [string range ${:content} 0 ${:blocksize}-1]
      set :content [string range ${:content} ${:blocksize} end]
      puts -nonewline ${:channel} $chunk
      :log "write [string length $chunk] bytes ([string length ${:content}] buffered)"
    }
  }


  ###############
  # Subscriptions
  ###############
  set ::subscription_count 0
  set ::message_count 0

  ::xotcl::Class create Subscriber -parameter {key channel user_id mode {start_of_page ""}}
  Subscriber proc current {-key } {
    set result [list]
    if {[info exists key]} {
      if {[info exists :subscriptions($key)]} {
        return [list $key [set :subscriptions($key)]]
      }
    } elseif {[info exists :subscriptions]} {
      foreach key [array names :subscriptions] {
        lappend result $key [set :subscriptions($key)]
      }
    }
  }

  Subscriber instproc close {} {
    set channel [:channel]
    #
    # It is important to make the channel nonblocking for the close,
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
    set channel [:channel]
    if {[catch {set eof [eof $channel]}]} {set eof 1}
    :log "sweep [:channel] EOF $eof"
    if {$eof} {
      throw {AD_CLIENTDISCONNECT} "connection $channel closed by peer"
    }
    # make an IO attempt to trigger EOF
    if {[catch {
      set blocking [fconfigure $channel -blocking]
      fconfigure $channel -blocking false
      set x [read $channel]
      fconfigure $channel -blocking $blocking
    } errorMsg]} {
      throw {AD_CLIENTDISCONNECT} "connection $channel closed due to IO error"
    }
  }

  Subscriber instproc send {msg} {
    #ns_log notice "SEND <$msg> [:mode]"
    :log ""
    ::sec_handler_reset
    set smsg [::xo::mr::bgdelivery encode_message [:mode] $msg]
    #:log "-- sending to subscriber for [:key] $smsg ch=[:channel] \
        #        mode=[:mode], user_id ${:user_id}"
    try {
      puts -nonewline [:channel] $smsg
      flush [:channel]
    } on error {errorMsg} {
      # Broken pipe and connection reset are to be expected if the
      # clients (e.g. browser in a chat) leaves the page. It is not a
      # real error.
      set ok_errors {
        "POSIX EPIPE {broken pipe}"
        "POSIX ECONNRESET {connection reset by peer}"
      }
      if {$::errorCode in $ok_errors} {
        throw {AD_CLIENTDISCONNECT} {client disconnected}
      } else {
        throw $::errorCode $errorMsg
      }
    }
  }

  Subscriber proc foreachSubscriber {key method {argument ""}} {
    :msg "$key $method '$argument'"
    if {[info exists :subscriptions($key)]} {
      set subs1 [list]
      foreach s [set :subscriptions($key)] {
        try {
          $s $method $argument
        } trap {AD_CLIENTDISCONNECT} {errMsg} {
          ns_log warning "$method to subscriber $s (key=$key): $errMsg"
          $s destroy
        } on error {errMsg} {
          ns_log error "error in $method to subscriber $s (key=$key): $errMsg\n$::errorInfo"
          $s destroy
        } on ok {result} {
          lappend subs1 $s
        }
      }
      set :subscriptions($key) $subs1
    }
  }

  Subscriber proc broadcast {key msg} {
    :foreachSubscriber $key send $msg
    incr ::message_count
  }

  Subscriber proc sweep {key} {
    :foreachSubscriber $key sweep
  }

  Subscriber instproc destroy {} {
    :close
    next
  }

  Subscriber instproc init {} {
    [:info class] instvar subscriptions
    lappend subscriptions([:key]) [self]
    incr ::subscription_count
    #:log "-- cl=[:info class], subscriptions([:key]) = $subscriptions([:key])"
    fconfigure [:channel] -translation binary

    fconfigure [:channel] -encoding utf-8
    puts -nonewline [:channel] ${:start_of_page}
    flush [:channel]
  }


  ###############
  # HttpSpooler
  ###############

  Class create ::HttpSpooler -parameter {channel {timeout 10000} {counter 0}}
  ::HttpSpooler instproc init {} {
    set :running 0
    set :release 0
    set :spooling 0
    set :queue [list]
  }
  ::HttpSpooler instproc all_done {} {
    catch {close [:channel]}
    :log ""
    :destroy
  }
  ::HttpSpooler instproc release {} {
    # release indicates the when running becomes 0, the spooler is finished
    set :release 1
    if {${:running} == 0} {:all_done}
  }
  ::HttpSpooler instproc done {reason request} {
    incr :running -1
    :log "--running ${:running}"
    $request destroy
    if {${:running} == 0 && ${:release}} {:all_done}
  }
  ::HttpSpooler instproc deliver {data request {encoding binary}} {
    :log "-- spooling ${:spooling}"
    if {${:spooling}} {
      :log "--enqueue"
      lappend :queue $data $request $encoding
    } else {
      #:log "--send"
      set :spooling 1
      # puts -nonewline [:channel] $data
      # :done

      set fd [file tempfile spool_filename [ad_tmpdir]/nsd-spool-XXXXXX]
      fconfigure $fd -translation binary -encoding $encoding
      puts -nonewline $fd $data
      close $fd

      set fd [open $spool_filename]
      fconfigure $fd -translation binary -encoding $encoding
      fconfigure [:channel] -translation binary  -encoding $encoding
      fcopy $fd [:channel] -command \
          [list [self] end-delivery $spool_filename $fd [:channel] $request]
    }
  }
  ::HttpSpooler instproc end-delivery {filename fd ch request bytes args} {
    :log "--- end of delivery of $filename, $bytes bytes written $args"
    if {[catch {close $fd} e]} {ns_log notice "httpspool, closing file $filename, error: $e"}
    set :spooling 0
    if {[llength ${:queue}]>0} {
      :log "--dequeue"
      lassign ${:queue} data req enc
      set :queue [lreplace ${:queue} 0 2]
      :deliver $data $req $enc
    }
    :done delivered $request
  }
  ::HttpSpooler instproc add {-request {-post_data ""}} {
    if {[regexp {http://([^/]*)(/.*)} $request _ host path]} {
      set port 80
      regexp {^([^:]+):(.*)$} $host _ host port
      incr :running
      xo::AsyncHttpRequest [self]::[incr :counter] \
          -host $host -port $port -path $path \
          -timeout [:timeout] -post_data $post_data -request_manager [self]
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

    Deliver the given file to the requester in the background. This proc uses the
    background delivery thread to send the file in an event-driven manner without
    blocking a request thread. This is especially important when large files are
    requested over slow connections.

    With NaviServer, this function is mostly obsolete, at least, when
    writer threads are configured. The writer threads have as well the
    advantage, that these can be used with https, while the bgdelivery
    thread works directly on the socket.

    One remaining purpose of this function is h264 streaming delivery
    (when the module is in use).

  } {

    #ns_setexpires 1000000
    #ns_log notice "expires-set $filename"
    #ns_log notice "status_code = $status_code, filename=$filename"

    if {![nsf::is object ::xo::cc]} {
      ::xo::ConnectionContext require -url [ad_conn url]
    }
    set query [::xo::cc actual_query]
    set secure_conn_p [security::secure_conn_p]
    set use_h264 [expr {[string match "video/mp4*" $mime_type] && $query ne ""
                        && ([string match {*start=[1-9]*} $query] || [string match {*end=[1-9]*} $query])
                        && [info commands h264open] ne ""
                        && !$secure_conn_p }]

    if {[info commands ns_driversection] ne ""} {
      set use_writerThread [ns_config [ns_driversection] writerthreads 0]
    } else {
      set use_writerThread 0
    }

    if {[info exists content_disposition]} {
      set fn [xo::backslash_escape \" $content_disposition]
      ns_set put [ns_conn outputheaders] Content-Disposition "attachment;filename=\"$fn\""
    }

    if {$secure_conn_p && !$use_writerThread} {
      #
      # The bgdelivery thread does not work over https, so fall back
      # to ns_returnfile. The writer thread works fine with https.
      #
      ns_returnfile $status_code $mime_type $filename
      return
    }

    if {$use_h264} {
      if {0} {
        # we have to obtain the size from the file; unfortunately, this
        # requires a duplicate open+close of the h264 stream. If the
        # application is performance sensitive, one might consider to use
        # the possibly incorrect size from the filesystem instead (works
        # perfectly for e.g. flowplayer)
        if {[catch {set handle [h264open $filename $query]} errorMsg]} {
          ns_log error "h264: error opening h264 channel for $filename $query: $errorMsg"
          return
        }
        set size [h264length $handle]
        h264close $handle
      } else {
        set size [ad_file size $filename]
      }
    } else {
      set size [ad_file size $filename]
    }

    # Make sure to set "connection close" for the requests (in other
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
          # nonempty
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
          #ns_log notice "given range <$range>, added header-field Content-Range: bytes $from-$to/$size // $ranges"
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
        :write_headers -binary -- $status_code $mime_type $bytes
      } else {
        :write_headers $status_code $mime_type $bytes
      }
    }

    if {$bytes == 0} {
      # Tcl behaves different, when one tries to send 0 bytes via
      # file_copy. So, we handle this special case here...
      # There is actually nothing to deliver....
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
    set tid [:get_tid]

    # :log "+++ lock ${:bgmutex}"
    ns_mutex_lock ${:mutex}

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

    ns_mutex_unlock ${:mutex}
    #ns_mutex unlock ${:bgmutex}
    # :log "+++ unlock ${:bgmutex}"

    if {$errorMsg ne ""} {
      error ERROR=$errorMsg
    }

    if {$use_h264} {
      #:log "MP4 q=[::xo::cc actual_query], h=[ns_set array [ns_conn outputheaders]]"
      :do -async ::h264Spooler spool -delete $delete -channel $ch -filename $filename \
          -context [list [::xo::cc requester],[::xo::cc url],$query [ns_conn start]] \
          -query $query \
          -client_data $client_data
    } else {
      #:log "FILE SPOOL $filename"
      :do -async ::fileSpooler spool -ranges $ranges -delete $delete -channel $ch -filename $filename \
          -context [list [::xo::cc requester],[::xo::cc url],$query [ns_conn start]] \
          -client_data $client_data
    }
    #
    # set the length for the access log (which is written when the
    # connection thread is done)
    ns_conn contentsentlength $size       ;# maybe overly optimistic
  }


#
# Check at load time, whether h264open is available. If so, perform
# detailed checking if h264 streaming for file deliveries is necessary.
#
if {[info commands h264open] ne ""} {
  #
  # try to use h264 module when required
  #
  ns_log notice "... h264open available"

  ad_proc -private ::xo::use_h264 {{-query ""} mime_type} {

    Check, if the file to be delivered can be and has to be delivered
    via the h264Spooler of bgdelivery

  } {
    if {$query eq ""} {
      if {![nsf::is object ::xo::cc]} {
        ::xo::ConnectionContext require -url [ad_conn url]
      }
      set query [::xo::cc actual_query]
    }
    set use_h264 [expr {[string match "video/mp4*" $mime_type] && $query ne ""
                        && ([string match {*start=[1-9]*} $query] || [string match {*end=[1-9]*} $query])
                        && [info commands h264open] ne ""
                        && ![security::secure_conn_p] }]
    return $use_h264
  }

} else {
  #
  # no h264
  #
  ns_log notice "... no h264open available"
  ad_proc -private ::xo::use_h264 {{-query ""} mime_type} {

    Check, if the file to be delivered can be and has to be delivered
    via the h264Spooler of bgdelivery

  } {
    return 0
  }
}


ad_proc -public ad_returnfile_background {{-client_data ""} status_code mime_type filename} {

  Deliver the given file to the requester in the background.  When
  using NaviServer with its writer threads, ns_returnfile is perfectly
  fine since it delivers its contents already in the background.

  The main reason for using the bgdelivery thread is currently (2019)
  the support of h264 streaming (when the module is in use). So we
  check, whether h264 is available and requested, otherwise pass
  everything to ns_returnfile.
} {
  #ns_log notice "ad_returnfile_background xo::use_h264 -> [xo::use_h264 $mime_type]"

  security::csp::add_static_resource_header -mime_type $mime_type
  if {[xo::use_h264 $mime_type]} {
    bgdelivery returnfile -client_data $client_data $status_code $mime_type $filename
  } else {
    ns_returnfile $status_code $mime_type $filename
  }
}

#####################################
bgdelivery proc -deprecated subscribe {key {initmsg ""} {mode default} } {
  set ch [ns_conn channel]
  thread::transfer [:get_tid] $ch
  #:do ::Subscriber sweep $key
  :do ::Subscriber new -channel $ch -key $key -user_id [ad_conn user_id] -mode $mode
}

bgdelivery proc -deprecated send_to_subscriber {key msg} {
  :do -async ::Subscriber broadcast $key $msg
}
#####################################
bgdelivery proc create_spooler {{-content_type text/plain} {-timeout 10000}} {
  ns_write "HTTP/1.0 200 OK\r\nContent-type: $content_type\r\n\r\n"
  set ch [ns_conn channel]
  thread::transfer [:get_tid] $ch
  :do ::HttpSpooler new -channel $ch -timeout $timeout
}

bgdelivery proc spooler_add_request {spooler request {post_data ""}} {
  :log "-- do -async $spooler add -request $request"
  :do -async $spooler add -request $request -post_data $post_data
}
bgdelivery proc spooler_release {spooler} {
  :do -async $spooler release
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
