xo::library doc {
  GenericMessageRelay procs

  Provide means to publish a subscription key and let clients write to
  these keys. These functions are used e.g. by the chat.

  @creation-date 2019-02-23
  @author Gustaf Neumann
}

namespace eval ::xo {

  nx::Class create ::xo::MessageRelay {
    :public method subscribe {key {-initmsg ""} {-mode default}} {
      #
      # Subscribe to a service identified by a key.
      #
      # @param key     unique id for a (potentially new communication hub)
      # @param initmsg optional message to be sent, when subscription happens
      # @param mode    optional mode
      #
    }

    :public method send_to_subscriber {key msg} {
      #
      # Send a message to a service identified by the key
      # (communication hub).
      #
      # @param key id for an existing communication hub
      # @param msg message to be sent
      #
    }

    :public method sweep {key} {
      #
      # Check existing subscriptions and clean stale ones.
      #
      # @param  key key of the communication hub
      #
    }

    :public method can_be_used {} {
      #
      # Check, if a message relay can be used in the current
      # configuration.
      #

      return 1
    }

    :method start_of_page {mode} {
      #
      # Compose reply header.
      #
      if {$mode eq "scripted"} {
        set content_type "text/html;charset=utf-8"
        set encoding "Cache-Control: no-cache\r\nTransfer-Encoding: chunked\r\n"
        set body "<html><body>[string repeat { } 1024]\r\n"
        set body [format %x [string length $body]]\r\n$body\r\n
      } else {
        #
        # Chrome refuses to expose partial response to ajax unless we
        # set content_type to octet stream.  Drawback is we have to
        # force the translation on the channel.
        #
        set content_type "application/octet-stream"
        set encoding ""
        set body ""
      }
      return "HTTP/1.1 200 OK\r\nContent-type: $content_type\r\n$encoding\r\n$body"
    }

    :public method encode_message {mode msg} {
      #
      # Provide different "encoding" depending on the mode. Notice
      # that for one chat, multiple clients might have difference
      # modes, since the modes are determined at also by the
      # capabilities of the client browser. So the incoming message
      # has to be recoded multiple times.
      #
      if {$mode eq "scripted"} {
        #::sec_handler_reset
        set emsg [encoding convertto utf-8 $msg]
        #ns_log notice "SEND data <$msg> encoded <$emsg>"
        set jsMsg "<script type='text/javascript' nonce='$::__csp_nonce'>\nvar data = $emsg;\n\
               parent.getData(data);</script>\n"
        set msg [format %x [string length $jsMsg]]\r\n$jsMsg\r\n
      }
      #ns_log notice "#### [self] encode_message <$mode> returns <$msg>"
      return $msg
    }

  }
}

namespace eval ::xo::mr {
  #
  # Create a dummy message relay (which can be always used)
  #
  xo::MessageRelay create ::xo::mr::none

  #
  # Message Relay based on bgdelivery. This interface works directly
  # on the socket and is therefore only useful for plain HTTP
  # connections.
  #
  xo::MessageRelay create ::xo::mr::bgdelivery {

    :public object method subscribe {key {-initmsg ""} {-mode default} } {
      ns_log notice "#### [self] subscribe <$key> mode <$mode>"
      set ch [ns_conn channel]
      thread::transfer [::bgdelivery get_tid] $ch
      # ::bgdelivery do ::Subscriber sweep $key
      ::bgdelivery do ::Subscriber new \
          -channel $ch -key $key \
          -user_id [ad_conn user_id] -mode $mode \
          -start_of_page [:start_of_page $mode]
    }

    :public object method send_to_subscriber {key msg} {
      ns_log notice "#### [self] send_to_subscriber $key $msg"
      ::bgdelivery do -async ::Subscriber broadcast $key $msg
    }

    :public object method can_be_used {} {
      #
      # We require support from the web server, an installed
      # bgdelivery. This method does not work on HTTPS, since this
      # method writes to the raw sockets.
      #
      return [expr {
                    [info commands ::thread::mutex] ne ""
                    && [info commands ::bgdelivery] ne ""
                    && (![ns_conn isconnected] || ![security::secure_conn_p])
                  }]
    }

    :public object method sweep {key} {
      ::bgdelivery do ::Subscriber sweep chat-[:chat_id]}
  }

  #
  # Message Relay based on ns_connchan. This interface works on the
  # full connection structure and can therefore be used for HTTP and
  # HTTPS connections.
  #
  xo::MessageRelay create ::xo::mr::connchan {

    :object method cleanup {key handle} {

      catch {ns_connchan close $handle}
      catch {nsv_unset mr_connchan_$key $handle}
    }

    :public object method subscribe {key {-initmsg ""} {-mode ""} } {
      #ns_log notice "#### [self] subscribe $key mode $mode"
      #
      # Unplug the connection channel from the current connection
      # thread. The currently unplugged channels can be queried via
      # "ns_connchan list"
      #
      set handle [ns_connchan detach]

      #
      # should check and append
      #
      if {![nsv_exists mr_connchan_$key $handle]} {
        nsv_set mr_connchan_$key $handle $mode
        try {
          ns_connchan write $handle [:start_of_page $mode]
        } on error {errorMsg} {
          ns_log warning "message relay: write on <$key> failed: $errorMsg"
          :cleanup $key $handle
        }
      } else {
        ns_log warning "message relay: duplicate registration for <$key> attempted"
      }

      #::bgdelivery do ::Subscriber new \
          #    -channel $ch -key $key \
          #    -user_id [ad_conn user_id] -mode $mode
    }

    :public object method send_to_subscriber {key msg} {
      #
      # Write directly to the subscribers from the connection
      # thread. It would be possible, to perform asynchronous
      # operations via "ns_connchan callback", which would be handled
      # in the background. Not sure, this is necessary.
      #

      #ns_log notice "#### [self] send_to_subscriber <[nsv_array names mr_connchan_$key]>"

      foreach handle [nsv_array names mr_connchan_$key] {
        try {
          ns_connchan write $handle [:encode_message [nsv_array names mr_connchan_$key $handle] $msg]
        } on error {errorMsg} {
          ns_log warning "message relay: duplicate registration for <$key> attempted"
          :cleanup $key $handle
        }
      }
    }

    :public object method can_be_used {} {
      return [expr {[info commands ::ns_connchan] ne ""}]
    }

    :public object method sweep {key} {
      #
      # For the time being, do nothing. Since the chat is verbose on
      # logins/logouts, write operations will fail, in which case the
      # array is cleaned up. If this is not sufficient, probably a
      # "ns_connchan eof" operation would be useful.
      #
    }
  }
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
