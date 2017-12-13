xo::library doc {
  Generic chat procs

  @creation-date 2006-02-02
  @author Gustaf Neumann
  @cvs-id $Id$
}

namespace eval ::xo {
  Class create Message -parameter {time user_id msg color}
  Class create Chat -superclass ::xo::OrderedComposite \
      -parameter {
        chat_id
        user_id
        session_id
        {mode default}
        {encoder noencode}
        {timewindow 600}
        {sweepinterval 599}
        {login_messages_p t}
        {logout_messages_p t}
      }

  Chat instproc init {} {
    # :log "-- "
    set :now [clock clicks -milliseconds]
    if {![info exists :user_id]} {
      set :user_id [ad_conn user_id]
    }
    if {![info exists :session_id]} {
      set :session_id [ad_conn session_id]
    }
    set cls [:info class]
    set :array $cls-${:chat_id}
    if {![nsv_exists $cls initialized]} {
      :log "-- initialize $cls"
      $cls initialize_nsvs
      ::xo::clusterwide nsv_set $cls initialized \
          [ad_schedule_proc \
               -thread "t" [:sweepinterval] $cls sweep_all_chats]
    }
    if {![nsv_exists ${:array}-seen newest]} {
      ::xo::clusterwide nsv_set ${:array}-seen newest 0
    }
    if {![nsv_exists ${:array}-color idx]} {
      ::xo::clusterwide nsv_set ${:array}-color idx 0
    }
    if {[:user_id] != 0 || [:session_id] != 0} {
      :init_user_color
    }
  }



  Chat instproc register_nsvs {msg_id user_id msg color secs} {
    # Tell the system we are back again, in case we were auto logged out
    if { ![nsv_exists ${:array}-login $user_id] } {
      ::xo::clusterwide nsv_set ${:array}-login $user_id [clock seconds]
    }
    ::xo::clusterwide nsv_set ${:array} $msg_id [list ${:now} $secs $user_id $msg $color]
    ::xo::clusterwide nsv_set ${:array}-seen newest ${:now}
    ::xo::clusterwide nsv_set ${:array}-seen last $secs
    ::xo::clusterwide nsv_set ${:array}-last-activity $user_id ${:now}
  }

  Chat instproc add_msg {{-get_new:boolean true} {-uid ""} msg} {
    # :log "--chat adding $msg"
    set user_id [expr {$uid ne "" ? $uid : ${:user_id}}]
    set color   [:user_color $user_id]
    set msg     [ns_quotehtml $msg]
    # :log "-- msg=$msg"

    if {$get_new
        && [info commands ::thread::mutex] ne ""
        && [info commands ::bgdelivery] ne ""} {
      # we could use the streaming interface
      :broadcast_msg [Message new -volatile -time [clock seconds] \
                            -user_id $user_id -color $color [list -msg $msg]]
    }
    :register_nsvs ${:now}.$user_id $user_id $msg $color [clock seconds]
    # this in any case a valid result, but only needed for the polling interface
    if {$get_new} {my get_new}
  }

  Chat instproc current_message_valid {} {
    expr { [info exists :user_id] && ${:user_id} != -1 }
  }

  Chat instproc active_user_list {} {
    nsv_array get ${:array}-login
  }

  Chat instproc nr_active_users {} {
    expr { [llength [nsv_array get ${:array}-login]] / 2 }
  }

  Chat instproc last_activity {} {
    if { ![nsv_exists ${:array}-seen last] } { return "-" }
    return [clock format [nsv_get ${:array}-seen last] -format "%d.%m.%y %H:%M:%S"]
  }

  Chat instproc check_age {key ago} {
    if {$ago > ${:timewindow}} {
      ::xo::clusterwide nsv_unset ${:array} $key
      #my log "--c unsetting $key"
      return 0
    }
    return 1
  }

  Chat instproc get_new {} {
    set last [expr {[nsv_exists ${:array}-seen ${:session_id}] ? [nsv_get ${:array}-seen ${:session_id}] : 0}]
    if {[nsv_get ${:array}-seen newest]>$last} {
      #my log "--c must check ${:session_id}: [nsv_get ${:array}-seen newest] > $last"
      foreach {key value} [nsv_array get ${:array}] {
        lassign $value timestamp secs user msg color
        if {$timestamp > $last} {
          #
          # add the message to the ordered composite.
          #
          :add [Message new -time $secs -user_id $user -msg $msg -color $color]
        } else {
          :check_age $key [expr {(${:now} - $timestamp) / 1000}]
        }
      }
      ::xo::clusterwide nsv_set ${:array}-seen ${:session_id} ${:now}
      #my log "--c setting session_id ${:session_id}: ${:now}"
    } else {
      #my log "--c nothing new for ${:session_id}"
    }
    :render
  }

  Chat instproc get_all {} {
    foreach {key value} [nsv_array get ${:array}] {
      lassign $value timestamp secs user msg color
      if {[:check_age $key [expr {(${:now} - $timestamp) / 1000}]]} {
        :add [Message new -time $secs -user_id $user -msg $msg -color $color]
      }
    }
    #my log "--c setting session_id ${:session_id}: ${:now}"
    ::xo::clusterwide nsv_set ${:array}-seen ${:session_id} ${:now}
    :render
  }

  Chat instproc sweeper {} {
    :log "--core-chat starting"
    foreach {user timestamp} [nsv_array get ${:array}-last-activity] {
      ns_log Notice "--core-chat at user $user with $timestamp"
      set ago [expr {(${:now} - $timestamp) / 1000}]
      ns_log Notice "--core-chat Checking: now=${:now}, timestamp=$timestamp, ago=$ago"
      # was 1200
      if {$ago > 300} {
        :logout -user_id $user -msg "auto logout"
        catch {::bgdelivery do ::Subscriber sweep chat-[:chat_id]}
      }
    }
    :log "-- ending"
  }

  Chat instproc logout {{-user_id ""} {-msg ""}} {
    set user_id [expr {$user_id ne "" ? $user_id : ${:user_id}}]
    ns_log Notice "--core-chat User $user_id logging out of chat"
    if {${:logout_messages_p}} {
      if {$msg eq ""} {set msg [_ chat.has_left_the_room].}
      :add_msg -get_new false $msg
    }
    
    # This values could already not be here. Just ignore when we don't
    # find them
    catch {
      ::xo::clusterwide nsv_unset -nocomplain ${:array}-login $user_id
    }
    catch {
      ::xo::clusterwide nsv_unset -nocomplain ${:array}-color $user_id
    }
    catch {
      ::xo::clusterwide nsv_unset -nocomplain ${:array}-last-activity $user_id
    }
  }

  Chat instproc init_user_color {} {
    if { [nsv_exists ${:array}-color ${:user_id}] } {
      return
    } else {
      set colors [::xo::parameter get -parameter UserColors -default [[:info class] set colors]]
      # ns_log notice "getting colors of [:info class] = [info exists colors]"
      set color [lindex $colors [expr { [nsv_get ${:array}-color idx] % [llength $colors] }]]
      ::xo::clusterwide nsv_set ${:array}-color ${:user_id} $color
      ::xo::clusterwide nsv_incr ${:array}-color idx
    }
  }

  Chat instproc get_users {} {
    set output ""
    foreach {user_id timestamp} [:active_user_list] {
      if {$user_id > 0} {
        set diff [clock format [expr {[clock seconds] - $timestamp}] -format "%H:%M:%S" -gmt 1]
        set userlink  [:user_link -user_id $user_id]
        append output "<TR><TD class='user'>$userlink</TD><TD class='timestamp'>$diff</TD></TR>\n"
      }
    }
    return $output
  }

  Chat instproc user_active {user_id} {
    # was the user already active?
    :log "--chat login already avtive? [nsv_exists ${:array}-last-activity $user_id]"
    return [nsv_exists ${:array}-last-activity $user_id]
  }

  Chat instproc login {} {
    :log "--chat login"
    if {${:login_messages_p} && ![:user_active ${:user_id}]} {
      :add_msg -get_new false [_ xotcl-core.has_entered_the_room]
    } elseif {![nsv_exists ${:array}-login ${:user_id}]} {
      # give some proof of our presence to the chat system when we
      # don't issue the login message
      ::xo::clusterwide nsv_set ${:array}-login ${:user_id} [clock seconds]
      ::xo::clusterwide nsv_set ${:array}-last-activity ${:user_id} ${:now}
    }
    :encoder noencode
    :log "--c setting session_id ${:session_id}: ${:now}"
    return [:get_all]
  }

  Chat instproc user_color { user_id } {
    if { ![nsv_exists ${:array}-color $user_id] } {
      :log "warning: Cannot find user color for chat (${:array}-color $user_id)!"
      return [lindex [[:info class] set colors] 0]
    }
    return [nsv_get ${:array}-color $user_id]
  }

  Chat instproc user_name { user_id } {
    acs_user::get -user_id $user_id -array user
    return [expr {$user(screen_name) ne "" ? $user(screen_name) : $user(name)}]
  }

  Chat instproc user_link { -user_id -color } {
    if {$user_id > 0} {
      set name [:user_name $user_id]
      set url "/shared/community-member?user%5fid=$user_id"
      if {![info exists color]} {
        set color [:user_color $user_id]
      }
      set creator "<a style='color:$color;' target='_blank' href='[ns_quotehtml $url]'>$name</a>"
    } elseif { $user_id == 0 } {
      set creator "Nobody"
    } else {
      set creator "System"
    }
    return [:encode $creator]
  }

  Chat instproc urlencode   {string} {ns_urlencode $string}
  Chat instproc noencode    {string} {set string}
  Chat instproc encode      {string} {my [:encoder] $string}
  Chat instproc json_encode {string} {
    string map [list \n \\n \" \\\" ' {\'}] $string
  }

  Chat instproc json_encode_msg {msg} {
    set old [:encoder]
    :encoder noencode ;# just for user_link
    set userlink [:user_link -user_id [$msg user_id] -color [$msg color]]
    :encoder $old
    set timeshort [clock format [$msg time] -format {[%H:%M:%S]}]
    set text [:json_encode [$msg msg]]
    foreach var {userlink timeshort} {set $var [:json_encode [set $var]]}
    return [subst -nocommands {{'messages': [
            {'user':'$userlink', 'time': '$timeshort', 'msg':'$text'}
           ]\n}
    }]
  }

  Chat instproc js_encode_msg {msg} {
    set json [:json_encode_msg $msg]
    return "<script type='text/javascript' language='javascript' nonce='$::__csp_nonce'>
    var data = $json;
    parent.getData(data);
    </script>\n"
  }

  Chat instproc broadcast_msg {msg} {
    :log "--chat broadcast_msg"
    ::xo::clusterwide \
        bgdelivery send_to_subscriber chat-[:chat_id] [:json_encode_msg $msg]
  }

  Chat instproc subscribe {-uid} {
    set user_id [expr {[info exists uid] ? $uid : ${:user_id}}]
    set color [:user_color $user_id]
    bgdelivery subscribe chat-[:chat_id] "" [:mode]
    if {${:login_messages_p} && ![:user_active $user_id]} {
      :broadcast_msg [Message new -volatile -time [clock seconds] \
                            -user_id $user_id -color $color \
                            -msg [_ xotcl-core.has_entered_the_room] ]
    }
    #my get_all
  }

  Chat instproc render {} {
    :orderby time
    set result "<div class='messages'>\n"
    foreach child [:children] {
      set msg       [$child msg]
      set user_id   [$child user_id]
      set color     [$child color]
      set timelong  [clock format [$child time]]
      set timeshort [clock format [$child time] -format {[%H:%M:%S]}]
      set userlink  [:user_link -user_id $user_id -color $color]
      ns_log notice "encode <$msg> using encoder [:encoder] gives <[:encode $msg]>"
      append result "<p class='line'><span class='timestamp'>$timeshort</span> " \
                        "<span class='user'>$userlink</span> " \
                        "<span class='message'>[:encode $msg]</span></p>\n"
    }
    append result "</div>"
    return $result
  }

  ############################################################################
  # Chat meta class, since we need to define general class-specific methods
  ############################################################################
  Class create ChatClass -superclass ::xotcl::Class
  ChatClass method sweep_all_chats {} {
    :log "-- starting"
    foreach nsv [nsv_names "[self]-*-seen"] {
      if { [regexp "[self]-(\[0-9\]+)-seen" $nsv _ chat_id] } {
        :log "--Chat_id $chat_id"
        :new -volatile -chat_id $chat_id -user_id 0 -session_id 0 -init -sweeper
      }
    }
    :log "-- ending"
  }

  ChatClass method initialize_nsvs {} {
    # read the last_activity information at server start into a nsv array
    ::xo::dc foreach get_rooms {
      select room_id, to_char(max(creation_date),'HH24:MI:SS YYYY-MM-DD') as last_activity
      from chat_msgs group by room_id
    } {
      ::xo::clusterwide nsv_set [self]-$room_id-seen last [clock scan $last_activity]
    }
  }

  ChatClass method flush_messages {-chat_id:required} {
    set array "[self]-$chat_id"
    ::xo::clusterwide nsv_unset -nocomplain $array
    ::xo::clusterwide nsv_unset -nocomplain $array-seen
    ::xo::clusterwide nsv_unset -nocomplain $array-last-activity
  }

  ChatClass method init {} {
    # default setting is set19 from http://www.graphviz.org/doc/info/colors.html
    # per parameter settings in the chat package are available (param UserColors)
    set :colors [list #1b9e77 #d95f02 #7570b3 #e7298a #66a61e #e6ab02 #a6761d #666666]
  }
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
