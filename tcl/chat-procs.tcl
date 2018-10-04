xo::library doc {
  Generic chat procs

  @creation-date 2006-02-02
  @author Gustaf Neumann
  @cvs-id $Id$
}

namespace eval ::xo {
  Class create Message -parameter {time user_id msg color {type "message"}}
  Class create Chat -superclass ::xo::OrderedComposite \
      -parameter {
        chat_id
        user_id
        session_id
        {mode default}
        {encoder noencode}
        {timewindow 600}
        {sweepinterval 5}
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

    if {[info commands ::thread::mutex] ne "" &&
        [info commands ::bgdelivery] ne ""} {
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
      # my log "--c setting session_id ${:session_id}: ${:now}"
    } else {
      # my log "--c nothing new for ${:session_id}"
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
        # ns_log warning "-user_id $user auto logout"
        try {::bgdelivery do ::Subscriber sweep chat-[:chat_id]}
      }
    }
    :broadcast_msg [Message new -volatile -type "users" -time [clock seconds]]
    :log "-- ending"
  }

  Chat instproc logout {{-user_id ""} {-msg ""}} {
    set user_id [expr {$user_id ne "" ? $user_id : ${:user_id}}]
    ns_log Notice "--core-chat User $user_id logging out of chat"
    if {${:logout_messages_p}} {
      if {$msg eq ""} {set msg [_ chat.has_left_the_room].}
      :add_msg -uid $user_id -get_new false $msg
    }

    # These values could already not be here. Just ignore when we don't
    # find them
    try {
      ::xo::clusterwide nsv_unset -nocomplain ${:array}-login $user_id
    }
    try {
      ::xo::clusterwide nsv_unset -nocomplain ${:array}-color $user_id
    }
    try {
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
    return [:json_encode_msg [Message new -volatile -type "users" -time [clock seconds]]]
  }

  Chat instproc user_active {user_id} {
    # was the user already active?
    :log "--chat login already avtive? [nsv_exists ${:array}-last-activity $user_id]"
    return [nsv_exists ${:array}-last-activity $user_id]
  }

  Chat instproc login {} {
    :log "--chat login"
    if {${:login_messages_p} && ![:user_active ${:user_id}]} {
      :add_msg -uid ${:user_id} -get_new false [_ xotcl-core.has_entered_the_room]
    } elseif {${:user_id} > 0 && ![nsv_exists ${:array}-login ${:user_id}]} {
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
    set screen_name [acs_user::get_user_info -user_id $user_id -element screen_name]
    if {$screen_name eq ""} {
      set screen_name [person::name -person_id $user_id]
    }
    return $screen_name
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
    return $creator
  }

  Chat instproc urlencode   {string} {ns_urlencode $string}
  Chat instproc noencode    {string} {set string}
  Chat instproc encode      {string} {my [:encoder] $string}
  Chat instproc json_encode {string} {
    string map [list \n \\n \" \\\" ' {\\'} \\ \\\\] $string
  }

  Chat instproc json_encode_msg {msg} {
    set type [$msg type]
    switch $type {
      "message" {
        set message   [$msg msg]
        set user_id   [$msg user_id]
        set color     [$msg color]
        set user      [:user_link -user_id $user_id -color $color]
        set timestamp [clock format [$msg time] -format {[%H:%M:%S]}]
        foreach var {message user timestamp} {
          set $var [:json_encode [set $var]]
        }
        return [subst {{"type": "$type", "message": "$message", "timestamp": "$timestamp", "user": "$user"}\n}]
      }
      "users" {
        set message [list]
        foreach {user_id timestamp} [:active_user_list] {
          if {$user_id < 0} continue
          set timestamp [clock format [expr {[clock seconds] - $timestamp}] -format "%H:%M:%S" -gmt 1]
          set user      [:user_link -user_id $user_id]
          foreach var {user timestamp} {
            set $var [:json_encode [set $var]]
          }
          lappend message [subst {{"timestamp": "$timestamp", "user": "$user"}}]
        }
        set message "\[[join $message ,]\]"
        return [subst {{"type": "$type", "chat_id": "[:chat_id]", "message": $message}\n}]
      }
    }
  }

  Chat instproc js_encode_msg {msg} {
    set json [string trim [:json_encode_msg $msg]]
    if {$json ne ""} {
      return [subst {
        <script type='text/javascript' language='javascript' nonce='$::__csp_nonce'>
           var data = $json;
           parent.getData(data);
        </script>\n
      }]
    } else {
      return
    }
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
  }

  Chat instproc render {} {
    :orderby time
    set result [list]
    # Piggyback the users list in every rendering, this way we don't
    # need a separate ajax request for the polling interface.
    :add [Message new -type "users" -time [clock seconds]]
    foreach child [:children] {
      lappend result [:json_encode_msg $child]
    }
    return "\[[join $result ,]\]"
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
    # empty stub for subclasses to extend
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
