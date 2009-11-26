ad_library {
  generic chat - chat procs

  @creation-date 2006-02-02
  @author Gustaf Neumann
  @cvs-id $Id$  
}

namespace eval ::xo {
  Class Message -parameter {time user_id msg color}
  Class Chat -superclass ::xo::OrderedComposite \
      -parameter {chat_id user_id session_id {mode default}
	{encoder urlencode} {timewindow 600} {sweepinterval 600}
      }

  Chat instproc init {} {
    my instvar array
    # my log "-- "
    my set now [clock clicks -milliseconds]
    if {![my exists user_id]}    {my set user_id [ad_conn user_id]}
    if {![my exists session_id]} {my set session_id [ad_conn session_id]}
    set cls [my info class]
    set array $cls-[my set chat_id]
    if {![nsv_exists $cls initialized]} {
      my log "-- initialize $cls"
      $cls initialize_nsvs
      ::xo::clusterwide nsv_set $cls initialized \
	  [ad_schedule_proc -thread "t" [my sweepinterval] $cls sweep_all_chats]
    }
    if {![nsv_exists $array-seen newest]} {::xo::clusterwide nsv_set $array-seen newest 0}
    if {![nsv_exists $array-color idx]}   {::xo::clusterwide nsv_set $array-color idx 0}
    if {[my user_id] != 0 || [my session_id] != 0} {
      my init_user_color
    }
  }



  Chat instproc register_nsvs {msg_id user_id msg color secs} {
    my instvar array now
    if { ![nsv_exists $array-login $user_id] } {
      ::xo::clusterwide nsv_set $array-login $user_id $secs
    }
    ::xo::clusterwide nsv_set $array $msg_id [list $now $secs $user_id $msg $color]
    ::xo::clusterwide nsv_set $array-seen newest $now
    ::xo::clusterwide nsv_set $array-seen last $secs
    ::xo::clusterwide nsv_set $array-last-activity $user_id $now
  }

  Chat instproc add_msg {{-get_new:boolean true} -uid msg} {
    my log "--chat adding $msg"
    my instvar array now
    set user_id [expr {[info exists uid] ? $uid : [my set user_id]}]
    set color   [my user_color $user_id]
    set msg     [ad_quotehtml $msg]
    my log "-- msg=$msg"
    
    if {$get_new 
	&& [info command ::thread::mutex] ne "" 
	&& [info command ::bgdelivery] ne ""} { 
      # we could use the streaming interface
      my broadcast_msg [Message new -volatile -time [clock seconds] \
			    -user_id $user_id -msg $msg -color $color]
    }
    my register_nsvs $now.$user_id $user_id $msg $color [clock seconds]
    # this in any case a valid result, but only needed for the polling interface
    if {$get_new} {my get_new}
  }

  Chat instproc current_message_valid {} {
    expr { [my exists user_id] && [my set user_id] != -1 }
  }
  
  Chat instproc active_user_list {} {
    nsv_array get [my set array]-login
  }
  
  Chat instproc nr_active_users {} {
      expr { [llength [nsv_array get [my set array]-login]] / 2 }
  }
  
  Chat instproc last_activity {} {
    if { ![nsv_exists [my set array]-seen last] } { return "-" }
    return [clock format [nsv_get [my set array]-seen last] -format "%d.%m.%y %H:%M:%S"]
  }
  
  Chat instproc check_age {key ago} {
    my instvar array timewindow
    if {$ago > $timewindow} {
      ::xo::clusterwide nsv_unset $array $key
      #my log "--c unsetting $key"
      return 0
    }
    return 1
  }

  Chat instproc get_new {} {
    my instvar array now session_id
    set last [expr {[nsv_exists $array-seen $session_id] ? [nsv_get $array-seen $session_id] : 0}]
    if {[nsv_get $array-seen newest]>$last} {
      #my log "--c must check $session_id: [nsv_get $array-seen newest] > $last"
      foreach {key value} [nsv_array get $array] {
	foreach {timestamp secs user msg color} $value break
	if {$timestamp > $last} {
	  my add [Message new -time $secs -user_id $user -msg $msg -color $color]
	} else {
	  my check_age $key [expr {($now - $timestamp) / 1000}]
	}
      }
      ::xo::clusterwide nsv_set $array-seen $session_id $now
      #my log "--c setting session_id $session_id: $now"
    } else {
      #my log "--c nothing new for $session_id"
    }
    my render
  }

  Chat instproc get_all {} {
    my instvar array now session_id
    foreach {key value} [nsv_array get $array] {
      foreach {timestamp secs user msg color} $value break
      if {[my check_age $key [expr {($now - $timestamp) / 1000}]]} {
	my add [Message new -time $secs -user_id $user -msg $msg -color $color]
      }
    }
    #my log "--c setting session_id $session_id: $now"
    ::xo::clusterwide nsv_set $array-seen $session_id $now
    my render
  }

  Chat instproc sweeper {} {
    my instvar array now
    my log "-- starting"
    foreach {user timestamp} [nsv_array get $array-last-activity] {
      ns_log Notice "YY at user $user with $timestamp"
      set ago [expr {($now - $timestamp) / 1000}]
      ns_log Notice "YY Checking: now=$now, timestamp=$timestamp, ago=$ago"
      # was 1200
      if {$ago > 300} { 
	my add_msg -get_new false -uid $user "auto logout" 
	nsv_unset $array-last-activity $user 
	nsv_unset $array-login $user
	nsv_unset $array-color $user
      }
    }
    my log "-- ending"
  }

  Chat instproc logout {} {
    my instvar array user_id
    ns_log Notice "YY User $user_id logging out of chat"
    my add_msg -get_new false [_ chat.has_left_the_room].
    catch {
      # do not try to clear nsvs, if they are not available
      # this situation could occur after a server restart, after which the user tries to leave the room
      ::xo::clusterwide nsv_unset $array-last-activity $user_id
      ::xo::clusterwide nsv_unset $array-login $user_id
      ::xo::clusterwide nsv_unset $array-color $user_id
    }
  }

  Chat instproc init_user_color {} {
    my instvar array user_id
    if { [nsv_exists $array-color $user_id] } {
      return
    } else {
      set colors [::xo::parameter get -parameter UserColors -default [[my info class] set colors]]
      # ns_log notice "getting colors of [my info class] = [info exists colors]"
      set color [lindex $colors [expr { [nsv_get $array-color idx] % [llength $colors] }]]
      ::xo::clusterwide nsv_set $array-color $user_id $color
      ::xo::clusterwide nsv_incr $array-color idx
    }
  }
  
  Chat instproc get_users {} {
    set output ""
    foreach {user_id timestamp} [my active_user_list] {
      if {$user_id > 0} {
	set diff [clock format [expr {[clock seconds] - $timestamp}] -format "%H:%M:%S" -gmt 1]
	set userlink  [my user_link -user_id $user_id]
	append output "<TR><TD class='user'>$userlink</TD><TD class='timestamp'>$diff</TD></TR>\n"
      }
    }     
    return $output
  }
  
  Chat instproc login {} {
    my log "--chat login"
    my instvar array user_id now
    # was the user already active?
    my log "--chat login already avtive? [nsv_exists $array-last-activity $user_id]"
    if {![nsv_exists $array-last-activity $user_id]} {
      my add_msg -get_new false [_ xotcl-core.has_entered_the_room]
    }
    my encoder noencode
    #my log "--c setting session_id [my set session_id]: $now"
    my get_all
  }

  Chat instproc user_color { user_id } {
    my instvar array
    if { ![nsv_exists $array-color $user_id] } {
      my log "warning: Cannot find user color for chat ($array-color $user_id)!"
      return [lindex [[my info class] set colors] 0]
    }
    return [nsv_get $array-color $user_id]
  }

  Chat instproc user_name { user_id } {
      acs_user::get -user_id $user_id -array user
      return [expr {$user(screen_name) ne "" ? $user(screen_name) : $user(name)}]
  }
  
  Chat instproc user_link { -user_id -color } {
    if {$user_id > 0} {
      set name [my user_name $user_id]
      set url "/shared/community-member?user%5fid=$user_id"
      if {![info exists color]} {
	set color [my user_color $user_id]
      }
      set creator "<a style='color:$color;' target='_blank' href='$url'>$name</a>"
    } elseif { $user_id == 0 } {
      set creator "Nobody"
    } else {
      set creator "System"
    }  
    return [my encode $creator]  
  }
  
  Chat instproc urlencode   {string} {ns_urlencode $string}
  Chat instproc noencode    {string} {set string}
  Chat instproc encode      {string} {my [my encoder] $string}
  Chat instproc json_encode {string} {
    string map [list \n \\n {"} {\"} ' {\'}] $string ;#"
  }
    
  Chat instproc json_encode_msg {msg} {
    set old [my encoder]
    my encoder noencode ;# just for user_link
    set userlink [my user_link -user_id [$msg user_id] -color [$msg color]]
    my encoder $old
    set timeshort [clock format [$msg time] -format {[%H:%M:%S]}]
    set text [my json_encode [$msg msg]]
    foreach var {userlink timeshort} {set $var [my json_encode [set $var]]}
    return [subst -nocommands {{'messages': [
      {'user':'$userlink', 'time': '$timeshort', 'msg':'$text'}
    ]\n}
    }]
  }

  Chat instproc js_encode_msg {msg} {
    set json [my json_encode_msg $msg]
    return "<script type='text/javascript' language='javascript'>
    var data = $json;
    parent.getData(data);
    </script>\n"
  }

  Chat instproc broadcast_msg {msg} {
    my log "--chat broadcast_msg $msg"
    ::xo::clusterwide \
        bgdelivery send_to_subscriber chat-[my chat_id] [my json_encode_msg $msg]
  }

  Chat instproc subscribe {-uid} {
    set user_id [expr {[info exists uid] ? $uid : [my set user_id]}]
    set color [my user_color $user_id]
    bgdelivery subscribe chat-[my chat_id] "" [my mode] 
    my broadcast_msg [Message new -volatile -time [clock seconds] \
                          -user_id $user_id -color $color \
                          -msg [_ xotcl-core.has_entered_the_room] ]
  }

  Chat instproc render {} {
    my orderby time
    set result ""
    foreach child [my children] {
      set msg       [$child msg]
      set user_id   [$child user_id]
      set color     [$child color]
      set timelong  [clock format [$child time]]
      set timeshort [clock format [$child time] -format {[%H:%M:%S]}]
      set userlink  [my user_link -user_id $user_id -color $color]

      append result "<p class='line'><span class='timestamp'>$timeshort</span>" \
	  "<span class='user'>$userlink:</span>" \
	  "<span class='message'>[my encode $msg]</span></p>\n"
    }
    return $result
  }

  
  ############################################################################
  # Chat meta class, since we need to define general class-specific methods
  ############################################################################
  Class create ChatClass -superclass ::xotcl::Class
  ChatClass method sweep_all_chats {} {
    my log "-- starting"
    foreach nsv [nsv_names "[self]-*-seen"] {
      if { [regexp "[self]-(\[0-9\]+)-seen" $nsv _ chat_id] } {
	my log "--Chat_id $chat_id"
	my new -volatile -chat_id $chat_id -user_id 0 -session_id 0 -init -sweeper
      }
    }
    my log "-- ending"
  }
    
  ChatClass method initialize_nsvs {} {
    # read the last_activity information at server start into a nsv array
    db_foreach [my qn get_rooms] {
      select room_id, to_char(max(creation_date),'HH24:MI:SS YYYY-MM-DD') as last_activity 
      from chat_msgs group by room_id} {
	::xo::clusterwide nsv_set [self]-$room_id-seen last [clock scan $last_activity]
      }
  }

  ChatClass method flush_messages {-chat_id:required} {
    set array "[self]-$chat_id"
    ::xo::clusterwide nsv_unset $array
    ::xo::clusterwide nsv_unset $array-seen
    ::xo::clusterwide nsv_unset $array-last-activity
  }

  ChatClass method init {} {
    # default setting is set19 from http://www.graphviz.org/doc/info/colors.html
    # per parameter settings in the chat package are available (param UserColors)
    my set colors [list #1b9e77 #d95f02 #7570b3 #e7298a #66a61e #e6ab02 #a6761d #666666]
  }
}

