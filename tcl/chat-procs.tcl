ad_library {
  generic chat - chat procs

    @creation-date 2006-02-02
    @author Gustaf Neumann
    @cvs-id $Id$  
}

namespace eval ::xo {
  Class Message -parameter {time user_id msg color}
  Class Chat -superclass ::xo::OrderedComposite \
      -parameter {chat_id user_id session_id 
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
      nsv_set $cls initialized \
	  [ad_schedule_proc -thread "t" [my sweepinterval] $cls sweep_all_chats]
    }
    if {![nsv_exists $array-seen newest]} {nsv_set $array-seen newest 0}
    if {![nsv_exists $array-color idx]}   {nsv_set $array-color idx 0}
    my init_user_color
  }

  Chat instproc add_msg {{-get_new:boolean true} -uid msg} {
    my instvar array now
    set user_id [expr {[info exists uid] ? $uid : [my set user_id]}]
    set msg_id $now.$user_id
    if { ![nsv_exists $array-login $user_id] } {
      nsv_set $array-login $user_id [clock seconds]
    }
    nsv_set $array $msg_id [list $now [clock seconds] $user_id $msg [my set user_color]]
    nsv_set $array-seen newest $now
    nsv_set $array-seen last [clock seconds] ;#### PETER?
    nsv_set $array-last-activity $user_id $now
    if {$get_new} {my get_new}
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
      nsv_unset $array $key
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
      nsv_set $array-seen $session_id $now
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
    nsv_set $array-seen $session_id $now
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
        nsv_unset $array-last-activity $user_id
        nsv_unset $array-login $user_id
        nsv_unset $array-color $user_id
    }
  }

  Chat instproc init_user_color {} {
    my instvar array user_id
    if { [nsv_exists $array-color $user_id] } {
      my set user_color [nsv_get $array-color $user_id]
    } else {
      [my info class] instvar colors
      ns_log notice "getting colors of [my info class] = [info exists colors]"
      set color [lindex $colors [expr { [nsv_get $array-color idx] % [llength $colors] }]]
      my set user_color $color
      nsv_set $array-color $user_id $color
      nsv_incr $array-color idx
    }
  }
  
  Chat instproc user_color { user_id } {
    my instvar array
    if { ![nsv_exists $array-color $user_id] } {
        ns_log Notice "Warning: Cannot find user color for chat ($array-color $user_id)!"
        return [lindex [[my info class] set colors] 0]
    }
    return [nsv_get $array-color $user_id]
  }
  
  Chat instproc login {} {
    my instvar array user_id now
    # was the user already active?
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

  Chat instproc user_link { -user_id -color } {
    if {$user_id > 0} {
      acs_user::get -user_id $user_id -array user
      set name [expr {$user(screen_name) ne "" ? $user(screen_name) : $user(name)}]
      #set name [chat_user_name $user_id]
      set url "/shared/community-member?user%5fid=$user_id"
      if {![info exists color]} {
	set color [my user_color $user_id]
      }
      set creator "<a style='color:$color;' target='_blank' href='$url'>$name</a>"
    } else {
      set creator "Nobody"
    }  
    return [my encode $creator]  
  }
  
  Chat instproc urlencode {string} {ns_urlencode $string}
  Chat instproc noencode  {string} {set string}
  Chat instproc encode    {string} {my [my encoder] $string}	
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
    db_foreach get_rooms {
      select room_id, to_char(max(creation_date),'HH24:MI:SS YYYY-MM-DD') as last_activity 
      from chat_msgs group by room_id} {
	nsv_set [self]-$room_id-seen last [clock scan $last_activity]
      }
  }

  ChatClass method flush_messages {-chat_id:required} {
    set array "[self]-$chat_id"
    nsv_unset $array
    nsv_unset $array-seen
    nsv_unset $array-last-activity
  }

  ChatClass method init {} {
    my set colors [list #006400 #0000ff #b8860b #bdb76b #8b0000]
    #ns_log notice "colors of [self] = [my set colors]"
  }
}

