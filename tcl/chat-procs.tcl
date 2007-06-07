ad_library {
  generic chat - chat procs
  
  @author Gustaf Neumann and Pablo Muñoz(pablomp@tid.es)
    
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
      nsv_set $cls initialized \
	  [ad_schedule_proc -thread "t" [my sweepinterval] $cls sweep_all_chats]
    }
    if {![nsv_exists $array-seen newest]} {nsv_set $array-seen newest 0}
    if {![nsv_exists $array-color idx]}   {nsv_set $array-color idx 0}
    my init_user_color
  }
  
 

  Chat instproc add_msg {{-get_new:boolean true} -uid msg} {
  
    my instvar array now user_id chat_id
    
     if { $get_new eq "true" } {
     
	db_1row room_info {
		select count(cr.room_id) as count
		from chat_room_user_id as cr
		where cr.room_id = :chat_id
		and cr.user_id = :user_id
	}
	
    	if { $count == 1 || [permission::permission_p -party_id $user_id -object_id [dotlrn::get_package_id] -privilege admin]} {    	       
    		set user_id [expr {[info exists uid] ? $uid : [my set user_id]}]
    		set color   [my user_color $user_id]   
    		
    		set msg $msg
    		my log "-- msg=$msg"
    
    		if {$get_new && [info command ::thread::mutex] ne ""} { 
      		# we could use the streaming interface
      		my broadcast_msg [Message new -volatile -time [clock seconds] \
			    -user_id $user_id -msg $msg -color $color]
	
    	}

    	set msg_id $now.$user_id
    	if { ![nsv_exists $array-login $user_id] } {
      			nsv_set $array-login $user_id [clock seconds]
      	}
    	
    	nsv_set $array $msg_id [list $now [clock seconds] $user_id $msg $color]    
    	nsv_set $array-seen newest $now
    	nsv_set $array-seen last [clock seconds]
    	nsv_set $array-last-activity $user_id $now
    # this in any case a valid result, but only needed for the polling interface
    	if {$get_new} {my get_new}
    	}
    
    
    } else {
    	
    	set user_id [expr {[info exists uid] ? $uid : [my set user_id]}]
    	set color   [my user_color $user_id]    
    	
    	set msg $msg
    	my log "-- msg=$msg"
    
    	if {$get_new && [info command ::thread::mutex] ne ""} { 
    	  # we could use the streaming interface
    	  my broadcast_msg [Message new -volatile -time [clock seconds] \
			    -user_id $user_id -msg $msg -color $color]
	
    	}

   	 set msg_id $now.$user_id
   	 if { ![nsv_exists $array-login $user_id] } {
   	   nsv_set $array-login $user_id [clock seconds]
   	 }
   	 nsv_set $array $msg_id [list $now [clock seconds] $user_id $msg $color]    
   	 nsv_set $array-seen newest $now
   	 nsv_set $array-seen last [clock seconds]
   	 nsv_set $array-last-activity $user_id $now
   	 # this in any case a valid result, but only needed for the polling interface
   	 if {$get_new} {my get_new}
    }
    
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
      nsv_unset $array $key
      #my log "--c unsetting $key"
      return 0
    }
    return 1
  }

  Chat instproc get_new {} {  
    my instvar array now session_id chat_id    
    
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
    my render2 -chat_id $chat_id
  }

  Chat instproc get_all {} {  
    my instvar array now session_id chat_id  
      
    foreach {key value} [nsv_array get $array] {
      
      foreach {timestamp secs user msg color} $value break
      if {[my check_age $key [expr {($now - $timestamp) / 1000}]]} {
	my add [Message new -time $secs -user_id $user -msg $msg -color $color]
      }
    }     
    #my log "--c setting session_id $session_id: $now"    
    nsv_set $array-seen $session_id $now
    my render2 -chat_id $chat_id
  }

  Chat instproc sweeper2 {} {
    my instvar array now chat_id
    my log "-- starting"    
    
    foreach {user timestamp} [nsv_array get $array-last-activity] {      
      set ago [expr {($now - $timestamp) / 1000}]      
      # was 1200      
      if {$ago > 1000} { 
		my add_msg -get_new false -uid $user "auto logout"		
     		db_dml insert_users {delete from chat_room_user_id where room_id = :chat_id and user_id = :user;}
		nsv_unset $array-last-activity $user
		nsv_unset $array-color $user 
		nsv_unset $array-login $user
					
      }
          
    }
    
    my log "-- ending"
  }

  Chat instproc logout {} {  
    my instvar array user_id chat_id
    
    my add_msg -get_new false [_ chat.has_left_the_room].
    
    db_dml insert_users {delete from chat_room_user_id where room_id = :chat_id and user_id = :user_id;}

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
      return
    } else {
      set colors [parameter::get -parameter UserColors -default [[my info class] set colors]]      
      set color [lindex $colors [expr { [nsv_get $array-color idx] % [llength $colors] }]]
      nsv_set $array-color $user_id $color
      nsv_incr $array-color idx
    }
  }
  
  Chat instproc init_user_color {} {
  
    my instvar array user_id
    if { [nsv_exists $array-color $user_id] } {
      return
    } else {
      set colors [parameter::get -parameter UserColors -default [[my info class] set colors]]      
      set color [lindex $colors [expr { [nsv_get $array-color idx] % [llength $colors] }]]
      nsv_set $array-color $user_id $color
      nsv_incr $array-color idx
    }
  }
  
  
  
  Chat instproc get_users {} {
    my instvar chat_id
    set output ""
    set count 0
    	
    
    foreach {user_id timestamp} [my active_user_list] {    
    	     
      set count [expr $count+1]
      if {$user_id > 0} {
      	db_1row room_info {
        	select count(1) as info
        	from chat_registered_users
        	where room_id = :chat_id
        	and user_id = :user_id
    	}
    	if { $info > 0 } {
		db_1row room_info {	
        		select alias as alias
        		from chat_registered_users
        		where room_id = :chat_id
        		and user_id = :user_id
		}
	
	set pp [my sweeper2]		
			
	set color [my user_color $user_id]
	set diff [clock format [expr {[clock seconds] - $timestamp}] -format "%H:%M:%S" -gmt 1]	
	set package_id [ad_conn package_id]
	db_1row url { 
     		   select site_node__url(node_id) as url
        		from site_nodes
        		where object_id=:package_id
    	}
	
	set userlink  [my user_link2 -user_id $user_id -alias $alias]
	set user_id2 [ad_conn user_id]
      		set url2 "private-room?room_id=$chat_id&user_id1=$user_id&user_id2=$user_id2"     		      		      		 
    		append link $url $url2
    		set address [my encode $link]
      		set narrow [dt_right_arrow]     		
      		
		append output "<tr><td><a target='_blank' title='[_ chat-portlet.private_room]' href='$address'><img src='$narrow' /></a></td><td class='user'>$userlink</td> <td class='timestamp'>$diff</td></tr>"
	
	}
	if { $info eq 0 } {
		set link ""
		set package_id [ad_conn package_id]
		db_1row url { 
     		   select site_node__url(node_id) as url
        		from site_nodes
        		where object_id=:package_id
    		}
		set diff [clock format [expr {[clock seconds] - $timestamp}] -format "%H:%M:%S" -gmt 1]
		set userlink  [my user_link -user_id $user_id]
		set user_id2 [ad_conn user_id]
      		set url2 "private-room?room_id=$chat_id&user_id1=$user_id&user_id2=$user_id2"     		      		      		 
    		append link $url $url2
    		set address [my encode $link]
      		set narrow [dt_right_arrow]      		
      			
		append output "<tr><td><a target='_blank' title='[_ chat-portlet.private_room]' href='$address'><img src='$narrow' /></a></td><td class='user'>$userlink</td> <td class='timestamp'>$diff</td></tr>"
	}
      }
    }     
    return $output
  }
  
  
    Chat instproc get_files {} {
	my instvar chat_id
	set output ""
	set count 0   
	
	db_foreach file "select distinct fil.file as file,
        fil.send_file_id,
        ao.package_id,
        cri.parent_id,
        cri.item_id
       from chat_rooms_files_sent as fil,
            acs_objects ao,
            cr_items cri,
            cr_revisions crr
        where fil.send_file_id = crr.revision_id
          and crr.item_id = cri.item_id
          and cri.item_id = ao.object_id
          and fil.room_id = :chat_id " {
	      
	      if {[apm_package_enabled_p dotlrn]} {
		  set community_id [dotlrn_community::get_community_id]
	      } else {
		  set community_id ""
	      }
	      if { ![string eq $community_id ""] } {
		  set fs_package_id [site_node_apm_integration::get_child_package_id \
					 -package_id [dotlrn_community::get_package_id $community_id] \
					 -package_key "file-storage"]
	      } else {
		  set fs_package_id $package_id
	      }

	      

	      set root_folder_id [fs::get_root_folder -package_id $fs_package_id]


	      set root_folder_id [fs::get_root_folder -package_id $fs_package_id]
#	      set folder_path [db_exec_plsql get_folder_path { select content_item__get_path(111,:root_folder_id); }]
	      set fs_file_url [db_string get_fs_file_url { 
		  select 
		  fs.file_upload_name
		  from fs_objects fs
		  where fs.live_revision = :send_file_id
	      }]
	      set file_url "[apm_package_url_from_id $fs_package_id]download/${file}?[export_vars -url {{file_id $item_id}}]"
	      append output "<tr><td class='files'><a href='$file_url' target='_blank'>$file</a></td></tr>"
# 	      set url [ad_conn url]
# 	   set inicio 0
# 	   set final [expr [string length $url]-16]
# 	   set comm_name [string range $url $inicio $final]
# 	   if { [string length $comm_name] > 0 } {              
# 	       append output "<tr><td class='files'><a href='$comm_name/file-storage/view/public\/$file' target='_blank'>$file</a></td></tr>"
# 	   } else {         
# 	       set user_id [ad_conn user_id]
# 	       acs_user::get -user_id $user_id -array user                          
# 	       set name [expr {$user(screen_name) ne "" ? $user(screen_name) : $user(name)}]                          
# 	       set folder_id "$name's Shared Files"
# 	       db_1row room_info {
# 		   select fs.folder_id as id
# 		   from fs_folders as fs
# 		   where fs.name = :folder_id
# 	       }              
# 	       set folder_id $id
# 	       append url_file "dotlrn_fs_" $user_id
# 	       append url_file "_root_folder"
# 	       append url_file2 "dotlrn_fs_" $user_id
# 	       append url_file2 "_shared_folder"
# 	       #append output "<tr><td class='files'><a href='/dotlrn/file-storage/index?folder_id=$folder_id' target='_blank'>$file</a></td></tr>"
# 	       append output "<tr><td class='files'><a href='/dotlrn/file-storage/view/$url_file/$url_file2/\/$file' target='_blank'>$file</a></td></tr>"
# 	   }
       } if_no_rows {
	   append output "<tr><td class='files'>[_ chat.no_files]</td></tr>"
       }
	return $output
    } 
    
  Chat instproc login {} {        
    my instvar array user_id now chat_id
    # was the user already active?
    
    db_1row room_info {
		select maximal_participants as maximal
		from chat_rooms as cp
		where cp.room_id = :chat_id
	}
	db_1row room_info {
		select count(cr.room_id) as count
		from chat_room_user_id as cr
		where cr.room_id = :chat_id
	}	
	if { $count < $maximal} {		
        	db_1row room_info {
			select count(cr.room_id) as count2
			from chat_room_user_id as cr
			where cr.user_id = :user_id
			and cr.room_id = :chat_id		
		}
        	if { $count2 == 0 } {
        		db_dml insert_users {insert into chat_room_user_id (room_id,user_id) values (:chat_id,:user_id);}
        	}
        } 
        if { $count == $maximal} {
               #Check if the user is active and the room is full
		db_1row room_info {
			select count(chat_room_user_id.user_id) as count
			from chat_room_user_id
			where chat_room_user_id.user_id = :user_id
			and chat_room_user_id.room_id = :chat_id
		}	
		if { $count == 0 } {
			if { [permission::permission_p -party_id $user_id -object_id [dotlrn::get_package_id] -privilege admin] } {
				#db_dml insert_users {insert into chat_room_user_id (room_id,user_id) values (:chat_id,:user_id);}
			} else {				
				ns_return 200 text/html "<HTML><BODY>\
				<div id='messages'>[_ chat.You_dont_have_permission_room]</div>\
				</BODY></HTML>"
  				ad_script_abort
  			}
  		}
  	}
    
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
  my instvar chat_id
   if {$user_id > 0} {
        set name [my user_name $user_id]
      set url "/shared/community-member?user%5fid=$user_id"
      if {![info exists color]} {
	set color [my user_color $user_id]
      }
      set user_id2 [ad_conn user_id]     
      set user_info "#chat.user_info#"
      set creator "<a style='color:$color;' title='[_ chat.user_info]' target='_blank' href='$url'>$name</a>"
    } elseif { $user_id == 0 } {
      set creator "Nobody"
    } else {
      set creator "System"
    }  
    return [my encode $creator]  
  }
    
  
  Chat instproc user_link2 { -user_id -color -alias} {    
  my instvar chat_id
   if {$user_id > 0} {
        set name $alias
      set url "/shared/community-member?user%5fid=$user_id"
      set user_id2 [ad_conn user_id]
      
      if {![info exists color]} {
	set color [my user_color $user_id]
      }
      set user_info "#chat.user_info#"
      set creator "<a style='color:$color;' title='[_ chat.user_info]' target='_blank' href='$url'>$alias</a>"
      
    } elseif { $user_id == 0 } {
      set creator "Nobody"
    } else {
      set creator "System"
    }  
    set tt [my encode $creator]
    
    return [my encode $creator]  
  }
  
  Chat instproc user_link3 { -url -color} {    
      
      set creator "<a style='color:$color;' title='[_ chat.user_info]' target='_blank' href='$url'>$url</a>"
   
    return [my encode $creator]  
  }
  
  Chat instproc user_link4 { -url -color} {    
      
      set creator "<a style='color:$color;' title='[_ chat.user_info]' target='_blank' href='http://$url/'>$url</a>"
   
    return [my encode $creator]  
  }
  
  Chat instproc user_link5 { -url -msg -color} {    
      
      set creator "<a style='color:$color;' title='[_ chat.user_info]' target='_blank' href='/../$url'>$msg</a>"
   
    return [my encode $creator]  
  }
  
  Chat instproc urlencode {string} {ns_urlencode $string}
  Chat instproc noencode  {string} {set string}
  Chat instproc encode    {string} {my [my encoder] $string}	

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
    bgdelivery send_to_subscriber chat-[my chat_id] [my json_encode_msg $msg]
  }

  Chat instproc subscribe {-uid} {
    set user_id [expr {[info exists uid] ? $uid : [my set user_id]}]
    set color [my user_color $user_id]
    bgdelivery subscribe chat-[my chat_id] [my json_encode_msg \
	[Message new -volatile -time [clock seconds] \
	     -user_id $user_id -color $color \
	     -msg [_ xotcl-core.has_entered_the_room] ]] [my mode]
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
  
  #pablomp

  Chat instproc render2 {-chat_id } {
   my instvar array          
   my orderby time
    set result ""
    set msg_true "f"
      
    
    db_1row room_info {
        select room.maximal_participants as maxp
        from chat_rooms as room
        where room.room_id = :chat_id        
      }   
     
	  #[nsv_get $array  $msg]
    foreach aux [my array] {
    
      set msg       [$array msg]
      set msg_all ""
     
      for {set i 0} {$i < [llength $msg]} {incr i 1} {
      	set word [lindex $msg $i]
      	
     
      	for {set j 0} {$j < [llength $word]} {incr j 1} {     	
      		if { [string range $word $j $j] eq "h" } {      	      		
      			set aux [expr $j+1]      		
      			if { [string range $word $aux [expr $aux+5] ] eq "ttp://" } { 
      		  		set url [lindex $msg $i]  		  		
      		  		lappend msg_all $i
      		  		set msg_true "t"
      		  		
      			}      		      		
      		} else {
      			if { [string range $word $j $j] eq "w" } { 
      			set aux [expr $j+1]      		
      				if { [string range $word $aux [expr $aux+1] ] eq "ww" } { 
      		  			set url [lindex $msg $i]  		  		
      		  			lappend msg_all $i
      		  			set msg_true "t"
      		  		
      				}
      			}
      		}
      	}
      }
            
      set user_id   [$child user_id]
      set color     [$child color]
      
      
      set timelong  [clock format [$child time]]
      set timeshort [clock format [$child time] -format {[%H:%M:%S]}]
      set timeshort2 [clock format [$child time] -format {[%D]}]
      
      db_1row room_info {
        select count(1) as info
        from chat_registered_users
        where room_id = :chat_id
        and user_id = :user_id
      }
        
    
    
    if { $info > 0 } {
	db_1row room_info {	
        	select alias as alias
        	from chat_registered_users
        	where room_id = :chat_id
        	and user_id = :user_id
	}	
	set userlink  [my user_link2 -user_id $user_id -color $color -alias $alias]
	
	if {$msg_true eq  "t"} {
	
	append result "<p class='line'><span class='timestamp'>$timeshort</span>" \
	  "<span class='user'>$userlink:</span>"
	
	append result "<span class='message'>"
	set k 0
	for {set l 0} {$l < [llength $msg]} {incr l 1} {
		
		
			if { $l eq [lindex $msg_all $k] } {
			
				if { [string range [lindex $msg $l] 0 0] eq "w" } {
					set msg_url  [my user_link4 -url [lindex $msg $l] -color $color]
				} else {
					set msg_url  [my user_link3 -url [lindex $msg $l] -color $color]
				}
				
				append result $msg_url
				append result " "
				if { $k < [llength $msg_all]} {
					set k [expr $k+1]
				}
			} else {
				append result [lindex $msg $l]
				append result " "
			}
		
	  	
	}
	append result "</span></p>\n"
	} else {		
        append result "<p class='line'><span class='timestamp'>$timeshort</span>" \
	  "<span class='user'>$userlink:</span>" \
	  "<span class='message'>[my encode $msg]</span></p>\n"
	}
	  
    }
   
    if {$info eq 0} {   
    	set userlink  [my user_link -user_id $user_id -color $color]
    	
    	if {$msg_true eq  "t"} {
	
	append result "<p class='line'><span class='timestamp'>$timeshort</span>" \
	  "<span class='user'>$userlink:</span>"
	
	append result "<span class='message'>"
	set k 0
	for {set l 0} {$l < [llength $msg]} {incr l 1} {
		
		
			if { $l eq [lindex $msg_all $k] } {
			
				if { [string range [lindex $msg $l] 0 0] eq "w" } {
					set msg_url  [my user_link4 -url [lindex $msg $l] -color $color]
				} else {
					set msg_url  [my user_link3 -url [lindex $msg $l] -color $color]
				}				
				append result $msg_url
				append result " "
				if { $k < [llength $msg_all]} {
					set k [expr $k+1]
				}
			} else {
				append result [lindex $msg $l]
				append result " "
			}
		
	  	
	}
	append result "</span></p>\n"
	} else {

        append result "<p class='line'><span class='timestamp'>$timeshort</span>" \
	  "<span class='user'>$userlink:</span>" \
	  "<span class='message'>[my encode $msg]</span></p>\n"
	}
    }
    }
    
    return $result
  }
  
  
 
  
  ############################################################################
  # Chat meta class, since we need to define general class-specific methods
  ############################################################################
  Class create ChatClass -superclass ::xotcl::Class
  ChatClass method sweep_all_chats {} {
    my log "-- starting----------swee_all_chats"
    foreach nsv [nsv_names "[self]-*-seen"] {
      if { [regexp "[self]-(\[0-9\]+)-seen" $nsv _ chat_id] } {
	my log "--Chat_id $chat_id"	
	my new -volatile -chat_id $chat_id -user_id 0 -session_id 0 -init -sweeper2
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
    # default setting is set19 from http://www.graphviz.org/doc/info/colors.html
    # per parameter settings in the chat package are available (param UserColors)
    my set colors [list #1b9e77 #d95f02 #7570b3 #e7298a #66a61e #e6ab02 #a6761d #666666]
  }
}

