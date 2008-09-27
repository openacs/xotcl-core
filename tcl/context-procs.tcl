ad_library {
  Definition of a connection context, containing user info, urls, parameters.
  this is used via "Package initialize"... similar as page_contracts and
  for included content (includelets), and used for per-connection caching as well.
  The intention is similar as with ad_conn, but based on objects.
  So far, it is pretty simple, but should get more clever in the future.

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @creation-date 2006-08-06
  @cvs-id $Id$
}

namespace eval ::xo {

  Class create Context -ad_doc {
    This class provides a context for evaluation, somewhat similar to an 
    activation record in programming languages. It combines the parameter
    declaration (e.g. of a page, an includelet) with the actual parameters
    (specified in an includelet) and the provided query values (from the url).
    The parameter decaration are actually XOTcl's non positional arguments.
  } -parameter {
    {parameter_declaration ""} 
    {actual_query " "}
    {package_id 0}
    locale
  }

  # syntactic sugar for includelets, to allow the same syntax as 
  # for "Package initialize ...."; however, we do not allow currently
  # do switch user or package id etc., just the parameter declaration
  Context instproc initialize {{-parameter ""}} {
    my set parameter_declaration $parameter
  }

  Context instproc process_query_parameter {
    {-all_from_query:boolean true}
    {-all_from_caller:boolean true}
    {-caller_parameters}
  } {
    my instvar queryparm actual_query 
    my proc __parse [my parameter_declaration]  {
      foreach v [info vars] { uplevel [list set queryparm($v) [set $v]]}
    }
    
    foreach v [my parameter_declaration] {
      set ([lindex [split [lindex $v 0] :] 0]) 1
    }
    if {$actual_query eq " "} {
      set actual_query [ns_conn query]
      #my log "--CONN ns_conn query = <$actual_query>"
    }

    # get the query parameters (from the url)
    #my log "--P processing actual query $actual_query"
    foreach querypart [split $actual_query &] {
      set name_value_pair [split $querypart =]
      set att_name  [ns_urldecode [lindex $name_value_pair 0]]
      if {[llength $name_value_pair] == 1} { 
        set att_value 1 
      }  else {
        set att_value [ns_urldecode [lindex $name_value_pair 1]]
      }
      if {[info exists (-$att_name)]} {
        lappend passed_args(-$att_name) $att_value
      } elseif {$all_from_query} {
        set queryparm($att_name) $att_value
      }
    }

    # get the query parameters (from the form if necessary)
    if {[my istype ::xo::ConnectionContext]} {
      foreach param [array names ""] {
	#my log "--cc check $param [info exists passed_args($param)]"
	set name [string range $param 1 end]
	if {![info exists passed_args($param)] &&
	    [my exists_form_parameter $name]} {
	  #my log "--cc adding passed_args(-$name) [my form_parameter $name]"
	  set passed_args($param) [my form_parameter $name]
	}
      }
    }
    
    # get the caller parameters (e.g. from the includelet call)
    if {[info exists caller_parameters]} {
      #my log "--cc caller_parameters=$caller_parameters"
      array set caller_param $caller_parameters
    
      foreach param [array names caller_param] {
        if {[info exists ($param)]} { 
          set passed_args($param) $caller_param($param) 
        } elseif {$all_from_caller} {
          set queryparm([string range $param 1 end]) $caller_param($param) 
        }
      }
    }

    set parse_args [list]
    foreach param [array names passed_args] {
      lappend parse_args $param $passed_args($param)
    }
    
    #my log "--cc calling parser eval [self] __parse $parse_args"
    eval [self] __parse $parse_args
    #my log "--cc qp [array names queryparm] // $actual_query"
  }

  Context instproc original_url_and_query args {
    if {[llength $args] == 1} {
      my set original_url_and_query [lindex $args 0]
    } elseif {[my exists original_url_and_query]} {
      return [my set original_url_and_query]
    } else {
      return [my url]?[my actual_query]
    }
  }

  Context instproc query_parameter {name {default ""}} {
    my instvar queryparm
    return [expr {[info exists queryparm($name)] ? $queryparm($name) : $default}]
  }
  Context instproc exists_query_parameter {name} {
    #my log "--qp my exists $name => [my exists queryparm($name)]"
    my exists queryparm($name)
  }
  Context instproc get_all_query_parameter {} {
    return [my array get queryparm]
  }

  Context ad_instproc export_vars {{-level 1}} {
    Export the query variables
    @param level target level
  } {
    my instvar queryparm package_id
    foreach p [my array names queryparm] {
      set value [my set queryparm($p)]
      uplevel $level [list set $p [my set queryparm($p)]]
    }
    uplevel $level [list set package_id $package_id]
    #::xo::show_stack
  }


  Context ad_instproc get_parameters {} {
    Conveniance routine for includelets. It combines the actual
    parameters from the call in the page (highest priority) wit
    the values from the url (second priority) and the default
    values from the signature
  } {
    set source [expr {[my exists __caller_parameters] ? 
                      [self] : [my info parent]}]
    $source instvar __caller_parameters
    
    if {![my exists __including_page]} {
      # a includelet is called from the toplevel. the actual_query might
      # be cached, so we reset it here.
      my actual_query [::xo::cc actual_query]
    }

    if {[info exists __caller_parameters]} {
      my process_query_parameter -all_from_query false -caller_parameters $__caller_parameters
    } else {
      my process_query_parameter -all_from_query false
    }
    my export_vars -level 2 
  }


  #
  # ConnectionContext, a context with user and url-specific information
  #

  Class ConnectionContext -superclass Context -parameter {
    user_id
    requestor
    user
    url
  }
  
  # TODO edit revision loop

  ConnectionContext proc require {
    -url
    {-package_id 0} 
    {-parameter ""}
    {-user_id -1}
    {-actual_query " "}
  } {
    if {![info exists url]} {
      #my log "--CONN ns_conn url"
      set url [ns_conn url]
    }
    #my log "--i [self args] URL='$url', pkg=$package_id"

    # create connection context if necessary
    if {$package_id == 0} {
      array set "" [site_node::get_from_url -url $url]
      set package_id $(package_id)
      #my log "--i setting pkg tp $package_id"
    } 

    # get locale; TODO at some time, we should get rid of the ad_conn init problem
    if {[ns_conn isconnected]} {
      # This can be called, before ad_conn is initialized. 
      # Since it is not possible to pass the user_id and ad_conn barfs
      # when  it tries to detect it, we use the catch and reset it later
      if {[catch {set locale [lang::conn::locale -package_id $package_id]}]} {
        set locale en_US
      }
    } else {
      set locale [lang::system::locale -package_id $package_id]
    }

    if {![my isobject ::xo::cc]} {
      my create ::xo::cc \
          -package_id $package_id \
          [list -parameter_declaration $parameter] \
	  -user_id $user_id \
	  -actual_query $actual_query \
          -locale $locale \
          -url $url
      #::xo::show_stack
      #my log "--cc ::xo::cc created $url [::xo::cc serialize]"
      ::xo::cc destroy_on_cleanup
    } else {
      #my log "--cc ::xo::cc reused $url -package_id $package_id"
      ::xo::cc configure \
          -url $url \
	  -actual_query $actual_query \
          -locale $locale \
          [list -parameter_declaration $parameter]
      #if {$package_id ne ""} {
      #  ::xo::cc package_id $package_id 
      #}
      ::xo::cc package_id $package_id 
      ::xo::cc set_user_id $user_id
      ::xo::cc process_query_parameter
    }
  }
  ConnectionContext instproc set_user_id {user_id} {
    if {$user_id == -1} {  ;# not specified
      if {[info exists ::ad_conn(user_id)]} {
        my set user_id [ad_conn user_id]
        if {[catch {my set untrusted_user_id [ad_conn untrusted_user_id]}]} {
          my set untrusted_user_id [my user_id]
        }
      } else {
        my set user_id 0
        my set untrusted_user_id 0
      }
    } else {
      my set user_id $user_id
      my set untrusted_user_id $user_id
    }
  }

  ConnectionContext instproc returnredirect {url} {
    #my log "--rp"
    my set __continuation [list ad_returnredirect $url]
    return ""
  }

  ConnectionContext instproc init {} {
    my instvar requestor user user_id
    my set_user_id $user_id
    set pa [expr {[ns_conn isconnected] ? [ad_conn peeraddr] : "nowhere"}]

    if {[my user_id] != 0} {
      set requestor $user_id
    } else {
      # for requests bypassing the ordinary connection setup (resources in oacs 5.2+)
      # we have to get the user_id by ourselves
      if { [catch {
        if {[info command ad_cookie] ne ""} {
          # we have the xotcl-based cookie code
          set cookie_list [ad_cookie get_signed_with_expr "ad_session_id"]
        } else {
          set cookie_list [ad_get_signed_cookie_with_expr "ad_session_id"]
        }
        set cookie_data [split [lindex $cookie_list 0] {,}]
        set untrusted_user_id [lindex $cookie_data 1]
        set requestor $untrusted_user_id
      } errmsg] } {
        set requestor 0
      }
    }
    
    # if user not authorized, use peer address as requestor key
    if {$requestor == 0} {
      set requestor $pa
      set user "client from $pa"
    } else {
      set user "<a href='/acs-admin/users/one?user_id=$requestor'>$requestor</a>"
    }
    #my log "--i requestor = $requestor"
    
    my process_query_parameter
  }

  ConnectionContext instproc cache {cmd} {
    set key cache($cmd)
    if {![my exists $key]} {my set $key [my uplevel $cmd]}
    return [my set $key]
  }
  ConnectionContext instproc cache_exists {cmd} {
    return [my exists cache($cmd)]
  }
  ConnectionContext instproc cache_get {cmd} {
    return [my set cache($cmd)]
  }
  ConnectionContext instproc cache_set {cmd value} {
    return [my set cache($cmd) $value]
  }
  ConnectionContext instproc cache_unset {cmd} {
    return [my unset cache($cmd)]
  }

  ConnectionContext instproc role=all {-user_id:required -package_id} {
    return 1
  }
  ConnectionContext instproc role=swa {-user_id:required -package_id} {
    return [my cache [list acs_user::site_wide_admin_p -user_id $user_id]]
  }
  ConnectionContext instproc role=registered_user {-user_id:required -package_id} {
    return [expr {$user_id != 0}]
  }
  ConnectionContext instproc role=unregistered_user {-user_id:required -package_id} {
    return [expr {$user_id == 0}]
  }
  ConnectionContext instproc role=admin {-user_id:required -package_id:required} {
    return [my permission -object_id $package_id -privilege admin -party_id $user_id]
  }
  ConnectionContext instproc role=creator {-user_id:required -package_id -object:required} {
    $object instvar creation_user
    return [expr {$creation_user == $user_id}]
  }
  ConnectionContext instproc role=app_group_member {-user_id:required -package_id} {
    return [my cache [list application_group::contains_party_p \
                          -party_id $user_id \
                          -package_id $package_id]]
  }
  ConnectionContext instproc role=community_member {-user_id:required -package_id} {
    if {[info command ::dotlrn_community::get_community_id] ne ""} {
      set community_id [my cache [list [dotlrn_community::get_community_id -package_id $package_id]]]
      if {$community_id ne ""} {
        return [my cache [list dotlrn::user_is_community_member_p \
                              -user_id $user_id \
                              -community_id $community_id]]
      }
    }
    return 0
  }

  ConnectionContext ad_instproc permission {-object_id:required -privilege:required -party_id } {
    call ::permission::permission_p but avoid multiple calls in the same
    session through caching in the connection context
  } {
    if {![info exists party_id]} {
      set party_id [my user_id]
    }
    # my log "--  context permission user_id=$party_id uid=[::xo::cc user_id] untrusted=[::xo::cc set untrusted_user_id]"
    if {$party_id == 0} {
      set key permission($object_id,$privilege,$party_id)
      if {[my exists $key]} {return [my set $key]}
      set granted [permission::permission_p -no_login -party_id $party_id \
                       -object_id $object_id \
                       -privilege $privilege]
      #my msg "--p lookup $key ==> $granted uid=[my user_id] uuid=[my set untrusted_user_id]"
      if {$granted || [my user_id] == [my set untrusted_user_id]} {
        my set $key $granted
        return $granted
      }
      # The permission is not granted for the public.
      # We force the user to login
      #my log "-- require login"
      #auth::require_login
      return 0
    }

    set key permission($object_id,$privilege,$party_id)
    if {[my exists $key]} {return [my set $key]}
    #my msg "--p lookup $key"
    my set $key [permission::permission_p -no_login \
                     -party_id $party_id \
                     -object_id $object_id \
                     -privilege $privilege]
    #my log "--  context return [my set $key]"
    #my set $key
  }
  
#   ConnectionContext instproc destroy {} {
#     my log "--i destroy [my url]"
#     #::xo::show_stack
#     next
#   }


  ConnectionContext instproc load_form_parameter {} {
    my instvar form_parameter
    if {[ns_conn isconnected]} {
      #array set form_parameter [ns_set array [ns_getform]]
      foreach {att value} [ns_set array [ns_getform]] {
        # For some unknown reasons, Safari 3.* returns sometimes
        # entries with empty names... We ignore these for now
        if {$att eq ""} continue
        if {[info exists form_parameter($att)]} {
          my set form_parameter_multiple($att) 1
        }
        lappend form_parameter($att) $value
      }
    } else {
      array set form_parameter {}
    }
  }
  ConnectionContext instproc form_parameter {name {default ""}} {
    my instvar form_parameter form_parameter_multiple
    if {![info exists form_parameter]} {
      my load_form_parameter
    }
    if {[info exists form_parameter($name)]} {
      if {[info exists form_parameter_multiple($name)]} {
        return $form_parameter($name)
      } else {
        return [lindex $form_parameter($name) 0]
      }
    } else {
      return $default
    }
  }
  ConnectionContext instproc exists_form_parameter {name} {
    my instvar form_parameter
    if {![info exists form_parameter]} {
      my load_form_parameter
    }
    my exists form_parameter($name)
  }
  ConnectionContext instproc get_all_form_parameter {} {
    return [my array get form_parameter]
  }
  
  ConnectionContext instproc set_parameter {name value} {
    set key [list get_parameter $name]
    if {[my cache_exists $key]} {my cache_delete $key}
    my set perconnectionparam($name) $value
  }
  ConnectionContext instproc get_parameter {name {default ""}} {
    my instvar perconnectionparam
    return [expr {[info exists perconnectionparam($name)] ? $perconnectionparam($name) : $default}]
  }
  ConnectionContext instproc exists_parameter {name} {
    my exists perconnectionparam($name)
  }

}

namespace eval ::xo {
  
  proc ::xo::update_query_variable {old_query var value} {
    #
    # replace in a url-query old occurances of var with new value.
    #
    set query [list [list $var $value]]
    foreach pair [split $old_query &] {
      foreach {key value} [split $pair =] break
      if {$key eq $var} continue
      lappend query [list [ns_urldecode $key] [ns_urldecode $value]]
    }
    return $query
  }

}