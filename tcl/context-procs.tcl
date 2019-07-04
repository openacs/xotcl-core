xo::library doc  {
  Context handling interface
  
  Definition of a connection context, containing user info, URLs, parameters.
  this is used via "Package initialize"... similar as page_contracts and
  for included content (includelets), and used for per-connection caching as well.
  The intention is similar as with ad_conn, but based on objects.
  So far, it is pretty simple, but should get more clever in the future.

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @creation-date 2006-08-06
  @cvs-id $Id$
}

namespace eval ::xo {

  ::xotcl::Class create Context -ad_doc {
    This class provides a context for evaluation, somewhat similar to an 
    activation record in programming languages. It combines the parameter
    declaration (e.g. of a page, an includelet) with the actual parameters
    (specified in an includelet) and the provided query values (from the url).
    The parameter decaration are actually XOTcl's non positional arguments.
  } -parameter {
    {parameter_declaration ""} 
    {actual_query " "}
    {package_id 0}
    {invoke_object}
    locale
  }

  #
  # Syntactic sugar for includelets, to allow the same syntax as 
  # for "Package initialize ...."; however, we do not allow currently
  # do switch user or package id etc., just the parameter declaration
  Context instproc initialize {{-parameter ""}} {
    set :parameter_declaration $parameter
  }

  Context instproc process_query_parameter {
    {-all_from_query:boolean true}
    {-all_from_caller:boolean true}
    {-caller_parameters}
  } {
    :proc __parse [:parameter_declaration]  {
      foreach v [info vars] { uplevel [list set :queryparm($v) [set $v]]}
    }
    
    foreach v [:parameter_declaration] {
      set ([lindex [split [lindex $v 0] :] 0]) 1
    }
    if {${:actual_query} eq " "} {
      if {[ns_conn isconnected]} {
        set :actual_query [ns_conn query]
      }
      #:log "--CONN ns_conn query = <$actual_query>"
    }

    set decodeCmd ns_urldecode
    if {$::xo::naviserver} {lappend decodeCmd --}

    # get the query parameters (from the url)
    #:log "--P processing actual query ${:actual_query}"
    foreach querypart [split ${:actual_query} &] {
      set name_value_pair [split $querypart =]
      set att_name [{*}$decodeCmd [lindex $name_value_pair 0]]
      if {$att_name eq ""} continue
      if {[llength $name_value_pair] == 1} { 
        set att_value 1 
      }  else {
        set att_value [{*}$decodeCmd [lindex $name_value_pair 1]]
      }
      if {[info exists (-$att_name)]} {
        lappend passed_args(-$att_name) $att_value
      } elseif {$all_from_query} {
        set :queryparm($att_name) $att_value
      }
    }

    # get the query parameters (from the form if necessary)
    if {[:istype ::xo::ConnectionContext]} {
      foreach param [array names ""] {
        #:log "--cc check $param [info exists passed_args($param)]"
        set name [string range $param 1 end]
        if {![info exists passed_args($param)] &&
            [:exists_form_parameter $name]} {
          #:log "--cc adding passed_args(-$name) [:form_parameter $name]"
          set passed_args($param) [:form_parameter $name]
        }
      }
    }
    
    # get the caller parameters (e.g. from the includelet call)
    if {[info exists caller_parameters]} {
      #:log "--cc caller_parameters=$caller_parameters"
      array set caller_param $caller_parameters
      
      foreach param [array names caller_param] {
        if {[info exists ($param)]} { 
          set passed_args($param) $caller_param($param) 
        } elseif {$all_from_caller} {
          set :queryparm([string range $param 1 end]) $caller_param($param) 
        }
      }
    }

    set parse_args [list]
    foreach param [array names passed_args] {
      lappend parse_args $param $passed_args($param)
    }
    
    #:log "--cc calling parser eval [self] __parse $parse_args"
    if {[catch {[self] __parse {*}$parse_args} errorMsg]} {
      ad_return_complaint 1 [ns_quotehtml $errorMsg]
      ad_script_abort
    }
    #:msg "--cc qp [array get :queryparm] // ${:actual_query}"
  }

  Context instproc original_url_and_query args {
    if {[llength $args] == 1} {
      set :original_url_and_query [lindex $args 0]
    } elseif {[info exists :original_url_and_query]} {
      return ${:original_url_and_query}
    } else {
      return ${:url}?${:actual_query}
    }
  }

  Context instproc query_parameter {name {default ""}} {
    if {[info exists :queryparm($name)]} {
      return [set :queryparm($name)]
    } 
    return $default
  }
  
  Context instproc exists_query_parameter {name} {
    #:log "--qp exists $name => [info exists :queryparm($name)]"
    info exists :queryparm($name)
  }
  Context instproc get_all_query_parameter {} {
    return [array get :queryparm]
  }

  Context ad_instproc export_vars {{-level 1}} {
    Export the query variables
    @param level target level
  } {
    foreach p [array names :queryparm] {
      regsub -all : $p _ varName
      uplevel $level [list set $varName [set :queryparm($p)]]
    }
    uplevel $level [list set package_id ${:package_id}]
    #::xo::show_stack
  }


  Context ad_instproc get_parameters {} {
    Convenience routine for includelets. It combines the actual
    parameters from the call in the page (highest priority) wit
    the values from the url (second priority) and the default
    values from the signature
  } {
    set source [expr {[info exists :__caller_parameters] ? 
                      [self] : [:info parent]}]
    $source instvar __caller_parameters
    
    if {![info exists :__including_page]} {
      #
      # An includelet is called from the toplevel. The actual_query
      # might be cached, so we reset it here.
      #
      set :actual_query [::xo::cc actual_query]
    }

    if {[info exists __caller_parameters]} {
      :process_query_parameter -all_from_query false -caller_parameters $__caller_parameters
    } else {
      :process_query_parameter -all_from_query false
    }
    :export_vars -level 2 
  }


  #
  # ConnectionContext, a context with user and url-specific information
  #

  Class create ConnectionContext -superclass Context -parameter {
    user_id
    requestor
    user
    url
    mobile
  }
  
  ConnectionContext proc require_package_id_from_url {{-package_id 0} url} {
    # get package_id from url in case it is not known
    if {$package_id == 0} {
      array set "" [site_node::get_from_url -url $url]
      set package_id $(package_id)
    }
    if {![info exists ::ad_conn(node_id)]} {
      # 
      # The following should not be necessary, but is here for
      # cases, where some oacs-code assumes wrongly it is running in a
      # connection thread (e.g. the site master requires to have a
      # node_id and a URL accessible via ad_conn)
      #
      if {![info exists (node_id)]} {
        if {$url eq ""} {
          set url [lindex [site_node::get_url_from_object_id -object_id $package_id] 0]
        }
        array set "" [site_node::get_from_url -url $url]
      }
      set ::ad_conn(node_id) $(node_id)
      set ::ad_conn(url) $url
      set ::ad_conn(extra_url) [string range $url [string length $(url)] end]
    }
    return $package_id
  }

  ConnectionContext proc require {
                                  -url
                                  {-package_id 0} 
                                  {-parameter ""}
                                  {-user_id -1}
                                  {-actual_query " "}
                                  {-keep_cc false}
                                } {
    set exists_cc [:isobject ::xo::cc]

    # if we have a connection context and we want to keep it, do
    # nothing and return.
    if {$exists_cc && $keep_cc} {
      return
    }

    if {[info exists ::ds_show_p] && [ds_database_enabled_p]} {
      ::xo::dc profile on
    }

    if {![info exists url]} {
      #:log "--CONN ns_conn url"
      set url [ns_conn url]
    }
    set package_id [:require_package_id_from_url -package_id $package_id $url]
    #:log "--i [self args] URL='$url', pkg=$package_id"

    # get locale; TODO at some time, we should get rid of the ad_conn init problem
    if {[ns_conn isconnected]} {
      # This can be called, before ad_conn is initialized. 
      # Since it is not possible to pass the user_id and ad_conn barfs
      # when it tries to detect it, we try to get it and reset it later
      ad_try {
        set locale [lang::conn::locale -package_id $package_id]
      } on error {errorMsg} {
        set locale en_US
      }
    } else {
      set locale [lang::system::locale -package_id $package_id]
    }
    if {!$exists_cc} {
      :create ::xo::cc \
          -package_id $package_id \
          [list -parameter_declaration $parameter] \
          -user_id $user_id \
          -actual_query $actual_query \
          -locale $locale \
          -url $url
      #::xo::show_stack
      #:msg "--cc ::xo::cc created $url [::xo::cc serialize]"
      ::xo::cc destroy_on_cleanup
    } else {
      #:msg "--cc ::xo::cc reused $url -package_id $package_id"
      ::xo::cc configure \
          -url $url \
          -actual_query $actual_query \
          -locale $locale \
          [list -parameter_declaration $parameter]

      ::xo::cc package_id $package_id 
      ::xo::cc set_user_id $user_id
      ::xo::cc process_query_parameter
    }

    # simple mobile detection
    ::xo::cc mobile 0
    if {[ns_conn isconnected]} {
      set user_agent [string tolower [ns_set get [ns_conn headers] User-Agent]]
      ::xo::cc mobile [regexp (android|webos|iphone|ipad) $user_agent]
    }

    if {![info exists ::ad_conn(charset)]} {
      set ::ad_conn(charset) [lang::util::charset_for_locale $locale] 
      set ::ad_conn(language) [::xo::cc lang]
      set ::ad_conn(file) ""
    }
  }
  ConnectionContext instproc lang {} {
    return [string range [:locale] 0 1]
  }
  ConnectionContext instproc set_user_id {user_id} {
    if {$user_id == -1} {  ;# not specified
      if {[info exists ::ad_conn(user_id)]} {
        set :user_id [ad_conn user_id]
        ad_try {
          set :untrusted_user_id [ad_conn untrusted_user_id]
        } on error {errorMsg} {
          set :untrusted_user_id ${:user_id}
        }
      } else {
        set :user_id 0
        set :untrusted_user_id 0
        array set ::ad_conn [list user_id $user_id untrusted_user_id $user_id session_id ""]
      }
    } else {
      set :user_id $user_id
      set :untrusted_user_id $user_id
      if {![info exists ::ad_conn(user_id)]} {
        array set ::ad_conn [list user_id $user_id untrusted_user_id $user_id session_id ""]
      }
    }
  }
  ConnectionContext instproc get_user_id {} {
    #
    # If the untrusted user_id exists, return it. This will return
    # consistently the user_id also in situations, where the login
    # cookie was expired. If no untrusted_user_id exists Otherwise
    # (maybe in a remoting setup), return the user_id.
    #
    if {[info exists :untrusted_user_id]} {
      return ${:untrusted_user_id}
    }
    return [:user_id]
  }

  ConnectionContext instproc returnredirect {-allow_complete_url:switch url} {
    #:log "--rp"
    set :__continuation [expr {$allow_complete_url 
                                 ? [list ad_returnredirect -allow_complete_url $url] 
                                 : [list ad_returnredirect $url]}]
    return ""
  }

  ConnectionContext instproc init {} {
    :set_user_id ${:user_id}
    set pa [expr {[ns_conn isconnected] ? [ad_conn peeraddr] : "nowhere"}]

    if {${:user_id} != 0} {
      set :requestor ${:user_id}
    } else {
      # for requests bypassing the ordinary connection setup (resources in oacs 5.2+)
      # we have to get the user_id by ourselves
      ad_try {
        set cookie_list [ad_get_signed_cookie_with_expr "ad_session_id"]
        set cookie_data [split [lindex $cookie_list 0] {,}]
        set untrusted_user_id [lindex $cookie_data 1]
        set :requestor $untrusted_user_id
      } on error {errorMsg } {
        set :requestor 0
      }
    }
    
    # if user not authorized, use peer address as requestor key
    if {${:requestor} == 0} {
      set :requestor $pa
      set :user "client from $pa"
    } else {
      set user_url [acs_community_member_admin_url -user_id ${:requestor}]
      set :user "<a href='$user_url'>${:requestor}</a>"
    }
    #:log "--i requestor = ${:requestor}"
    
    :process_query_parameter
  }

  ConnectionContext instproc cache {cmd} {
    set key :cache($cmd)
    if {![info exists $key]} {set $key [:uplevel $cmd]}
    return [set $key]
  }
  ConnectionContext instproc cache_exists {cmd} {
    return [info exists :cache($cmd)]
  }
  ConnectionContext instproc cache_get {cmd} {
    return [set :cache($cmd)]
  }
  ConnectionContext instproc cache_set {cmd value} {
    return [set :cache($cmd) $value]
  }
  ConnectionContext instproc cache_unset {cmd} {
    return [unset :cache($cmd)]
  }

  ConnectionContext instproc role=all {-user_id:required -package_id} {
    return 1
  }
  ConnectionContext instproc role=swa {-user_id:required -package_id} {
    return [:cache [list acs_user::site_wide_admin_p -user_id $user_id]]
  }
  ConnectionContext instproc role=registered_user {-user_id:required -package_id} {
    return [expr {$user_id != 0}]
  }
  ConnectionContext instproc role=unregistered_user {-user_id:required -package_id} {
    return [expr {$user_id == 0}]
  }
  ConnectionContext instproc role=admin {-user_id:required -package_id:required} {
    return [:permission -object_id $package_id -privilege admin -party_id $user_id]
  }
  ConnectionContext instproc role=creator {-user_id:required -package_id -object:required} {
    $object instvar creation_user
    return [expr {$creation_user == $user_id}]
  }
  ConnectionContext instproc role=app_group_member {-user_id:required -package_id} {
    return [:cache [list application_group::contains_party_p \
                        -party_id $user_id \
                        -package_id $package_id]]
  }
  ConnectionContext instproc role=community_member {-user_id:required -package_id} {
    if {[info commands ::dotlrn_community::get_community_id] ne ""} {
      set community_id [:cache [list [dotlrn_community::get_community_id -package_id $package_id]]]
      if {$community_id ne ""} {
        return [:cache [list dotlrn::user_is_community_member_p \
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
      set party_id ${:user_id}
    }
    # :log "--  context permission user_id=$party_id uid=[::xo::cc user_id] untrusted=[::xo::cc set untrusted_user_id]"
    if {$party_id == 0} {
      set granted [permission::permission_p -no_login -party_id $party_id \
                       -object_id $object_id \
                       -privilege $privilege]
      #:msg "--p lookup $key ==> $granted uid=[:user_id] uuid=${:untrusted_user_id}"
      if {$granted || ${:user_id} == ${:untrusted_user_id}} {
        return $granted
      }
      # The permission is not granted for the public.
      # We force the user to login
      #:log "-- require login"
      #auth::require_login
      return 0
    }
    
    #:msg "--p lookup $key"
    return [permission::permission_p -no_login \
                -party_id $party_id \
                -object_id $object_id \
                -privilege $privilege]
    #:log "--  context return [set :$key]"
    #set :$key
  }
  
  #   ConnectionContext instproc destroy {} {
  #     :log "--i destroy [:url]"
  #     #::xo::show_stack
  #     next
  #   }

  ConnectionContext instproc load_form_parameter_from_values {values} {
    foreach {att value} $values {
      # For some unknown reasons, Safari 3.* returns sometimes
      # entries with empty names... We ignore these for now
      if {$att eq ""} continue
      if {[info exists :form_parameter($att)]} {
        set :form_parameter_multiple($att) 1
      }
      lappend :form_parameter($att) $value
    }
  }

  ConnectionContext instproc load_form_parameter {} {
    if {[ns_conn isconnected] && [ns_conn method] eq "POST"} {
      :load_form_parameter_from_values [ns_set array [ns_getform]]
    } else {
      array set :form_parameter {}
    }
  }

  ConnectionContext instproc form_parameter {name {default ""}} {
    if {![info exists :form_parameter]} {
      :load_form_parameter
    }
    if {[info exists :form_parameter($name)]} {
      if {[info exists :form_parameter_multiple($name)]} {
        return [set :form_parameter($name)]
      } else {
        return [lindex [set :form_parameter($name)] 0]
      }
    } else {
      return $default
    }
  }
  ConnectionContext instproc exists_form_parameter {name} {
    if {![info exists :form_parameter]} {
      :load_form_parameter
    }
    info exists :form_parameter($name)
  }
  ConnectionContext instproc get_all_form_parameter {} {
    return [array get :form_parameter]
  }

  #
  # Version of query_parameter respecting set-parameter
  #
  ConnectionContext instproc query_parameter {name {default ""}} {
    if {[:exists_parameter $name]} {
      return [:get_parameter $name]
    }
    next
  }

  
  ConnectionContext instproc set_parameter {name value} {
    set key [list get_parameter $name]
    if {[:cache_exists $key]} {:cache_unset $key}
    set :perconnectionparam($name) $value
  }
  ConnectionContext instproc get_parameter {name {default ""}} {
    return [expr {[info exists :perconnectionparam($name)] ? [set :perconnectionparam($name)] : $default}]
  }
  ConnectionContext instproc exists_parameter {name} {
    info exists :perconnectionparam($name)
  }

}

namespace eval ::xo {
  
  proc ::xo::update_query_variable {old_query var value} {
    #
    # Replace in a URL-query old occurrences of var with new value.
    #
    # @return pairs in a form suitable for export_vars
    #
    set decodeCmd ns_urldecode
    if {$::xo::naviserver} {lappend decodeCmd --}

    set query [list [list $var $value]]
    foreach pair [split $old_query &] {
      lassign [split $pair =] key value
      if {$key eq $var} continue
      lappend query [list [{*}$decodeCmd $key] [{*}$decodeCmd $value]]
    }
    return $query
  }

  proc ::xo::update_query {old_query var value} {
    #
    # Replace in a URL-query old occurrences of var with new value.
    #
    # @return encoded HTTP query
    #
    set decodeCmd ns_urldecode
    set encodeCmd ns_urlencode
    if {$::xo::naviserver} {lappend decodeCmd --; lappend encodeCmd --}

    set query [{*}$encodeCmd $var]=[{*}$encodeCmd $value]
    foreach pair [split $old_query &] {
      lassign [split $pair =] key value
      if {[{*}$decodeCmd $key] eq $var} continue
      append query &$pair
    }
    return $query
  }

}

::xo::library source_dependent

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
#    eval: (setq tcl-type-alist (remove* "method" tcl-type-alist :test 'equal :key 'car))
# End:
