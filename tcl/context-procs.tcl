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
    set declared_parameters [lmap v ${:parameter_declaration} {
      string range [lindex [split [lindex $v 0] :] 0] 1 end
    }]

    if {${:actual_query} eq " "} {
      if {[ns_conn isconnected]} {
        set :actual_query [ns_conn query]
      }
      #:log "--P actual_query <${:actual_query}> url [ns_conn url] q [ns_conn query]"
    }
    set passed_args ""
    #:log "--P processing actual query '${:actual_query}'"
    try {
      set paramset [ns_parsequery ${:actual_query}]
      foreach {att_name att_value} [ns_set array $paramset] {
        if {$att_name eq ""} continue
        if {$att_name in $declared_parameters} {
          dict lappend passed_args -$att_name $att_value
        } elseif {$all_from_query} {
          set :queryparm($att_name) $att_value
        }
      }
    } on error {errorMsg} {
      ad_log warning "process_query_parameter: $errorMsg"
      ad_return_complaint 1 "invalid characters in HTTP query parameters"
    }

    # get the query parameters (from the form if necessary)
    if {[:istype ::xo::ConnectionContext]} {
      foreach name $declared_parameters {
        set param -$name
        #:log "--cc check $param [dict exists $passed_args $param]"
        if {![dict exists $passed_args $param]
            && [:exists_form_parameter $name]
          } {
          #:log "--cc adding passed_args(-$name) [:form_parameter $name]"
          dict set passed_args $param [:form_parameter $name]
        }
      }
    }

    # get the caller parameters (e.g. from the includelet call)
    if {[info exists caller_parameters]} {
      #:log "--cc caller_parameters=$caller_parameters"

      foreach param [dict keys $caller_parameters] {
        set name [string range $param 1 end]
        if {$name in $declared_parameters} {
          dict set passed_args $param [dict get $caller_parameters $param]
        } elseif {$all_from_caller} {
          set :queryparm($name) [dict get $caller_parameters $param]
          lappend declared_parameters $name
        }
      }
    }

    if {[::acs::icanuse "nsf::parseargs -asdict"]} {
      # OLD {64.347249 microseconds per iteration}
      # NEW {17.132942 microseconds per iteration}
      try {
        foreach {k v} [nsf::parseargs -asdict ${:parameter_declaration} $passed_args] {
          set :queryparm($k) $v
        }
      } on error {errorMsg} {
        ad_return_complaint 1 [ns_quotehtml $errorMsg]
        ad_script_abort
      }
    } else {
      #:log "--cc calling parser eval [self] __parse <${:parameter_declaration}> <$passed_args>"

      :proc __parse ${:parameter_declaration} {
        foreach v [info vars] {
          :log "--cc uplevel [list set :queryparm($v) [set $v]]"
          uplevel [list set :queryparm($v) [set $v]]
        }
      }

      if {[catch {[self] __parse {*}$passed_args} errorMsg]} {
        ad_return_complaint 1 [ns_quotehtml $errorMsg]
        ad_script_abort
      }
    }
    set :declared_parameters $declared_parameters
    #:log "--cc qp [array get :queryparm] // ${:actual_query}"
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

  Context instproc unset_query_parameter {name} {
    unset -nocomplain :queryparm($name)
  }

  Context instproc set_query_parameter {name value} {
    set :queryparm($name) $value
  }

  Context ad_instproc export_vars {-all:switch {-level 1}} {

    Export either the declared query variables (default) or all (when
    explicitly demanded).

    @param all when specified, export all query variables
    @param level target level
  } {

    if {$all} {
      foreach p [array names :queryparm] {
        regsub -all : $p _ varName
        uplevel $level [list set $varName [set :queryparm($p)]]
      }
    } else {
      #
      # Export only declared parameters (coming from the package
      # initialization or from the includelet definition).
      #
      foreach p [array names :queryparm] {
        if {$p in ${:declared_parameters}} {
          #ns_log notice "=== export <$p>"
          uplevel $level [list set $p [set :queryparm($p)]]
        }
      }
    }
    #
    # Set always variable package_id
    #
    uplevel $level [list set package_id ${:package_id}]
  }


  Context ad_instproc get_parameters {} {
    Convenience routine for includelets. It combines the actual
    parameters from the call in the page (highest priority) with
    the values from the url (second priority) and the default
    values from the signature.
  } {
    set source [expr {[info exists :__caller_parameters]
                      ? [self] : [:info parent]}]
    $source instvar __caller_parameters

    #set n [expr {[info exists :name] ? ${:name} : "NONE"}]
    #ns_log notice "$n: GET PARAMETERS source <$source> have [info exists __caller_parameters]"
    if {![info exists :__including_page]} {
      #
      # An includelet is called from the top-level. The actual_query
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
    requester
    user
    url
    mobile
  }

  ConnectionContext proc require_package_id_from_url {{-package_id 0} url} {
    #
    # Get package_id from URL in case it is not known. In case, the
    # package_id is known, this method is essentially a no-op, but
    # takes care about ::ad_conn initialization.
    #
    if {$package_id == 0} {
      set node_info [site_node::get_from_url -url $url]
      set package_id [dict get $node_info package_id]
    }
    if {![info exists ::ad_conn(node_id)] && [info exists node_info]} {
      #
      # The following should not be necessary, but is here for
      # cases, where some oacs-code assumes wrongly it is running in a
      # connection thread (e.g. the site master requires to have a
      # node_id and a URL accessible via ad_conn)
      #
      if {![dict exists $node_info node_id]} {
        if {$url eq ""} {
          set url [lindex [site_node::get_url_from_object_id -object_id $package_id] 0]
        }
        set node_info [site_node::get_from_url -url $url]
      }
      set ::ad_conn(node_id) [dict get $node_info node_id]
      set ::ad_conn(url) $url
      set ::ad_conn(extra_url) [string range $url [string length [dict get $node_info url]] end]
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
    #
    # This is a private method used for low-level connection context
    # creation. This function has to be called either with a valid
    # "-url" when being used outside connection threads.
    #
    set exists_cc [nsf::is object ::xo::cc]

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
      if {[ns_conn isconnected]} {
        set url [ad_conn url]
      } else {
        set url ""
        ad_log error "fallback to empty url"
      }
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
        ns_log warning "fall back to locale en_US"
        set locale en_US
      }
    } else {
      set locale [lang::system::locale -package_id $package_id]
    }
    if {!$exists_cc} {
      try {
        :create ::xo::cc \
            -package_id $package_id \
            -parameter_declaration $parameter \
            -user_id $user_id \
            -actual_query $actual_query \
            -locale $locale \
            -url $url
      } on error {errorMsg} {
        if {[nsf::is object ::xo::cc]} {
          ::xo::cc destroy
        }
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo $errorMsg
      }
      ::xo::cc destroy_on_cleanup

      # if {[ns_conn isconnected]} {
      #   ns_log notice "XXX ::xo::cc created [ns_conn id] [ns_conn request]"
      #   ::xo::cc set ID [ns_conn id]
      # } else {
      #   ns_log notice "XXX ::xo::cc created without connection"
      #   ::xo::cc set ID UNKNOWN
      # }
      # ::xo::cc proc destroy {args} {
      #   set ID [expr {[info exists :ID] ? ${:ID} : {-}}]
      #   ns_log notice "::xo::cc destroyed ID $ID"
      #   next
      # }

      #::xo::show_stack
      #:msg "--cc ::xo::cc created $url [::xo::cc serialize]"

    } else {
      #:msg "--cc ::xo::cc reused $url -package_id $package_id"
      ::xo::cc configure \
          -url $url \
          -actual_query $actual_query \
          -locale $locale \
          -parameter_declaration $parameter

      ::xo::cc package_id $package_id
      ::xo::cc set_user_id $user_id
      ::xo::cc process_query_parameter
    }

    # simple mobile detection
    ::xo::cc mobile 0
    if {[ns_conn isconnected]} {
      set user_agent [string tolower [ns_set iget [ns_conn headers] User-Agent]]
      ::xo::cc mobile [regexp (android|webos|iphone|ipad) $user_agent]
    }

    if {![info exists ::ad_conn(charset)]} {
      set ::ad_conn(charset) [lang::util::charset_for_locale $locale]
      set ::ad_conn(language) [::xo::cc lang]
      set ::ad_conn(file) ""
    }
  }

  ConnectionContext instproc requestor {} {
    #
    # Helper method to ease migration to the name without the spelling
    # error.
    #
    ad_log_deprecated method "... requestor" "... requester"
    return [expr {[info exists :requester] ? ${:requester} : ${:requester}}]
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
    return ${:user_id}
  }
  ConnectionContext ad_instproc eval_as_user {-user_id:integer cmd} {

    Run a command as the specified different user. Essentially, this
    method updates xo::cc and the ad_conn array array with the
    specified user, runs the command and resets the user to the
    previous value.

    @param user_id switch temporarily to this user
    @param cmd command to be exevuted

  } {
    #ns_log notice "RUN AS USER $user_id $cmd"
    set result ""
    set current_user_id [:get_user_id]
    try  {
      :set_user_id $user_id
      :uplevel $cmd
    } on ok {r} {
      set result $r
    } finally {
      :set_user_id $current_user_id
    }
    return $result
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
      set :requester ${:user_id}
    } else {
      #
      # For requests bypassing the ordinary connection setup
      # (resources in oacs 5.2+) we have to get the user_id by
      # ourselves.
      #
      ad_try {
        set cookie_list [ad_get_signed_cookie_with_expr "ad_session_id"]
        set cookie_data [split [lindex $cookie_list 0] {,}]
        set untrusted_user_id [lindex $cookie_data 1]
        set :requester $untrusted_user_id
      } on error {errorMsg } {
        set :requester 0
      }
    }

    # if user not authorized, use peer address as requester key
    if {${:requester} == 0} {
      set :requester $pa
      set :user "client from $pa"
    } else {
      set user_url [acs_community_member_admin_url -user_id ${:requester}]
      set :user "<a href='$user_url'>${:requester}</a>"
    }
    #:log "--i requester = ${:requester}"

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

  ConnectionContext ad_instproc permission {
    -object_id:integer,required
    -privilege:required
    -party_id:integer
  } {
    Call ::permission::permission_p but avoid multiple calls in the same
    request through caching in the connection context
  } {
    if {![info exists party_id]} {
      set party_id ${:user_id}
    }
    # :log "--  context permission user_id=$party_id uid=[::xo::cc user_id]" \
        "untrusted=[::xo::cc set untrusted_user_id]"
    if {$party_id == 0} {
      set granted [permission::permission_p -no_login -party_id $party_id \
                       -object_id $object_id \
                       -privilege $privilege]
      #:msg "--p lookup $key ==> $granted uid=${:user_id} uuid=${:untrusted_user_id}"
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

  ConnectionContext instproc require_form_parameter {} {
    if {![info exists :form_parameter]} {
      :load_form_parameter
    }
  }

  ConnectionContext instproc form_parameter {spec {default ""}} {
    :require_form_parameter

    set name $spec
    regexp {^([^:]+):(.*)$} $spec . name constraint

    if {[info exists :form_parameter($name)]} {
      if {[info exists :form_parameter_multiple($name)]} {
        set value [set :form_parameter($name)]
      } else {
        set value [lindex [set :form_parameter($name)] 0]
      }
      if {[info exists constraint]} {
        set r [xo::validate_parameter_constraints $name $constraint $value]
        if {$r ne $value} {
          ns_log notice "converting value checker: form parameter validate <$spec> -> '$value' -> '$r'"
          set value $r
        }
      } else {
        #:msg "FORM_PARAMETER spec <$spec> no constraint -> '$value'"
      }
      return $value
    } else {
      return $default
    }
  }
  ConnectionContext instproc exists_form_parameter {name} {
    :require_form_parameter
    info exists :form_parameter($name)
  }
  ConnectionContext instproc get_all_form_parameter {} {
    :require_form_parameter
    return [array get :form_parameter]
  }

  #
  # Version of query_parameter respecting set-parameter
  #
  ConnectionContext ad_instproc query_parameter {__spec {default ""}} {

    Get query parameter with default and optional value constraints.
    In case the value check for the query parameter fails, and no
    further precautions are performed (::aa_test_noabort is set), the
    method raises an exception with "ad_return_complaint" and aborts
    the script.

    @param __spec has the formname or name:value_constraint
    @param default default value
    @return actual value of the query parameter
  } {
    #
    # Try to split up provided "__spec" argument into name and
    # value constraint components.
    #
    set __name $__spec
    regexp {^([^:]+):(.*)$} $__spec . __name constraint

    if {[:exists_parameter $__name]} {
      set value [:get_parameter $__name]
    } else {
      set value [next $__name $default]
    }
    #
    # If we have a value-constraint, we check for empty values only in
    # cases, where multiplicity is specified. This means effectively
    # that the default multiplicity is "0..1".
    #
    if {[info exists constraint]} {
      set r [xo::validate_parameter_constraints $__name $constraint $value]
      if {$r ne $value} {
        ns_log notice "converting value checker: query parameter <$__spec> -> '$value' -> '$r'"
        set value $r
      }
    }
    return $value
  }

  ConnectionContext instproc set_parameter {name value} {
    set key [list get_parameter $name]
    if {[:cache_exists $key]} {:cache_unset $key}
    set :perconnectionparam($name) $value
  }
  ConnectionContext instproc unset_parameter {name} {
    set key [list get_parameter $name]
    if {[:cache_exists $key]} {:cache_unset $key}
    unset -nocomplain :perconnectionparam($name)
  }
  ConnectionContext instproc get_parameter {name {default ""}} {
    return [expr {[info exists :perconnectionparam($name)]
                  ? [set :perconnectionparam($name)]
                  : $default}]
  }
  ConnectionContext instproc exists_parameter {name} {
    info exists :perconnectionparam($name)
  }
  ConnectionContext instproc perconnection_parameter_get_all {} {
    array get :perconnectionparam
  }
  ConnectionContext instproc perconnection_parameter_set_all {pairs} {
    unset -nocomplain :perconnectionparam
    array set :perconnectionparam $pairs
  }
}

namespace eval ::xo {

  ad_proc -private ::xo::update_query_variable {old_query var value} {

    Replace in a URL-query old occurrences of var with new value.

    @return pairs in a form suitable for export_vars
  } {
    set query [list [list $var $value]]
    foreach {key value} [ns_set array [ns_parsequery $old_query]] {
      if {$key eq $var
          || [::util::suspicious_query_variable -proc xo::update_query $key $value]} {
        continue
      }
      lappend query [list $key $value]
    }
    return $query
  }

  ad_proc -private ::xo::update_query {old_query var value} {

    Replace in a URL-query old occurrences of var with new value.

    @return encoded HTTP query
  } {
    set encodeCmd ns_urlencode
    if {$::xo::naviserver} {lappend encodeCmd --}

    set query [{*}$encodeCmd $var]=[{*}$encodeCmd $value]

    if {$old_query ne ""} {
      foreach {key value} [ns_set array [ns_parsequery $old_query]] {
        if {$key eq $var
            || [::util::suspicious_query_variable -proc xo::update_query $key $value]} {
          continue
        }
        append query &[{*}$encodeCmd $key]=[{*}$encodeCmd $value]
      }
    }
    return $query
  }

  ad_proc ::xo::validate_parameter_constraints {name constraint value} {

    Validate the provided value against the constraints.  In case of
    failure, return with ad_return_complaint when there is a
    connection, otherwise raise an error.

  } {
    #
    # If we have a value-constraint, we check for empty values only in
    # cases, where multiplicity is specified. This means effectively
    # that the default multiplicity is "0..1".
    #
    #ns_log notice "::xo::validate_parameter_constraints $name $constraint input '$value'"
    if {[string first . $constraint] > -1 || $value ne ""} {
      try {
        #
        # Use parseargs with "-asdict" option when it is available,
        # since it does not globber the variable namespace. For legacy
        # applications, lets hope that no query parameter named
        # "__name" is used with a value constraint.
        #
        if {[::acs::icanuse "nsf::parseargs -asdict"]} {
          #
          # Newer versions will use this branch
          #
          set value [dict get \
                         [nsf::parseargs -asdict ${name}:$constraint [list $value]] \
                         $name]
        } else {
          #
          # This is the legacy branch.  nsf::parseargs might clobber
          # "name", therefore, save it in an highly unlikely variable
          # name.
          #
          set { name } $name
          nsf::parseargs ${name}:$constraint [list $value]
          set value [set ${ name }]
        }
      } on error {errorMsg} {
        #ns_log notice ".... nsf::parseargs error '$errorMsg'"
        if {[ns_conn isconnected] && ![info exists ::aa_test_noabort]} {
          ad_return_complaint 1 [ns_quotehtml $errorMsg]
          ad_script_abort
        } else {
          throw $::errorInfo $errorMsg
        }
      }
    }
    #ns_log notice "::xo::validate_parameter_constraints $name $constraint -> '$value'"
    return $value
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
