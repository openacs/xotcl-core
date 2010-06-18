ad_library {
  XOTcl API for policies 

  @author Gustaf Neumann
  @creation-date 2007-03-09
  @cvs-id $Id$
}

namespace eval ::xo {
  
  Class Policy

  Policy instproc defined_methods {class} {
    set c [self]::$class
    expr {[my isclass $c] ? [$c array names require_permission] : [list]}
  }
  
  Policy instproc check_privilege {
    {-login true} 
    -user_id:required 
    -package_id 
    privilege object method
  } {
    #my log "--p [self proc] [self args]"
    if {$privilege eq "nobody"} {
      return 0
    }
    if {$privilege eq "everybody" || $privilege eq "public" || $privilege eq "none"} {
      return 1
    }

    #my log "--login $login user_id=$user_id uid=[::xo::cc user_id] untrusted=[::xo::cc set untrusted_user_id]"
    if {$login && $user_id == 0} {
      #
      # The tests below depend on the user_id.
      # The main reason, we call auth:require_login here is to check for exired logins.
      #
      #my log "--p [self proc] calls require_login"
      set user_id [auth::require_login]
    }

    if {$privilege eq "login" || $privilege eq "registered_user"} {
      return [expr {$user_id != 0}]
    }

    if {[::xo::cc cache [list acs_user::site_wide_admin_p -user_id $user_id]]} {
      # swa is allowed to do everything handled below as well
      return 1
    } elseif {$privilege eq "swa"} {
      return 0
    }

    if {[::xo::cc permission -object_id $package_id -privilege admin -party_id $user_id]} {
      # package_admin is allowed to do everything handled below as well
      return 1
    } elseif {$privilege eq "admin"} {
      return 0
    }

    set allowed -1   ;# undecided
    # try object specific privileges. These have the signature:
    # 
    # <class> instproc privilege=<name> {{-login true} user_id package_id method}
    #
    if {[$object info methods privilege=$privilege] ne ""} {
      if {![info exists package_id]} {set package_id [::xo::cc package_id]}
      set allowed [$object privilege=$privilege -login $login $user_id $package_id $method]
    }
    #my msg "--check_privilege {$privilege $object $method} ==> $allowed"
    return $allowed
  }

  Policy instproc get_privilege {{-query_context "::xo::cc"} permission object method} {
    # the privilege might by primitive (one word privilege)
    # or it might be complex (attribute + privilege)
    # or it might be conditional (primitive or complex) in a list of privilges

    foreach p $permission {
      #my msg "checking permission '$p'"
      set condition [lindex $p 0]
      if {[llength $condition]>1} {
        # we have a condition
	foreach {cond value} $condition break
        if {[$object condition=$cond $query_context $value]} {
          return [my get_privilege [list [lrange $p 1 end]] $object $method]
        }
      } else {
        # we have no condition
        return [list [expr {[llength $p] == 1 ? "primitive" : "complex"}] $p]
      }
    }
    # In cases, where is no permission defined, or all conditions
    # fail, and no unconditional privilege is defined, reject access.
    # Maybe, we should search the class hierarchy up in the future.
    return [list primitive nobody]
  }

  Policy instproc get_permission {{-check_classes true} object method} {
    set permission ""
    set o [self]::[namespace tail $object]
    set key require_permission($method)
    if {[my isobject $o] && [$o exists $key]} {
      set permission [$o set $key]
    } elseif {[my isobject $o] && [$o exists default_permission]} {
      set permission [$o set default_permission]
    } elseif {$check_classes} {
      # we have no object specific policy information, check the classes
      set c [$object info class]
      foreach class [concat $c [$c info heritage]] {
	set c [self]::[namespace tail $class]
	if {![my isclass $c]} continue
	set permission [my get_permission -check_classes false $class $method]
	if {$permission ne ""} break
      }
    }
    return $permission
  }
  
  Policy ad_instproc check_permissions {-user_id -package_id {-link ""} object method} {

    This method checks whether the current user is allowed
    or not to invoke a method based on the given policy.
    This method is purely checking and does not force logins
    or other side effects. It can be safely used for example
    to check whether links should be shown or not.

    @see enforce_permissions
    @return 0 or 1
    
  } {
    if {![info exists user_id]} {set user_id [::xo::cc user_id]}
    if {![info exists package_id]} {set package_id [::xo::cc package_id]}
    #my msg [info exists package_id]=>$package_id-[my exists logical_package_id]
    set ctx ""
    if {$link ne ""} {
      set query [lindex [split $link ?] 1]
      set ctx [::xo::Context new -destroy_on_cleanup -actual_query $query]
      $ctx process_query_parameter
    }

    set permission [my get_permission $object $method]
    #my log "--permission for o=$object, m=$method => $permission"

    #my log "--     user_id=$user_id uid=[::xo::cc user_id] untrusted=[::xo::cc set untrusted_user_id]"
    if {$permission ne ""} {
      foreach {kind p} [my get_privilege -query_context $ctx $permission $object $method] break
      #my msg "--privilege = $p kind = $kind"
      switch -- $kind {
	primitive {return [my check_privilege -login false \
			       -package_id $package_id -user_id $user_id \
			       $p $object $method]}
	complex {
	  foreach {attribute privilege} $p break
	  set id [$object set $attribute]
	  #my msg "--p checking permission -object_id /$id/ -privilege $privilege -party_id $user_id\
	  #	==> [::xo::cc permission -object_id $id -privilege $privilege -party_id $user_id]"
	  return [::xo::cc permission -object_id $id -privilege $privilege -party_id $user_id]
	}
      }
    }
    return 0
  }

  Policy ad_instproc enforce_permissions {-user_id -package_id object method} {

    This method checks whether the current user is allowed
    or not to invoke a method based on the given policy and
    forces logins if required.

    @see check_permissions
    @return 0 or 1
    
  } {
    if {![info exists user_id]} {set user_id [::xo::cc user_id]}
    if {![info exists package_id]} {set package_id [::xo::cc package_id]}

    set allowed 0
    set permission [my get_permission $object $method]
    if {$permission ne ""} {
      foreach {kind p} [my get_privilege $permission $object $method] break
      switch -- $kind {
	primitive {
	  set allowed [my check_privilege \
			   -user_id $user_id -package_id $package_id \
			   $p $object $method]
	  set privilege $p
	}
	complex {
	  foreach {attribute privilege} $p break
	  set id [$object set $attribute]
	  set allowed [::xo::cc permission -object_id $id \
			   -privilege $privilege \
			   -party_id $user_id]
        }
      }
    }

    #my log "--p enforce_permissions {$object $method} : $permission ==> $allowed"

    if {!$allowed} {
      set untrusted_user_id [::xo::cc set untrusted_user_id]
      if {$permission eq ""} {
	ns_log notice "enforce_permissions: no permission for $object->$method defined"
      } elseif {$user_id == 0 && $untrusted_user_id} {
        ns_log notice "enforce_permissions: force login, user_id=0 and untrusted_id=$untrusted_user_id"
        auth::require_login
      } else {
	ns_log notice "enforce_permissions: $user_id doesn't have $privilege on $object"
      }
	ad_return_forbidden  "[_ xotcl-core.permission_denied]" [_ xotcl-core.policy-error-insufficient_permissions]
      ad_script_abort
    }
  
    return $allowed
  }

}