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

  Policy instproc check_privilege {{-login true} -user_id -package_id privilege object method} {
    set allowed -1   ;# undecided
    if {[acs_user::site_wide_admin_p -user_id $user_id]} {
      return 1
    }
    switch $privilege {
      none  {return 1}
      login {
        if {$login} {
          auth::require_login; return 1
        } else {
          return [expr {$user_id != 0}]
        }
      }
      swa   {
        set allowed 0
        #if {!$allowed} {
        #  ad_return_warning "Insufficient Permissions" \
        #      "Only side wide admins are allowed for this operation! ($object $method)"
        #  ad_script_abort
        #}
      }
      default {
        # try object specific privileges. These have the signature:
        # 
        # <class> instproc privilege=<name> {{-login true} user_id package_id method}
        #
        if {[$object info methods privilege=$privilege] ne ""} {
	  if {![info exists package_id]} {set package_id [::xo::cc package_id]}
          set allowed [$object privilege=$privilege -login $login $user_id $package_id $method]
        }
      }
    }
    #my log "--check_privilege {$privilege $object $method} ==> $allowed"
    return $allowed
  }

  Policy instproc get_privilege {permission object method} {
    # the privilege might by primitive (one word privilege)
    # or it might be complex (attribute + privilege)
    # or it might be conditional (primitive or complex) in a list of privilges

    foreach p $permission {
      
      set condition [lindex $p 0]
      if {[llength $condition]>1} {
        # we have a condition
	foreach {cond value} $condition break
        if {[$object condition=$cond $value]} {
          return [my get_privilege [lrange $p 1 end] $object $method]
        } 
      } else {
        # we have no condition
        return [list [expr {[llength $p] == 1 ? "primitive" : "complex"}] $p]
      }
    }
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
  
  Policy ad_instproc check_permissions {-user_id -package_id object method} {

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

    set permission [my get_permission $object $method]
    if {$permission ne ""} {
      foreach {kind p} [my get_privilege $permission $object $method] break
      switch $kind {
	primitive {return [my check_privilege -login false \
			       -package_id $package_id -user_id $user_id \
			       $p $object $method]}
	complex {
	  foreach {attribute privilege} $p break
	  set id [$object set $attribute]
	  #my log "--p checking permission::permission_p -object_id $id -privilege $privilege"
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

    #my log "--p enforce_permissions {$object $method}"
    set allowed 0
    set permission [my get_permission $object $method]
    if {$permission ne ""} {
      foreach {kind p} [my get_privilege $permission $object $method] break
      switch $kind {
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
      ns_log notice "permission::require_permission: $user_id doesn't \
		have $privilege on $object"
      ad_return_forbidden  "Permission Denied"  "<blockquote>
  You don't have sufficient permissions for $method on this object ($object).
</blockquote>"
      ad_script_abort
    }
  
    return $allowed
  }

}