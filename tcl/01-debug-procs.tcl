## tell serializer to export methods, although these are methods of 
# ::xotcl::Object

package require xotcl::serializer

::Serializer exportMethods {
  ::xotcl::Object instproc log 
  ::xotcl::Object instproc msg
  ::xotcl::Object instproc __timediff
  ::xotcl::Object instproc debug
  ::xotcl::Object instproc qn
  ::xotcl::Object instproc serialize
  ::xotcl::Object instforward db_1row
  ::xotcl::Object instproc destroy_on_cleanup
  ::xotcl::nonposArgs proc integer
  ::xotcl::nonposArgs proc optional
}

if {$::xotcl::version < 1.5} {
  # XOTcl 1.5 comes already with a predefined, more powerful
  # implementation of contains.

  ::Serializer exportMethods {
    ::xotcl::Object instproc contains
  }
  ::xotcl::Object instproc contains cmds {
    my requireNamespace
    namespace eval [self] $cmds
  }
  # XOTcl 1.5 or newer supports slots. Here we have to 
  # emulate slots up to a certain point
  namespace eval ::xo {
    Class create ::xo::Attribute \
      -parameter {
        {name "[namespace tail [::xotcl::self]]"}
        {domain "[lindex [regexp -inline {^(.*)::slot::[^:]+$} [::xotcl::self]] 1]"}
        {multivalued false}
        {required false}
        default 
        type
        spec
        pretty_name 
        {pretty_plural ""}
        {datatype "text"} 
        constraint_values
        help_text 
        validator
      }
  }
} else {
  namespace eval ::xo {
    # create xo::Attribute as a subclass of the slot ::xotcl::Attribute
    Class create ::xo::Attribute \
        -superclass ::xotcl::Attribute \
        -parameter {
          spec
          {required false}
          pretty_name 
          {pretty_plural ""}
          {datatype "text"}
          constraint_values
          help_text 
          validator
        }
  }
}

namespace eval ::xo {
  ::xo::Attribute instproc init {} {
    my instvar name pretty_name
    next
    # provide a default pretty name for the attribute based on message keys
    if {![info exists pretty_name]} {
      set object_type [my domain] 
      if {[regexp {^::([^:]+)::} $object_type _ head]} {
	set tail [namespace tail $object_type]
	set pretty_name "#$head.$tail-$name#"
	#my log "--created pretty_name = $pretty_name"
      } else {
	error "Cannot determine automatically message key for pretty name. \
		Use namespaces for classes"
      }
    }
  }

  proc package_id_from_package_key { key } {
    return [db_string dbqd.null.get_package_id_from_key \
                {select package_id from apm_packages where package_key = :key}]
  }
}

::xotcl::Object instforward db_1row -objscope

::xotcl::Object instproc serialize {} {
  ::Serializer deepSerialize [self]
}

namespace eval ::xo {
  proc slotobjects cl {
    set so [list]
    array set names ""
    foreach c [concat $cl [$cl info heritage]] {
      foreach s [$c info slots] {
	set n [namespace tail $s]
	if {![info exists names($n)]} {
	  lappend so $s
	  set names($n) $s
	}
      }
    }
    return $so
  }
  ::xotcl::Class create ::xo::InstanceManager \
      -instproc alloc args {
        set r [next]
        set key blueprint($r)
        if {![ns_conn isconnected]} {
          [self class] set $key 1
        } elseif {![[self class] exists $key]} {
          [self class] set connectionobject($r) 1
        }
        return $r
      } \
      -instproc destroy args {
        next
        ns_log notice "--unset -nocomplain [self class]::blueprint([self])"
        [self class] unset -nocomplain blueprint([self])
        [self class] unset -nocomplain connectionobject([self])
      }

  # deactivate for now
  #::xotcl::Object instmixin add ::xo::InstanceManager
}

::xotcl::nonposArgs proc integer args {
  if {[llength $args] < 2} return
  foreach {name value} $args break
  if {![string is integer $value]} {error "value '$value' of $name not an integer"}
}
::xotcl::nonposArgs proc optional {name args} {
  ;
}

::xotcl::Object instproc __timediff {} {
  set now [ns_time get]
  if {[ns_conn isconnected]} {
    set start_time [ns_conn start]
  } else {
    if {![info exists ::__start_time]} {set ::__start_timestamp $now}
    set start_time $::__start_timestamp
  }
  set t [ns_time diff $now $start_time]
  set ms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
  if {[info exists ::__last_timestamp]} {
    set t [ns_time diff $now $::__last_timestamp]
    set dms [expr {[ns_time seconds $t]*1000 + [ns_time microseconds $t]/1000}]
    set diff ", ${dms}ms"
  } else {
    set diff ""
  }
  set ::__last_timestamp $now
  return "${ms}ms$diff"
}

::xotcl::Object instproc log msg {
  ns_log notice "$msg, [self] [self callingclass]->[self callingproc] ([my __timediff])"
}

::xotcl::Object instproc debug msg {
  ns_log debug "[self] [self callingclass]->[self callingproc]: $msg"
}
::xotcl::Object instproc msg {{-html false} msg} {
  if {[ns_conn isconnected]} {
    set msg "$msg  ([self] [self callingclass]->[self callingproc])"
    if {$html} {
        util_user_message -html -message $msg
    } else {
      util_user_message -message $msg
    }
  }
}
::xotcl::Object instproc qn query_name {
  set qn "dbqd.[my uplevel self class]-[my uplevel self proc].$query_name"
  return $qn
}
namespace eval ::xo {
  Class Timestamp
  Timestamp instproc init {} {my set time [clock clicks -milliseconds]}
  Timestamp instproc diffs {} {
    set now [clock clicks -milliseconds]
    set ldiff [expr {[my exists ltime] ? [expr {$now-[my set ltime]}] : 0}]
    my set ltime $now
    return [list [expr {$now-[my set time]}] $ldiff]
  }
  Timestamp instproc diff {{-start:switch}} {
    lindex [my diffs] [expr {$start ? 0 : 1}]
  }

  Timestamp instproc report {{string ""}} {
    foreach {start_diff last_diff} [my diffs] break
    my log "--$string (${start_diff}ms, diff ${last_diff}ms)"
  }

  proc show_stack {{m 100}} {
    if {[::info exists ::template::parse_level]} {
      set parse_level $::template::parse_level
    } else {
      set parse_level ""
    }
    set msg "### tid=[::thread::id] <$parse_level> connected=[ns_conn isconnected] "
    if {[ns_conn isconnected]} {
      append msg "flags=[ad_conn flags] status=[ad_conn status] req=[ad_conn request]"
    }
    my log $msg
    set max [info level]  
    if {$m<$max} {set max $m}
    my log "### Call Stack (level: command)"
    for {set i 0} {$i < $max} {incr i} {
      if {[catch {set s [uplevel $i self]} msg]} {
	set s ""
      }
      my log "### [format %5d -$i]:\t$s [info level [expr {-$i}]]"
    }
  }

}

namespace eval ::xo {
  # 
  # Make reporting back of the version numbers of the most important 
  # nvolved components easier.
  #
  proc report_version_numbers {{pkg_list {acs-kernel xotcl-core xotcl-request-monitor xowiki s5 xoportal xowf}}} {
    append _ "Database: "
    if {[db_driverkey {}] eq "postgresql"} {
      append _ [db_string dbqd.null.get_version {select version() from dual}] \n
    } else {
      append _ [db_driverkey {}]\n
    }
    append _ "Server:    [ns_info patchlevel] ([ns_info name])\n"
    append _ "Tcl:       $::tcl_patchLevel\n"
    append _ "XOTcl:     $::xotcl::version$::xotcl::patchlevel\n"
    append _ "Tdom:      [package req tdom]\n"
    append _ "libthread: [ns_config ns/server/[ns_info server]/modules libthread]\n"
    append _ "Tcllib: \n"
    append _ [exec sh -c "ls -ld [ns_info home]/lib/tcll*"] \n\n
    foreach pk $pkg_list {
      if {[apm_package_installed_p $pk]} {
        append _ "[format %-22s $pk:] " [apm_version_get -package_key $pk -array ""; set x "$(release_date), $(version_name)"] \n
      }
    }
    return $_
  }
}

#ns_log notice "--T [info command ::ttrace::isenabled]"
# tell ttrace to put these to the blueprint
#if {[info command ::ttrace::isenabled] ne "" && [::ttrace::isenabled]} {
#  ns_log notice "--T :ttrace::isenabled"
#  set blueprint [ns_ictl get]
#  ns_ictl save [append blueprint [::Serializer serializeExportedMethods \
#				      [::Serializer new -volatile]]]
#  unset blueprint
#  ns_log notice "--T [ns_ictl get]"
#}

namespace eval ::xo {
  #
  # In earlier versions of xotcl-core, we used variable traces
  # to trigger deletion of objects. This had two kind of problems:
  #   1) there was no way to control the order of the deletions
  #   2) the global variables used for managing db handles might
  #      be deleted already
  #   3) the traces are executed at a time when the connection 
  #      is already closed
  # Aolserver 4.5 supports a trace for freeconn. We can register
  # a callback to be executed before the connection is freed,
  # therefore, we have still information from ns_conn available.
  # For aolserver 4.5 we use oncleanup, which is at least before
  # the cleanup of variables.
  #
  # In contrary, in 4.0.10, on cleanup is called after the global
  # variables of a connection thread are deleted. Therefore
  # the triggered calls should not use database handles,
  # since these are as well managed via global variables,
  # the will be deleted as well at this time,.
  # 
  # To come up with an approach working for 4.5 and 4.0.10, we
  # distinguish between a at_cleanup and at_close, so connection
  # related info can still be obtained. 
  #
  if {[catch {set registered [ns_ictl gettraces freeconn]}]} {
    ns_log notice "*** you should really upgrade to Aolserver 4.5"
    # "ns_ictl oncleanup" is called after variables are deleted
    if {[ns_ictl epoch] == 0} {
      ns_ictl oncleanup ::xo::at_cleanup
      ns_ictl oninit [list ns_atclose ::xo::at_close]
      ns_ictl ondelete ::xo::at_delete
    }
    
#     proc trace_cleanup {args} {
#       set name [lindex $args 1]
#       #ns_log notice "*** cleanup <$args> '$name'"
#       if {[::xotcl::Object isobject $name]} {
# 	ns_log notice "*** cleanup $name destroy"
# 	$name destroy
#       }
#     }
  } else {

    # register only once
    if {[lsearch $registered ::xo::cleanup] == -1} {
      ns_ictl trace freeconn ::xo::freeconn
    }
    if {[lsearch [ns_ictl gettraces delete] ::xo::at_delete] == -1} {
      ns_ictl ondelete ::xo::at_delete
    }    

    proc ::xo::freeconn {} {
      catch {::xo::at_close}
      catch {::xo::at_cleanup}
    }
  }

  #proc ::xo::at_create {} {
  #  ns_log notice "--at_create *********"
  #  foreach i [::xo::InstanceManager array names blueprint] {
  #    if {![::xotcl::Object isobject $i]} {
  #      ::xo::InstanceManager unset blueprint($i)
  #      ns_log notice "--at_create no such object: $i"
  #    }
  #  }
  #}

  ::xotcl::Object instproc destroy_on_cleanup {} {
    #my log "--cleanup adding ::xo::cleanup([self]) [list [self] destroy]"
    set ::xo::cleanup([self]) [list [self] destroy]
  }

  proc at_close {args} {
  }

  proc at_cleanup {args} {
    #ns_log notice "*** start of cleanup <$args> ([array get ::xo::cleanup])"
    set at_end ""
    foreach {name cmd} [array get ::xo::cleanup] {
      #::trace remove variable ::xotcl_cleanup($name) unset ::xo::cleanup
      if {![::xotcl::Object isobject $name]} {
        #ns_log notice "--D $name already destroyed, nothing to do"
        continue
      }
      if {$name eq "::xo::cc"} {
        append at_end $cmd\n
        continue
      }
      #ns_log notice "*** cleanup $cmd"
      if {[catch {eval $cmd} errorMsg]} {
        set obj [lindex $cmd 0]
        ns_log error "Error during ::xo::cleanup: $errorMsg $::errorInfo"
        catch {
          ns_log notice "... analyze: cmd = $cmd"
          ns_log notice "... analyze: $obj is_object? [::xotcl::Object isobject $obj]"
          ns_log notice "... analyze: class [$obj info class]"
          ns_log notice "... analyze: precedence [$obj info precedence]"
          ns_log notice "... analyze: methods [lsort [$obj info methods]]"
          #
          # In case, we want to destroy some objects, and the
          # destructor fails, make sure to destroy them even
          # then. Half-deleted zombies can produce harm. We reclass
          # the object to the base classe and try again.
          #
          if {[lindex $cmd 1] eq "destroy"} {
            ns_log error "... forcing object destroy without application level destuctors"
            if {[$obj isclass]} {
              $obj class ::xotcl::Class; $obj destroy
            } else {
              $obj class ::xotcl::Object; $obj destroy
            }
          }
        }
      }
    }
    #ns_log notice "*** at_end $at_end"
    if {[catch {eval $at_end} errorMsg]} {
      ns_log notice "Error during ::xo::cleanup: $errorMsg $::errorInfo"
    }
    array unset ::xo::cleanup
    #ns_log notice "*** end of cleanup"
  }

  proc ::xo::at_delete args {
    #
    # Delete all object and classes at a time, where the thread is
    # fully functioning. During interp exit, the commands would be
    # deleted anyhow, but there exists a potential memory leak, when
    # e.g. a destroy method (or an exit handler) writes to ns_log.
    # ns_log requires the thread name, but it is cleared already
    # earlier (after the interp deletion trace). Aolserver recreated
    # the name and the an entry in the thread list, but this elements
    # will not be freed. If we destroy the objects here, the mentioned
    # problem will not occur.
    #
    ns_log notice "ON DELETE $args"
    set t0 [clock clicks -milliseconds]
    #
    # Check, if we have a new XOTcl implementation with ::xotcl::finalize
    # 
    if {[info command ::xotcl::finalize] ne ""} {
      ::xotcl::finalize
    } else {
      # Delete the objects and classes manually
      set objs [::xotcl::Object allinstances]
      ns_log notice "deleting [llength $objs] objects"
      foreach o $objs {
        if {![::xotcl::Object isobject $o]} continue
        if {[$o istype ::xotcl::Class]} continue
        $o destroy
      }
      foreach o [::xotcl::Class allinstances] {
        if {![::xotcl::Object isobject $o]} continue
        if {$o eq "::xotcl::Object" || $o eq "::xotcl::Class"} continue
        $o destroy
      }
    }
    set t1 [clock clicks -milliseconds]
    ns_log notice "ON DELETE done ([expr {$t1-$t0}]ms)"
  }
  
}

#ns_log notice "*** FREECONN? [ns_ictl gettraces freeconn]"
#ns_ictl trace freeconn {ns_log notice "*** FREECONN  isconnected=[ns_conn isconnected]"}
#ns_ictl oncleanup {ns_log notice "*** ONCLEANUP isconnected=[ns_conn isconnected]"}
