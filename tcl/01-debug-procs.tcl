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
}

::xotcl::Object instforward db_1row -objscope

::xotcl::Object instproc serialize {} {
  ::Serializer deepSerialize [self]
}


# Currently, xotcl's serializer does not export ::xotcl::* commands,
# except methods for ::xotcl::Object and ::xotcl::Core, so we use the
# mixin instead of te direct defintion... should be changed in the future
# namespace eval ::xo {
#   Class create ::xo::NonPosArgs \
#     -instproc integer args {
#       if {[llength $args] < 2} return
#       foreach {name value} $args break
#       if {![string is integer $value]} {
#         error "value '$value' of $name not an integer"
#       }
#     } \
#     -instproc optional {name args} {
#       ;
#     }
# }
# ::xotcl::nonposArgs proc integer
#  ::xotcl::nonposArgs proc optional

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
::xotcl::Object instproc msg msg {
  util_user_message -message "$msg, [self] [self callingclass]->[self callingproc] ([my __timediff])"
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

# ::xotcl::Class instproc import {class pattern} {
#   namespace eval [self] [list \
#   namespace import [list import [$class self]]::$pattern;
#     my log "--namespace [list import [$class self]]::$pattern"
#   ]
# }

# ::xotcl::Class instproc export args {
#   my log "--namespace eval [self] {eval namespace export $args}"
#   namespace eval [self] [list eval namespace export $args]
# }

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
      ns_ictl oncleanup ::xo::at_init
    }
    proc ::xo::at_init {} {
      ns_atclose ::xo::at_close
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

    proc ::xo::freeconn {} {
      catch {::xo::at_close}
      catch {::xo::at_cleanup}
    }
  }

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
        ns_log notice "--D $name already destroyed, nothing to do"
        continue
      }
      if {$name eq "::xo::cc"} {
        append at_end $cmd\n
        continue
      }
      #ns_log notice "*** cleanup $cmd"
      if {[catch {eval $cmd} errorMsg]} {
        set obj [lindex $cmd 0]
        ns_log notice "Error during ::xo::cleanup: $errorMsg $::errorInfo"
        catch {
          ns_log notice "... analyze: cmd = $cmd"
          ns_log notice "... analyze: $obj is_object? [::xotcl::Object isobject $obj]"
          ns_log notice "... analyze: class [$obj info class]"
          ns_log notice "... analyze: precedence [$obj info precedence]"
          ns_log notice "... analyze: methods [lsort [$obj info methods]]"
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
}

#ns_log notice "*** FREECONN? [ns_ictl gettraces freeconn]"
#ns_ictl trace freeconn {ns_log notice "*** FREECONN  isconnected=[ns_conn isconnected]"}
#ns_ictl oncleanup {ns_log notice "*** ONCLEANUP isconnected=[ns_conn isconnected]"}
