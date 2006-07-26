# tell serializer to export methods, allthough these are methods of 
# ::xotcl::Object
::Serializer exportMethods {
  ::xotcl::Object instproc log 
  ::xotcl::Object instproc debug
  ::xotcl::Object instproc contains
}

::xotcl::Object instproc contains cmds {
  my requireNamespace
  namespace eval [self] $cmds
}

::xotcl::Object instproc log msg {
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
  ns_log notice "[self] [self callingclass]->[self callingproc]: $msg (${ms}ms$diff)"
  set ::__last_timestamp $now
}

::xotcl::Object instproc debug msg {
  ns_log debug "[self] [self callingclass]->[self callingproc]: $msg"
}

namespace eval ::xo {
  Class Timestamp
  Timestamp instproc init {} {my set time [clock clicks -milliseconds]}
  Timestamp instproc report {{string ""}} {
    set now [clock clicks -milliseconds]
    set rel [expr {[my exists ltime] ? "(diff [expr {$now-[my set ltime]}]ms)" : ""}]
    my log "--$string [expr {$now-[my set time]}]ms $rel"
    my set ltime $now
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

