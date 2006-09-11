# tell serializer to export methods, although these are methods of 
# ::xotcl::Object

::Serializer exportMethods {
  ::xotcl::Object instproc log 
  ::xotcl::Object instproc debug
  ::xotcl::Object instproc contains
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
  ns_log notice "$msg, [self] [self callingclass]->[self callingproc] (${ms}ms$diff)"
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

  #
  # a simple calback for cleanup of per connection objects
  # ns_atclose is a little to early for us...
  #
  ::xotcl::Object instproc destroy_on_cleanup {} {
    set ::xotcl_cleanup([self]) 1
    ::trace add variable ::xotcl_cleanup([self]) unset ::xo::cleanup_callback
  }
  proc ::xo::cleanup_callback {var object op} {
    if {![::xotcl::Object isobject $object]} {
      #ns_log notice "--D $object already destroyed, nothing to do"
      $object destroy
    } else {
      #ns_log notice "--D $object destroy"
      $object destroy
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

