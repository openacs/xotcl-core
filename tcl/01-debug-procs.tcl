
if {$::tcl_version < 8.5
    || ([regexp {8[.]5[.]([0-9]+)$} $::tcl_patchLevel _ minor] && $minor < 4)
  } {
  ns_log error "We require for this version of xotcl-core at least Tcl 8.5.4 (avail: Tcl $::tcl_patchLevel)"
  return
}
if {[info exists ::xotcl_version] || ([info exists ::xotcl::version] && $::xotcl::version < 2.0)} {
  ns_log error "We require for this version of xotcl-core at least XOTcl 2.0"
  return
}

package require xotcl::serializer

#
# Keep the initcmds of classes for documentation purposes.
#
::nsf::configure keepcmds 1

#
# Tell serializer to export methods, although these are methods of the
# base classes.
#
::Serializer exportMethods {
  ::xotcl::Object instproc log
  ::xotcl::Object instproc ds
  ::xotcl::Object instproc msg
  ::xotcl::Object instproc __timediff
  ::xotcl::Object instproc debug
  ::xotcl::Object instproc qn
  ::xotcl::Object instproc serialize
  ::xotcl::Object instproc www-show-object
  ::xotcl::Object instproc destroy_on_cleanup
  ::xotcl::Object instproc set_instance_vars_defaults
  ::xotcl::Object instproc mset
  ::xotcl::Class instproc extend_slot
}

namespace eval ::xo {
  # create xo::Attribute as a subclass of the slot ::xotcl::Attribute
  ::xotcl::MetaSlot create ::xo::Attribute \
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

set ::xo::naviserver [expr {[ns_info name] eq "NaviServer"}]

if {[info commands ::nx::Object] ne ""} {
  ns_log notice "Defining minimal XOTcl 1 compatibility"
  ::nsf::method::alias ::xo::Attribute instvar ::nsf::methods::object::instvar

  # provide compatibility with nsf 2.0b6, which has "-noinit" removed
  ::nx::ObjectParameterSlot create ::xo::Attribute::slot::noinit \
      -methodname ::nsf::methods::object::noinit -noarg true

  # The following line would cause a dependency of an nx object to
  # xotcl (serializer); since XOTcl depends on NX, this would be a
  # cyclic dependency.
  #     ::nsf::method::alias ::nx::Slot istype ::nsf::classes::xotcl::Object::istype
  # Therefore, we just grab the body to reduce dependencies on nsf internals
  ::nx::Slot public method istype {class}  [::nx::Object info method body ::nsf::classes::xotcl::Object::istype]
  ::nx::Slot public alias set -frame object ::set
  ::nx::Slot public method exists {var}   {::nsf::var::exists [self] $var}
  ::nx::Object public method serialize {} {::Serializer deepSerialize [self]}
  ::nx::Object public method destroy_on_cleanup {} {set ::xo::cleanup([self]) [list [self] destroy]}
  ::nx::Object method qn {query_name} {
    return "dbqd.[:uplevel [list current class]]-[:uplevel [list current method]].$query_name"
  }
  #
  # Allow the use of types "naturalnum" and "token" e.g. in
  # ::xowiki::Package initialize.
  #
  ::nx::Slot eval {
    :method type=naturalnum {name value} {
      if {![string is integer -strict $value] || $value < 0 } {
        return -code error "Value '$value' of parameter $name is not a natural number."
      }
    }
    :method type=token {name value} {
      if {![regexp {^[\w.,: -]+$} $value]} {
        return -code error "Value '$value' of parameter $name is not a valid token."
      }
    }
  }

  ::xotcl::Object proc setExitHandler {code} {::nsf::exithandler set $code}
  ::xotcl::Object instproc set_instance_vars_defaults {} {:configure}

  ::Serializer exportMethods {
    ::nx::Object method serialize
    ::nx::Object method destroy_on_cleanup
    ::nx::Object method qn
    ::nx::Slot method istype
    ::nx::Slot method exists
    ::nx::Slot method set
    ::nx::Slot method type=naturalnum
    ::nx::Slot method type=token
    ::nx::Object nsfproc ::nsf::debug::call
    ::nx::Object nsfproc ::nsf::debug::exit
  }

  if {[nx::Class  info methods -path "info superclasses"] eq ""} {
    # There is no "info superclasses" defined, it must be a beta
    # release of nsf.  Map method names to improve robustness for
    # earlier versions (should be transitional code).
    array set ::xo::mapMethodNames {
      superclasses superclass
      subclasses subclass
      mixins "mixin classes"
    }
  } else {
    array set ::xo::mapMethodNames {
      superclasses superclasses
      subclasses subclasses
      mixins mixins
    }
  }

  #
  # Make sure, the ::nsf::debug namespace exists (might not be
  # available in older versions of nsf)
  #
  namespace eval ::nsf::debug {}

  proc ::nsf::debug::call {level objectInfo methodInfo arglist} {
    ns_log Warning "DEBUG call($level) - {$objectInfo} {$methodInfo} $arglist"
  }
  proc ::nsf::debug::exit {level objectInfo methodInfo result usec} {
    #ns_log Warning "DEBUG exit($level) - {$objectInfo} {$methodInfo} $usec usec -> $result"
    ns_log Warning "DEBUG exit($level) - {$objectInfo} {$methodInfo} $usec usec"
  }
}


namespace eval ::xo {
  ::xo::Attribute instproc init {} {
    next
    # provide a default pretty name for the attribute based on message keys
    if {![info exists :pretty_name]} {
      set object_type ${:domain}
      if {[regexp {^::([^:]+)::} $object_type _ head]} {
        set tail [namespace tail $object_type]
        set :pretty_name "#$head.$tail-${:name}#"
        #:log "--created pretty_name = ${:pretty_name}"
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


if {[info exists ::acs::preferdbi]} {
  ::xotcl::Object instforward dbi_1row    -objscope ::dbi_1row
  ::xotcl::Object instforward dbi_0or1row -objscope ::dbi_0or1row
  ::xotcl::Object instproc    db_1row    {. sql} {:dbi_1row $sql}
  ::xotcl::Object instproc    db_0or1row {. sql} {:dbi_0or1row $sql}
  ::Serializer exportMethods {
    ::xotcl::Object instforward dbi_1row
    ::xotcl::Object instforward dbi_0or1row
    ::xotcl::Object instproc db_1row
    ::xotcl::Object instproc db_0or1row
  }
} else {
  ::xotcl::Object instforward db_1row -objscope
  ::xotcl::Object instforward db_0or1row -objscope
  ::Serializer exportMethods {
    ::xotcl::Object instforward db_1row
    ::xotcl::Object instforward db_0or1row
  }
}

if {[::package vcompare [package require xotcl::serializer] 2.0] < -1} {
  #
  # The serializer of xotcl/2.0 registers already a method "serialize"
  # on ::xotcl::Object. Don't mess with that.
  #
  ::xotcl::Object instproc serialize {} {
    ::Serializer deepSerialize [self]
  }
}

::xotcl::Object instproc mset {pairs} {
  #
  # Import all attribute value pairs into the current XOTcl object.
  #
  if {[llength $pairs] > 0} {
    nsf::directdispatch [self] -frame object ::lassign [dict values $pairs] {*}[dict keys $pairs]
  }
}

::xotcl::Object instproc www-show-object {} {
  #
  # Allow to show an arbitrary object via API-browser.  Per-default,
  # e.g. site-wide can use e.g. /xowiki/index?m=show-object
  #
  set form [rp_getform]
  ns_set update $form object [self]
  ns_set update $form show_source    [::xo::cc query_parameter "show_source" 1]
  ns_set update $form show_methods   [::xo::cc query_parameter "show_methods" 2]
  ns_set update $form show_variables [::xo::cc query_parameter "show_variables" 1]
  rp_internal_redirect /packages/xotcl-core/www/show-object
}

namespace eval ::xo {
  proc slotobjects cl {
    set so [list]
    array set names ""
    foreach c [list $cl {*}[$cl info heritage]] {
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

if {[info commands ::xotcl::nonposArgs] ne ""} {
  ::xotcl::nonposArgs proc integer args {
    if {[llength $args] < 2} return
    lassign $args name value
    if {![string is integer $value]} {error "value '$value' of $name not an integer"}
  }
  ::xotcl::nonposArgs proc optional {name args} {
    ;
  }
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
  ns_log notice "$msg, [self] [self callingclass]->[self callingproc] ([:__timediff])"
}
::xotcl::Object instproc ds msg {
  ds_comment "[self]: $msg, ([self callingclass]->[self callingproc] [:__timediff])"
}
::xotcl::Object instproc debug msg {
  ns_log debug "[self] [self callingclass]->[self callingproc]: $msg"
}
::xotcl::Object instproc msg {{-html false} msg} {
  if {[ns_conn isconnected]} {
    set msg "[self]: $msg  ([self callingclass]->[self callingproc])"
    if {$html} {
      util_user_message -html -message $msg
    } else {
      util_user_message -message $msg
    }
  }
}

# quick debugging tool
# proc ::! args {
#   ns_log notice "-- PROC [info level -1]"
#   ns_log notice "-- CALL $args"
#   set r [uplevel $args]
#   ns_log notice "-- EXIT $r"
#   return $r
# }

::xotcl::Object instproc qn query_name {
  #set qn "dbqd.[:uplevel [list self class]]-[:uplevel [list self proc]].$query_name"
  set l [info level]
  if {$l < 2} {
    set prefix topLevel
  } else {
    set prefix [:uplevel {info level 0}]
  }
  return "dbqd.$prefix.$query_name"
}
namespace eval ::xo {
  Class create Timestamp
  Timestamp instproc init {} {set :time [clock clicks -milliseconds]}
  Timestamp instproc diffs {} {
    set now [clock clicks -milliseconds]
    set ldiff [expr {[info exists :ltime] ? ($now-${:ltime}): 0}]
    set :ltime $now
    return [list [expr {$now-${:time}}] $ldiff]
  }
  Timestamp instproc diff {{-start:switch}} {
    lindex [:diffs] [expr {$start ? 0 : 1}]
  }

  Timestamp instproc report {{string ""}} {
    lassign [:diffs] start_diff last_diff
    :log "--$string (${start_diff}ms, diff ${last_diff}ms)"
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
    ::xotcl::Object log $msg
    set max [info level]
    if {$m<$max} {set max $m}
    ::xotcl::Object  log "### Call Stack (level: command)"
    for {set i 0} {$i < $max} {incr i} {
      try {
        set s [uplevel $i self]
      } on error {errorMsg} {
        set s ""
      }
      ::xotcl::Object  log "### [format %5d -$i]:\t$s [info level [expr {-$i}]]"
    }
  }

}

namespace eval ::xo {
  #
  # Make reporting back of the version numbers of the most important
  # involved components easier.
  #
  proc report_version_numbers {{pkg_list {acs-kernel xotcl-core xotcl-request-monitor xowiki s5 xoportal xowf}}} {
    append _ "Database: "
    if {[db_driverkey {}] eq "postgresql"} {
      append _ [db_string dbqd.null.get_version {select version() from dual}] \n
    } else {
      append _ [db_driverkey {}]\n
    }
    append _ "Server:    [ns_info patchlevel] ([ns_info name] [ns_info tag])\n"
    append _ "Tcl:       $::tcl_patchLevel\n"
    append _ "XOTcl:     $::xotcl::version$::xotcl::patchlevel\n"
    append _ "Tdom:      [package req tdom]\n"
    append _ "libthread: [ns_config ns/server/[ns_info server]/modules libthread]\n"
    append _ "Tcllib:    "
    foreach dir $::auto_path {
      set p [glob -nocomplain $dir/tcllib*]
      if {$p ne ""} {
        append _ "$p"
        # just show first occurrences on path
        break
      }
    }
    append _ \n
    foreach pk $pkg_list {
      if {[apm_package_installed_p $pk]} {
        append _ "[format %-22s $pk:] " [apm_version_get -package_key $pk -array ""; set x "$(release_date), $(version_name)"] \n
      }
    }
    return $_
  }

  proc pg_version {} {
    #
    # Return 2 digit version number (suitable for number compare
    # operations) from PostgreSQL or 0.0 if not available
    #
    set key ::xo::pg_version
    if {[info exists $key]} {
      return [set $key]
    }
    set version 0.0
    if {[db_driverkey {}] eq "postgresql"} {
      set version_string [db_string dbqd.null.get_version {select version() from dual}]
      regexp {PostgreSQL ([0-9]+[.][0-9+])} $version_string . version
    }
    return [set $key $version]
  }
}

#ns_log notice "--T [info commands ::ttrace::isenabled]"
# tell ttrace to put these to the blueprint
#if {[info commands ::ttrace::isenabled] ne "" && [::ttrace::isenabled]} {
#  ns_log notice "--T :ttrace::isenabled"
#  set blueprint [ns_ictl get]
#  ns_ictl save [append blueprint [::Serializer serializeExportedMethods \
    #                      [::Serializer new -volatile]]]
#  unset blueprint
#  ns_log notice "--T [ns_ictl get]"
#}

namespace eval ::xo {
  #
  # Cleanup functions
  #

  #
  # Register xo::freeconn function only once
  #
  if {"::xo::freeconn" ni [ns_ictl gettraces freeconn]} {
    if {[catch {ns_ictl trace freeconn ::xo::freeconn} errorMsg]} {
      ns_log Error "ns_ictl trace returned: $errorMsg"
    }
  }
  
  #
  # Register::xo::at_delete function only once
  #
  if {"::xo::at_delete" ni [ns_ictl gettraces delete]} {
    if {[catch {ns_ictl trace delete ::xo::at_delete} errorMsg]} {
      ns_log Warning "rhe command 'ns_ictl trace delete' returned: $errorMsg"
    }
  }
  
  proc ::xo::freeconn {} {
    catch {::xo::at_cleanup}
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
    #:log "--cleanup adding ::xo::cleanup([self]) [list [self] destroy]"
    set ::xo::cleanup([self]) [list [self] destroy]
  }

  proc at_cleanup {args} {
    ::xo::dc profile off
    ::xo::broadcast receive
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
      try {
        {*}$cmd
      } on error {errorMsg} {
        set obj [lindex $cmd 0]
        ns_log error "Error during ::xo::cleanup: $errorMsg $::errorInfo"
        try {
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
    try {
      {*}$at_end
    } on error {errorMsg} {
      ns_log notice "Error during ::xo::cleanup: $errorMsg $::errorInfo"
    }
    array unset ::xo::cleanup
    #ns_log notice "*** end of cleanup"
  }

  proc ::xo::at_delete args {
    #
    # Delete all object and classes at a time, where the thread is
    # still fully functioning. During interp exit, the commands would
    # be deleted anyhow, but there exists a potential memory leak,
    # when e.g. a destroy method (or an exit handler) writes to
    # ns_log.  ns_log requires the thread name, but it is cleared
    # already earlier (after the interp deletion trace). AOLserver
    # recreated the name and the an entry in the thread list, but this
    # elements will not be freed. If we destroy the objects here, the
    # mentioned problem will not occur.
    #
    ns_log notice "ON DELETE $args"
    ::xo::broadcast clear

    #
    # Make sure, no handles are allocated any more. Otherwise, when
    # the thread is reused, there would be a conflict, when the thread
    # has already a handle associated but tries to obtain an
    # additional handle.
    #
    db_release_unused_handles

    set t0 [clock clicks -milliseconds]
    ::xo::system_stats recordtimes
    #
    # Check, if we have a new XOTcl implementation with ::xotcl::finalize
    #
    if {[info commands ::xotcl::finalize] ne ""} {
      ::xotcl::finalize
    } else {
      # Delete the objects and classes manually
      set objs [::xotcl::Object allinstances]
      ns_log notice "no finalize available, deleting [llength $objs] objects"
      foreach o $objs {
        if {![::xotcl::Object isobject $o]} continue
        if {[$o istype ::xotcl::Class]} continue
        catch {$o destroy} errorMsg
      }
      foreach o [::xotcl::Class allinstances] {
        if {![::xotcl::Object isobject $o]} continue
        if {$o eq "::xotcl::Object" || $o eq "::xotcl::Class"} continue
        catch {$o destroy} errorMsg
      }
    }
    set t1 [clock clicks -milliseconds]
    ns_log notice "ON DELETE done ([expr {$t1-$t0}]ms)"
  }

  proc ::xo::stats {{msg ""}} {
    set xobjs   [llength [::xotcl::Object info instances -closure]]
    set nobjs   [llength [::nx::Object info instances  -closure]]
    set tmpObjs [llength [info commands ::nsf::__#*]]
    set tdoms   [llength [list {*}[info commands domNode0*] {*}[info commands domDoc0x*]]]
    set nssets  [llength [ns_set list]]
    ns_log notice "xo::stats $msg: current objects xotcl $xobjs nx $nobjs tmp $tmpObjs tDOM $tdoms ns_set $nssets"
  }

  #
  # ::xo::Module is very similar to a plain Tcl namespace: When it is
  # created/recreated, it does not perform a cleanup of its
  # contents. This means that preexisting procs, objects classes,
  # variables etc. will survive a recreation. As a consequence,
  # ::xo::Modules can easily span multiple files an they can be used
  # like a namespace. However, the modules have the advantage that it
  # is possible to define procs, instprocs with non-positional
  # arguments directly in it. It is as well possible to use mixins
  # etc.
  #
  Class create Module
  Module instproc init    args {:requireNamespace}
  Module instproc cleanup args {ns_log notice "create/recreate [self] without cleanup"}
}

namespace eval ::xo {

  #
  # For the time being: catch changed parameter values; it would be nice
  # to have in the future a more generic interface to trigger actions
  # directly on all parameter changes.
  #
  ad_proc -public -callback subsite::parameter_changed -impl xotcl-core_param_changed {
    -package_id:required
    -parameter:required
    -value:required
  } {
    Implementation of subsite::parameter_changed for xotcl-core parameters

    @param package_id the package_id of the package the parameter was changed for
    @param parameter  the parameter name
    @param value      the new value
  } {
    set package_key [apm_package_key_from_id $package_id]
    if {$package_key eq "xotcl-core"
        && $parameter eq "NslogRedirector"
        && [info commands ::xo::ns_log_redirector_manager] ne ""
      } {
      ::xo::ns_log_redirector_manager set_level $value
      #
      # Update the blueprint to reflect the parameter change
      # immediately.
      #
      # This is a heavy solution, but the NslogRedirector is not
      # likely to be changed frequently on a production system. The
      # alternative, a server restart, is even more expensive.
      #
      ns_eval [list ::xo::ns_log_redirector_manager set_level $value]
      #set blueprint [ns_ictl get]
      #set last [string last "\n::xo::ns_log_redirector_manager" $blueprint]
      #if {$last > -1} { set blueprint [string range $blueprint 0 $last-1]}
      #ns_ictl save "$blueprint\n::xo::ns_log_redirector_manager set_level $value"
    }
  }
}

namespace eval ::xo {

  ::xotcl::Object create ::xo::system_stats

  if {$::tcl_platform(os) eq "Linux"} {

    ::xo::system_stats proc thread_info {pid tid} {
      set s ""
      set fn /proc/$pid/task/$tid/stat
      if {[file readable $fn]} {
        try {
          set f [open $fn]
          set s [read $f]
        } on error err {
          set errorMsg "IO error $err reading file $fn"
          if {[info exists f]} { append errorMsg " (fh $f)" }
          ns_log error $errorMsg
        } finally {
          close $f
        }
      } elseif {[file readable /proc/$pid/task/$pid/stat]} {
        set fn /proc/$pid/task/$pid/stat
        try {
          set f [open $fn]
          set s [read $f]
        } on error err {
          set errorMsg "IO error $err reading file $fn"
          if {[info exists f]} { append errorMsg " (fh $f)" }
          ns_log error $errorMsg
        } finally {
          close $f
        }
      }
      if {$s ne ""} {
        lassign $s tid comm state ppid pgrp session tty_nr tpgid flags minflt \
            cminflt majflt cmajflt utime stime cutime cstime priority nice \
            numthreads itrealval starttime vsize rss rsslim startcode endcode \
            startstack kstkesp kstkeip signal blocked sigignore sigcatch wchan \
            nswap cnswap ext_signal processor ...
        # utime and stimes are jiffies. Since Linux has HZ 100, we can
        # multiply the jiffies by 10 to obtain ms
        return [list utime [expr {$utime*10}] stime [expr {$stime*10}]]
      }
    }
 
  } else {
    ::xo::system_stats proc thread_info {pid tid} {
      return ""
    }
  }

  ::xo::system_stats proc gettid {} {
    set hex [ns_thread id]
    foreach t [ns_info threads] {
      if {[lindex $t 2] eq $hex} {
        return [list name [lindex $t 0] tid [lindex $t 7]]
      }
    }
    return ""
  }

  ::xo::system_stats proc thread_classify {name} {
    switch -glob -- $name {
      "-main-"    { set group main }
      "::*"       { set group tcl:[string range $name 2 end]}
      "-sched*"   { set group scheds  }
      "-conn:*"   { set group conns   }
      "-driver:*" { set group drivers }
      "-asynclogwriter*" { set group logwriter }
      "-writer*"  { set group writers }
      default     { set group others  }
    }
    return $group
  }

  ::xo::system_stats proc recordtimes {} {
    set threadInfo [:gettid]
    if {$threadInfo ne ""} {
      array set i $threadInfo
      array set i [:thread_info [pid] $i(tid)]
      if {[info exists i(stime)]} {
        set group [:thread_classify $i(name)]
        nsv_incr [self] $group,stime $i(stime)
        nsv_incr [self] $group,utime $i(utime)
      }
    }
  }

  ::xo::system_stats proc aggregate {group time value} {
    upvar $time times
    if {![info exists times($group)]} {set times($group) 0}
    set times($group) [expr {$times($group) + $value}]
  }

  ::xo::system_stats proc aggcpuinfo {utime stime ttime} {
    upvar $utime utimes $stime stimes $ttime ttimes
    set pid [pid]
    array set varnames {utime utimes stime stimes}
    foreach index [nsv_array names [self]] {
      lassign [split $index ,] group kind
      :aggregate $group $varnames($kind) [nsv_get [self] $index]
    }
    set threadInfo [ns_info threads]
    if {[file readable /proc/$pid/statm] && [llength [lindex $threadInfo 0]] > 7} {
      foreach t $threadInfo {
        array unset s
        array set s [:thread_info $pid [lindex $t 7]]
        if {[info exists s(stime)]} {
          set group [:thread_classify [lindex $t 0]]
          :aggregate $group $varnames(utime) $s(utime)
          :aggregate $group $varnames(stime) $s(stime)
        }
      }
    }
    foreach group [array names utimes] {
      :aggregate $group ttimes [expr {$utimes($group) + $stimes($group)}]
    }
  }
}


namespace eval ::xo {
  #
  # xo::broadcast implements a simple mechanism to send commands to
  # different connection and scheduled threads. The receiving threads
  # have to call "xo::broadcast receive" when they are able to process
  # the commands. The connection threads realize this in xo::atcleanup
  # after a request was processed (defined in this file).
  #
  ::xotcl::Object create ::xo::broadcast
  ::xo::broadcast proc send {-thread_pattern cmd} {
    foreach thread_info [ns_info threads] {
      set tn [lindex $thread_info 0]
      if { [info exists thread_name] && ![string match $thread_pattern $tn] } {
        continue
      }
      switch -glob -- $tn {
        -conn:* -
        -sched:* {
          set tid [lindex $thread_info 2]
          nsv_lappend broadcast $tid $cmd
        }
      }
    }
  }
  ::xo::broadcast proc blueprint {cmd} {
    foreach t [::xotcl::THREAD info instances] {
      $t do eval $cmd
    }
    ns_eval ${cmd}\n::xo::at_cleanup
  }
  ::xo::broadcast proc clear {} {
    catch {nsv_unset broadcast [ns_thread id]}
  }
  ::xo::broadcast proc receive {} {
    set tid [ns_thread id]
    if {[nsv_exists broadcast $tid]} {
      foreach cmd [nsv_get broadcast $tid] {
        ns_log notice "broadcast received {$cmd}"
        try {
          {*}$cmd
        } on error {errorMsg} {
          ns_log notice "broadcast receive error: $errorMsg for cmd $cmd"
        }
      }
      :clear
    }
  }
}

proc ::xo::getObjectProperty {o what args} {
  switch -- $what {
    "mixin" {
      return [$o ::nsf::methods::object::info::mixins]
    }
    "instmixin" {
      return [$o ::nsf::methods::class::info::mixins]
    }
    "mixinof" {
      return [$o ::nsf::methods::class::info::mixinof -scope object]
    }
    "instmixinof" {
      return [$o ::nsf::methods::class::info::mixinof -scope class]
    }

    "instproc" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info instprocs {*}$args]}
      return [$o info methods -type scripted -callprotection all {*}$args]
    }
    "instcommand" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info instcommands {*}$args]}
      return [$o info methods {*}$args]
    }
    "instforward" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info instforward {*}$args]}
      return [$o info methods -type forwarder {*}$args]
    }
    "instmethodtype" {
      return [$o ::nsf::methods::class::info::method type {*}$args]
    }
    "methodtype" {
      return [$o ::nsf::methods::object::info::method type {*}$args]
    }
    "proc" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info procs {*}$args]}
      return [$o info object methods -type scripted {*}$args]
    }
    "command" {
      return [$o ::nsf::methods::object::info::methods {*}$args]
    }
    "forward" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info forward {*}$args]}
      return [$o info object methods -type forwarder {*}$args]
    }
    "slots" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info slots]}
      return [$o info object methods -type forwarder]
    }
    "class" {
      #if {"::xotcl::Object" in [$o info precedence]} {return [$o info class]}
      return [$o ::nsf::methods::object::info::class]
    }
    "superclass" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info superclass]}
      return [$o info $::xo::mapMethodNames(superclasses)]
    }
    "heritage" {
      #if {"::xotcl::Object" in [$o info precedence]} {return [$o info heritage]}
      return [$o info heritage]
    }
    "subclass" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info subclass]}
      return [$o info $::xo::mapMethodNames(subclasses)]
    }
    "parameter" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info parameter]}
      set result ""
      foreach p [$o info lookup parameters configure] {
        set n [nsf::parameter::info name $p]
        if {[string match __* $n]} continue
        lappend result $n
      }
      return $result
    }
    "isclass" {
      return [nsf::is class $o]
    }
    "isobject" {
      return [nsf::is object $o]
    }
    "isbaseclass" {
      if {[info commands $o] eq ""} {return 0}
      if {[catch {set p [$o info precedence]}]} {return 0}
      return [expr {[lindex $p end] eq $o}]
    }
    "instmethodparameter" {
      return [$o ::nsf::methods::class::info::method parameter {*}$args]
    }
    "methodparameter" {
      return [$o ::nsf::methods::object::info::method parameter {*}$args]
    }
    "instargs" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info instargs {*}$args]}
      set posargs {}
      foreach m [$o info method args {*}$args] p [$o info method parameters {*}$args] {
        if {[string index [lindex $p 0] 0] eq "-"} continue
        lappend posargs $m
      }
      return $posargs
    }
    "args" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o info args {*}$args]}
      set posargs {}
      foreach m [$o info object method args {*}$args] p [$o info object method parameters {*}$args] {
        if {[lindex [string index $p 0] 0] eq "-"} continue
        lappend posargs $m
      }
      return $posargs
    }
    "instargdefault" {
      if {"::xotcl::Object" in [$o info precedence]} {
        return [uplevel [list $o info instdefault {*}$args]]
      }
      lassign $args method arg varName
      foreach p [$o info method parameters $method] {
        lassign $p name default
        if {$name eq $arg} {
          uplevel [list set $varName $default]
          return [expr {[llength $p] == 2}]
        }
      }
      return 0
    }
    "argdefault" {
      if {"::xotcl::Object" in [$o info precedence]} {
        return [uplevel [list $o info default {*}$args]]
      }
      lassign $args method arg varName
      foreach p [$o info object method parameters $method] {
        lassign $p name default
        if {$name eq $arg} {
          uplevel [list set $varName $default]
          return [expr {[llength $p] == 2}]
        }
      }
      return 0
    }

    "array-exists" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o array exists {*}$args]}
      return [$o eval [list array exists :{*}$args]]
    }
    "array-get" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o array get {*}$args]}
      return [$o eval [list array get :{*}$args]]
    }
    "array-set" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o array set {*}$args]}
      return [$o eval [list array set :{*}$args]]
    }
    "set" {
      if {"::xotcl::Object" in [$o info precedence]} {return [$o set {*}$args]}
      return [$o eval [list set :[lindex $args 0]]]
    }
    "vars" {
      return [$o ::nsf::methods::object::info::vars]
    }

    "isnxobject" {
      if {[info commands ::nsf::dispatch] ne "" && [info commands $o] ne ""} {
        return [::nsf::dispatch $o ::nsf::methods::object::info::hastype ::nx::Object]
      } {
        return 0
      }
    }
    default {
      error "no idea how to return $what"
    }
  }
}

#
# Helper method to copy a slot and configure it
#
::xotcl::Class instproc extend_slot {arg} {

  # The argument list is e.g. "foo -name x -title y"
  #
  # It is placed into one argument to avoid interference with the "-"
  # argument parsing since it will always start with a non-dashed
  # value.
  #
  set name [lindex $arg 0]
  set config [lrange $arg 1 end]

  # search for slot
  foreach c [:info heritage] {
    if {[info commands ${c}::slot::$name] ne ""} {
      set slot ${c}::slot::$name
      break
    }
  }
  if {![info exists slot]} {error "can't find slot $name"}

  # copy slot and configure it
  set newSlot [self]::slot::$name

  $slot copy $newSlot
  $newSlot configure -domain [self] -manager $newSlot -create_acs_attribute false -create_table_attribute false {*}$config
  set :db_slot($name) $newSlot
}


#ns_log notice "*** FREECONN? [ns_ictl gettraces freeconn]"
#ns_ictl trace freeconn {ns_log notice "*** FREECONN  isconnected=[ns_conn isconnected]"}
#ns_ictl oncleanup {ns_log notice "*** ONCLEANUP isconnected=[ns_conn isconnected]"}

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
