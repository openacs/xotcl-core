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
      } {
        #
        # OpenACS specific attribute slot class, which can be further
        # refined.  Contains meta data based on the OpenACS meta-data
        # conventions.
        #
      }
}

set ::xo::naviserver [expr {[ns_info name] eq "NaviServer"}]

if {[nsf::is object ::nx::Object]} {
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
  # Allow the use of types "naturalnum", "token", "localurl", "html", "nohtml" e.g. in
  # ::xowiki::Package initialize.
  #
  ::nx::Slot eval {
    :method type=naturalnum {name value} {
      if {![string is integer -strict $value] || $value < 0 } {
        return -code error "Value '$value' of parameter $name is not a natural number."
      }
    }
    :method type=object_id {name value} {
      #
      # Object ID has SQL integers, which have a different value range
      # than Tcl integers. SQL integers are classical 32-bit quantities.
      #
      if {![string is integer -strict $value]
          || $value < -2147483648
          || $value > 2147483647
        } {
        return -code error "Value '$value' of parameter $name is not a valid object ID."
      }
    }
    :method type=token {name value} {
      if {![regexp {^[\w.,: -]+$} $value]} {
        return -code error "Value '$value' of parameter $name is not a valid token."
      }
    }
    :method type=localurl {name value} {
      if { $value eq "" || [util::external_url_p $value]} {
        return -code error "Value '$value' of parameter $name is not a valid local url."
      }
    }
    :method type=nohtml {name value} {
      if {[ad_page_contract_filter_proc_nohtml name value] == 0} {
        return -code error "Value '$value' of parameter $name contains HTML."
      }
    }
    :method type=html {name value} {
      if {[ad_page_contract_filter_proc_html name value] == 0} {
        return -code error "Value '$value' of parameter $name contains unsafe HTML."
      }
    }
    :method type=range {name value arg} {
      lassign [split $arg -] min max
      if {$min eq ""} {
        unset min
      }
      if {$max eq ""} {
        unset max
      }
      if {[info exists min] && [info exists max] &&
        ($value < $min || $value > $max)} {
        error "value '$value' of parameter $name not between $min and $max"
      } elseif {[info exists min] && $value < $min} {
        error "value '$value' of parameter $name must not be smaller than $min"
      } elseif {[info exists max] && $value > $max} {
        error "value '$value' of parameter $name must not be larger than $max"
      }
      return $value
    }
    :method type=oneof {name value arg} {
      if {$value ni [split $arg |]} {
        error "value '$value' of parameter $name is invalid"
      }
    }
    :method type=dbtext {name value} {
      #
      # Ensure that the value can be used in an SQL query.
      #
      # Note that this is not the same as quoting or otherwise
      # ensuring the safety of the statement itself. What we enforce
      # here is that the value will be accepted by the db interface
      # without complaining. The actual definition may change or be
      # database specific in the future.
      #

      #
      # Reject the NUL character
      #
      if {[string first \u00 $value] != -1} {
        error "value '$value' of parameter $name contains the NUL character"
      }
    }
    :method type=signed {name input} {
      #
      # Check, if a value is a signed value, signed by
      # ::security::parameter::signed. Note that this is a converting
      # checker. Therefore, call it always with "signed,convert" to
      # obtain the value which was signed.
      #
      set pair [ns_base64urldecode $input]
      if {[string is list -strict $pair] && [llength $pair] == 2} {
        lassign $pair value signature
        set secret [ns_config "ns/server/[ns_info server]/acs" parameterSecret ""]
        #ns_log notice "[list ad_verify_signature -secret $secret $value $signature]"
        if {[ad_verify_signature -secret $secret $value $signature]} {
          return $value
        }
      }
      ad_log warning "Value '$input' of parameter $name is not properly signed"
      return -code error "Value of parameter $name is not properly signed"
    }
    :method type=cr_item_of_package {name value:int32 package_id:int32} {
      if {![::xo::db::CrClass id_belongs_to_package -item_id $value -package_id $package_id]} {
        error "value '$value' of is not a valid content repository item of the required package"
      }
    }
  }

  ::xotcl::Object proc setExitHandler {code} {::nsf::exithandler set $code}
  ::xotcl::Object instproc set_instance_vars_defaults {} {:configure}

  ::Serializer exportMethods {
    ::nx::Object method serialize
    ::nx::Object method destroy_on_cleanup
    ::nx::Object method qn
    ::nx::Slot method exists
    ::nx::Slot method istype
    ::nx::Slot method set
    ::nx::Slot method type=cr_item_of_package
    ::nx::Slot method type=dbtext
    ::nx::Slot method type=html
    ::nx::Slot method type=localurl
    ::nx::Slot method type=naturalnum
    ::nx::Slot method type=nohtml
    ::nx::Slot method type=object_id
    ::nx::Slot method type=oneof
    ::nx::Slot method type=range
    ::nx::Slot method type=signed
    ::nx::Slot method type=token
    ::nx::Object nsfproc ::nsf::debug::call
    ::nx::Object nsfproc ::nsf::debug::exit
  }

  #
  # Make sure, the ::nsf::debug namespace exists (might not be
  # available in older versions of nsf)
  #
  #namespace eval ::nsf::debug {}

  proc ::nsf::debug::call {level objectInfo methodInfo arglist} {
    ns_log Warning "DEBUG call($level) - {$objectInfo} {$methodInfo} $arglist"
  }

  if {[acs::icanuse "nsf::config profile"]} {
    #
    # The debug call-data of nsf returns only timing information, when
    # nsf was compiled with --enable-profile. So, just try to display
    # it, when available.
    #
    proc ::nsf::debug::exit {level objectInfo methodInfo result usec} {
      #ns_log Warning "DEBUG exit($level) - {$objectInfo} {$methodInfo} $usec usec -> $result"
      ns_log Warning "DEBUG exit($level) - {$objectInfo} {$methodInfo} $usec usec"
    }
  } else {
    proc ::nsf::debug::exit {level objectInfo methodInfo result usec} {
      #ns_log Warning "DEBUG exit($level) - {$objectInfo} {$methodInfo} -> $result"
      ns_log Warning "DEBUG exit($level) - {$objectInfo} {$methodInfo}"
    }
  }
}


namespace eval ::xo {
  ::xo::Attribute instproc init {} {
    #
    # Constructor of the OpenACS specific attribute slot class
    #
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
  # e.g. a site-wide admin can use e.g. /xowiki/index?m=show-object
  #
  if {[ns_conn isconnected]} {
    set form [ns_getform]
    ns_set update $form object [self]
    ns_set update $form show_source    [::xo::cc query_parameter show_source:integer 1]
    ns_set update $form show_methods   [::xo::cc query_parameter show_methods:integer 2]
    ns_set update $form show_variables [::xo::cc query_parameter show_variables:integer 1]
    ns_set update $form as_img 1
    rp_internal_redirect /packages/xotcl-core/www/show-object
  } else {
    ns_log error "show-object can only be called with an active connection"
  }
  ad_script_abort
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

::xotcl::Object instproc log args {
  set msg [join $args { }]
  ns_log notice "[self] [self callingclass]->[self callingproc]: $msg ([:__timediff])"
}
::xotcl::Object instproc ds args {
  set msg [join $args { }]
  ds_comment "[self] [self callingclass]->[self callingproc]: $msg ([:__timediff])"
}
::xotcl::Object instproc debug args {
  set msg [join $args { }]
  ns_log debug "[self] [self callingclass]->[self callingproc]: $msg"
}
::xotcl::Object instproc msg {{-html false} args} {
  set msg [join $args { }]
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
    set prefix [lindex [:uplevel {info level 0}] 0]
    #ns_log notice "QN <$query_name> -> PREFIX <$prefix>"
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
    set msg "### template::parse_level <$parse_level> connected=[ns_conn isconnected] "
    if {[ns_conn isconnected]} {
      append msg "flags=[ad_conn flags] status=[ad_conn status] req=[ad_conn request]"
    }
    ns_log notice $msg
    set max [info level]
    if {$m<$max} {set max $m}
    ns_log notice "### Call Stack (level: command)"
    for {set i 0} {$i < $max} {incr i} {
      try {
        set s [uplevel $i self]
      } on error {errorMsg} {
        set s ""
      }
      ns_log notice "### [format %5d -$i]:   $s [info level [expr {-$i}]]"
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
    append _ "NSF:       $::nsf::patchLevel\n"
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
        apm_version_get -package_key $pk -array info
        append _ \
            "[format %-22s $pk:] " \
            "$info(release_date), $info(version_name)" \
            \n
      }
    }
    return $_
  }

  proc pg_version {} {
    #
    # Return 2 digit version number (suitable for number compare
    # operations) from PostgreSQL or 0.0 if not available
    #
    return [acs::per_thread_cache eval -key xotcl-core.pg_version {
      set version 0.0
      if {[db_driverkey {}] eq "postgresql"} {
        set version_string [db_string dbqd.null.get_version {select version() from dual}]
        regexp {PostgreSQL ([0-9]+[.][0-9+])} $version_string . version
      }
      set version
    }]
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
      ns_log Warning "The command 'ns_ictl trace delete' returned: $errorMsg"
    }
  }

  proc ::xo::freeconn {} {
    catch {::xo::at_cleanup}
  }

  #proc ::xo::at_create {} {
  #  ns_log notice "--at_create *********"
  #  foreach i [::xo::InstanceManager array names blueprint] {
  #    if {![nsf::is object $i]} {
  #      ::xo::InstanceManager unset blueprint($i)
  #      ns_log notice "--at_create no such object: $i"
  #    }
  #  }
  #}

  ::xotcl::Object instproc destroy_on_cleanup {} {
    #:log "--cleanup adding ::xo::cleanup([self]) [list [self] destroy]"
    set ::xo::cleanup([self]) [list [self] destroy]
  }

  #
  # Activate/deactivate the following line to track (unexpected)
  # memory size changes in the system log.
  #
  set ::xo::rss 0 ;# set it to one to activate it

  #
  # Experimental low-level cleanup handlers, which are similar to
  # ::xo::cleanup, but which survive per-request cleanup and which
  # have to be manually deregistered.
  #
  proc add_cleanup {key cmd} {
    set ::xo::cleanup_always($key) $cmd
  }
  proc remove_cleanup {key} {
    unset ::xo::cleanup_always($key)
  }

  ad_proc at_cleanup {args} {
    #
    # Per-request cleanup handler. The handler is as well called by
    # the xowiki-datasource and must be therefore public.
    #

  } {
    #
    # The following block is a safety measure: When there is no cleanup
    # for ::xo::cc defined, the object will survive a request and many
    # things might go wrong. The test is quite cheap an can reduce
    # debugging time on some sites.
    #
    if {[nsf::is object ::xo::cc] && ![info exists ::xo::cleanup(::xo::cc)]} {
      ns_log notice [::xo::cc serialize]
      ns_log error "no cleanup for ::xo::cc registered"
      ::xo::cc destroy
    }
    ::xo::dc profile off
    ::xo::broadcast receive

    if {$::xo::rss} {
      #
      # The following code works just for Linux, since it depends on
      # the /proc filesystem and the order of values in the resulting
      # line.
      #
      if {[file readable /proc/[pid]/statm]} {
        set F [open /proc/[pid]/statm]; set c [read $F]; close $F
        lassign $c size rss shared
        set size [format %.2f [expr {$rss * 4.096 / 1048576}]]
        if {$::xo::rss != $size} {
          ns_log notice "=== RSS size change to: $size GB"
          set ::xo::rss $size
        }
      }
    }

    #ns_log notice "*** start of cleanup <$args> ([array get ::xo::cleanup])"
    set at_end ""
    foreach {name cmd} [list {*}[array get ::xo::cleanup] {*}[array get ::xo::cleanup_always]] {
      #::trace remove variable ::xotcl_cleanup($name) unset ::xo::cleanup
      if {![nsf::is object $name]} {
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
          ns_log notice "... analyze: $obj is_object? [nsf::is object $obj]"
          ns_log notice "... analyze: class [$obj info class]"
          ns_log notice "... analyze: precedence [$obj ::nsf::methods::object::info::precedence]"
          ns_log notice "... analyze: methods [lsort [$obj info methods]]"
          #
          # In case, we want to destroy some objects, and the
          # destructor fails, make sure to destroy them even
          # then. Half-deleted zombies can produce harm. We reclass
          # the object to the base class and try again.
          #
          if {[lindex $cmd 1] eq "destroy"} {
            ns_log error "... forcing object destroy without application level destructors"
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
      ns_log Error "Error during ::xo::cleanup: $errorMsg $::errorInfo"
    }
    unset -nocomplain ::xo::cleanup
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
        if {![nsf::is object $o]} continue
        if {[$o istype ::xotcl::Class]} continue
        catch {$o destroy} errorMsg
      }
      foreach o [::xotcl::Class allinstances] {
        if {![nsf::is object $o]} continue
        if {$o eq "::xotcl::Object" || $o eq "::xotcl::Class"} continue
        catch {$o destroy} errorMsg
      }
    }
    set t1 [clock clicks -milliseconds]
    ns_log notice "ON DELETE done ([expr {$t1-$t0}]ms)"
  }

  proc ::xo::stats {{msg ""}} {
    dict set stats xotcl   [llength [::xotcl::Object info instances -closure]]
    dict set stats nx      [llength [::nx::Object info instances  -closure]]
    dict set stats tmpObjs [llength [info commands ::nsf::__#*]]
    dict set stats tdom    [llength [list {*}[info commands domNode0*] {*}[info commands domDoc0x*]]]
    dict set stats nssets  [expr {[acs::icanuse "ns_set stats"] ? [list [ns_set stats]] : [llength [ns_set list]]}]
    ns_log notice "xo::stats $msg: $stats"
    return $stats
  }

  #
  # ::xo::Module is very similar to a plain Tcl namespace: When it is
  # created/recreated, it does not perform a cleanup of its
  # contents. This means that preexisting procs, objects classes,
  # variables etc. will survive a recreation. As a consequence,
  # ::xo::Modules can easily span multiple files and they can be used
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
      if {[ad_file readable $fn]} {
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
    #
    # Get name and tid of the current thread
    #
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
      "-asynclogwriter*" { set group logwriter }
      "-conn:*"          { set group conns   }
      "-driver:*"        { set group drivers }
      "-main-"           { set group main }
      "-ns_job_*"        { set group ns_job }
      "-nsproxy*"        { set group nsproxy }
      "-sched*"          { set group scheds  }
      "-socks-"          { set group socks }
      "-spooler*"        { set group spoolers }
      "-task:tclhttp*"   { set group tclhttp }
      "-writer*"         { set group writers }
      "::*"              { set group tcl:[string range $name 2 end]}
      default            { set group others  }
    }
    return $group
  }

  ::xo::system_stats proc recordtimes {} {
    set threadInfo [:gettid]
    if {$threadInfo ne ""} {
      set i [:thread_info [pid] [dict get $threadInfo tid]]
      lappend i {*}$threadInfo
      if {[dict exists $i stime]} {
        set group [:thread_classify [dict get $i name]]
        nsv_incr [self] $group,stime [dict get $i stime]
        nsv_incr [self] $group,utime [dict get $i utime]
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
    set varnames {utime utimes stime stimes}
    foreach index [nsv_array names [self]] {
      lassign [split $index ,] group kind
      :aggregate $group [dict get $varnames $kind] [nsv_get [self] $index]
    }
    set threadInfo [ns_info threads]
    if {[file readable /proc/$pid/statm] && [llength [lindex $threadInfo 0]] > 7} {
      foreach t $threadInfo {
        set s [:thread_info $pid [lindex $t 7]]
        if {[dict exists $s stime]} {
          set group [:thread_classify [lindex $t 0]]
          :aggregate $group [dict get $varnames utime] [dict get $s utime]
          :aggregate $group [dict get $varnames stime] [dict get $s stime]
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

    set tids {}
    foreach thread_info [ns_info threads] {
      set tn [lindex $thread_info 0]
      set tid [lindex $thread_info 2]
      dict set tids $tid 1
      if { [info exists thread_pattern] && ![string match $thread_pattern $tn] } {
        continue
      }
      switch -glob -- $tn {
        -conn:* -
        -sched:* {
          nsv_lappend broadcast $tid $cmd
        }
      }
    }

    foreach tid [nsv_array names broadcast] {
      if {![dict exists $tids $tid]} {
        nsv_unset broadcast $tid
        ns_log notice "xo::broadcast cleanup of TID $tid (thread does not exist anymore)"
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
          eval $cmd
        } on error {errorMsg} {
          ns_log notice "broadcast receive error: $errorMsg for cmd $cmd"
        }
      }
      :clear
    }
  }
}

# nx::Object create acs::object_property {
#   :public object method mixin {o} {
#     $o ::nsf::methods::object::info::mixins
#   }
#   :public object method instmixin {o} {
#     $o ::nsf::methods::class::info::mixins
#   }
#   :public object method mixinof {o} {
#     $o ::nsf::methods::class::info::mixinof -scope object   
#   }
#   :public object method instmixinof {o} {
#     $o ::nsf::methods::class::info::mixinof -scope class
#   }
#   :public object method instproc {o args} {
#     if {[nsf::is class,type=::xotcl::Class $o]} {return [$o info instprocs {*}$args]}
#     if {[nsf::is class,type=::nx::Class $o]} {return $o info methods -path -type scripted -callprotection all {*}$args}
#   }
#   :public object method instcommand {o args} {
#     if {[nsf::is class,type=::xotcl::Class $o]} {return [$o info instcommands {*}$args]}
#     if {[nsf::is class,type=::nx::Class $o]} {return [$o info methods -path {*}$args]}
#   }
# }

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
    if {[nsf::is object ${c}::slot::$name]} {
      set slot ${c}::slot::$name
      break
    }
  }
  if {![info exists slot]} {error "can't find slot $name"}

  # copy slot and configure it
  set newSlot [self]::slot::$name

  $slot copy $newSlot
  $newSlot configure \
      -domain [self] \
      -manager $newSlot \
      -create_acs_attribute false \
      -create_table_attribute false \
      {*}$config
  #
  # Changing the domain is necessary for "update_attribute_from_slot"
  # for the extended slots like "title", "description" etc. But then
  # the accessor methods (for "title", "description") have to be
  # installed manually for the classes, on which the extension
  # happens.
  #
  ::nsf::method::setter [$newSlot domain] $name
  ns_log notice "=== change domain of $name from [$newSlot domain] to [$slot domain]"
  $newSlot domain [$slot domain]

  #
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
