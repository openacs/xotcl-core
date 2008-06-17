ad_library {
    Utility functions for ical data 
    
    @author neumann@wu-wien.ac.at
    @creation-date July 20, 2005

  Incomplete backport from my calendar extensions
}

namespace eval ::xo {
  ::xotcl::Object ::xo::ical -ad_doc {
    The Object ::calendar::ical provides the methods for 
    importing and exporting single or multiple calendar items
    in the ical format (see rfc 2445). Currently only the part
    of ical is implemented, which is used by the mozilla
    calendar (sunbird, or the xul-file for thunderbird or firefox).
    
    @author Gustaf Neumann
  }


  #
  # conversion routines from and to the date formats used by ical
  #
  ical proc date_time_to_clock {date time utc} {
    set year  [string range $date 0 3]
    set month [string range $date 4 5]
    set day   [string range $date 6 7]
    set hour  [string range $time 0 1]
    set min   [string range $time 2 3]
    set sec   [string range $time 4 5]
    set TZ [expr {$utc ? "GMT" : ""}]
    return [clock scan "$year-$month-$day $hour:$min $TZ"]
  }
  ical proc clock_to_utc {seconds} {
    clock format $seconds -format "%Y%m%dT%H%M%SZ" -gmt 1
  }
  ical proc clock_to_iso {seconds} {
    clock format $seconds -format "%Y-%m-%dT%H:%M:%SZ" -gmt 1
  }
  ical proc clock_to_local_day {seconds} {
    clock format $seconds -format "%Y%m%d"
  }
  ical proc clock_to_oacstime {seconds} {
    clock format $seconds -format "%Y-%m-%d %H:%M" 
  }
  
  ical ad_proc dates_valid_p {
    -start_date:required
    -end_date:required
  } {
    A sanity check that the start time is before the end time.
    This is a rewrite of calendar::item::dates_valid_p, but
    about 100 times faster.
  } {
    #my log "$start_date <= $end_date = [expr {[clock scan $start_date] <= [clock scan $end_date]}]"
    expr {[clock scan $start_date] <= [clock scan $end_date]}
  }

  ical ad_proc text_to_ical {{-remove_tags false} text} {
    Transform arbitrary text to the escaped ical text format 
    (see rfc 2445)
  } {
    if {$remove_tags} {regsub -all {<[^>]+>} $text "" text}
    regsub -all \n $text \\n text
    regsub -all {(\\|\;|\,)} $text {\\\1} text
    return $text
  }
  ical ad_proc ical_to_text {text} {
    Transform the escaped ical text format to plain text
  } {
    regsub -all {\\(n|N)} $text \n text
    regsub -all {\\(\\|\;|\,)} $text {\1} text
    return $text
  }
  
}

namespace eval ::xo {
  Class create ::xo::ical::VCALITEM -parameter {
    creation_date
    last_modified
    due
    stamp
    uid
    priority
    summary
    url
    description
  }
  ::xo::ical::VCALITEM instproc as_ical {} {
    my instvar creation_date last_modified stamp uid

    if {![info exists stamp]}         {set stamp $creation_date}
    if {![info exists last_modified]} {set last_modified $stamp}
    
    set tcl_stamp         [::xo::db::tcl_date $stamp tz]
    set tcl_creation_date [::xo::db::tcl_date $creation_date tz]
    set tcl_last_modified [::xo::db::tcl_date $last_modified tz]
    set dtstamp   [::xo::ical clock_to_utc [clock scan $tcl_stamp]]
    set dtlastmod [::xo::ical clock_to_utc [clock scan $tcl_last_modified]]
    set dtcreated [::xo::ical clock_to_utc [clock scan $tcl_creation_date]]

    if {[my exists due]} {
      set dtdue [::xo::ical clock_to_utc [clock scan [my due]]]
      set due "DUE:$dtdue\n"
    } else {
      set due ""
    }

    if {[my exists url]}      {set url "URL:[my url]\n"} {set url ""}
    if {[my exists priority]} {set url "PRIORITY:[my priority]\n"} {set priority ""}
    if {[my exists description]} {set description "DESCRIPTION:[::xo::ical text_to_ical [my description]]\n"} {set description ""}
    if {[my exists summary]}  {set summary "SUMMARY:[::xo::ical text_to_ical [my summary]]\n"} {set summary ""}
#STATUS:IN-PROCESS
#PERCENT-COMPLETE:25
    set item_type [namespace tail [my info class]]
    append t "BEGIN:$item_type\n" \
        "CREATED:$dtcreated\n" \
        "LAST-MODIFIED:$dtlastmod\n" \
        "DTSTAMP:$dtstamp\n" \
        "UID:$uid\n" \
        $due $summary $url $description $priority \
        "END:$item_type\n"
    return $t
  }
  Class create ::xo::ical::VTODO -superclass ::xo::ical::VCALITEM -parameter {
  }
  # just a stub for now
  Class create ::xo::ical::VEVENT -superclass ::xo::ical::VCALITEM -parameter {
  }  

  #
  # This class is designed to be a mixin for an ordered composite
  #
  Class create ::xo::ical::VCALENDAR -parameter {prodid version method}
  ::xo::ical::VCALENDAR instproc as_ical {} {
    if {[my exists prodid]}  {set prodid  "PRODID:[my prodid]\n"} {set prodid ""}
    if {[my exists method]}  {set method  "METHOD:[string toupper [my method]]\n"} {set method ""}
    if {[my exists version]} {set version "VERSION:[my version]\n"} {set version "VERSION:2.0\n"}
    set t ""
    append t "BEGIN:VCALENDAR\n" $prodid $version $method
    foreach i [my children] {
      append t [$i as_ical]
    }
    append t "END:VCALENDAR\n"
    return $t
  }

}

namespace eval ::xo {
  Class create dav -parameter {
    {url /webdav}
    {package}
  }

  dav ad_instproc unknown {method args} {
    Return dav specific connection info similar to ad_conn
  } {
    my log "--dav unknown called with '$method' <$args>"
    switch [llength $args] {
      0 {if {[my exists $method]} {return [my set method]}
        return [ad_conn $method]
      }
      1 {my set method $args}
      default {my log "--dav ignoring <$method> <$args>"}
    }
  }

  dav ad_instproc set_user_id {} {
    Set user_id based on authentication header
  } {
    set ah [ns_set get [ns_conn headers] Authorization]
    if {$ah ne ""} {
      # should be something like "Basic 29234k3j49a"
      my debug "auth_check authentication info $ah"
      # get the second bit, the base64 encoded bit
      set up [lindex [split $ah " "] 1]
      # after decoding, it should be user:password; get the username
      set user [lindex [split [ns_uudecode $up] ":"] 0]
      set password [lindex [split [ns_uudecode $up] ":"] 1]
      array set auth [auth::authenticate \
                          -username $user \
                          -authority_id [::auth::get_register_authority] \
                          -password $password]
      my debug "auth $user $password returned [array get auth]"
      if {$auth(auth_status) ne "ok"} {
        array set auth [auth::authenticate \
                            -email $user \
                            -password $password]
        if {$auth(auth_status) ne "ok"} {
          my debug "auth status $auth(auth_status)"
          ns_returnunauthorized
          my set user_id 0
          return 0
        }
      }
      my debug "auth_check user_id='$auth(user_id)'"
      ad_conn -set user_id $auth(user_id)
      
    } else {
      # no authenticate header, anonymous visitor
      ad_conn -set user_id 0
      ad_conn -set untrusted_user_id 0
    }
    my set user_id [ad_conn user_id]
  }

  dav ad_instproc initialize {} {
    Setup connection object and authenticate user
  } {
    my instvar uri method urlv destination
    ad_conn -reset
    set uri [ns_urldecode [ns_conn url]]
    set dav_url_regexp "^[my url]"
    regsub $dav_url_regexp $uri {} uri
    if {$uri eq ""} {
      set uri "/"
    }
    my set_user_id

    set method [string toupper [ns_conn method]]
    #my log "--dav conn_setup: uri '$uri' method $method"
    set urlv [split [string trimright $uri "/"] "/"]
    set destination [ns_urldecode [ns_set iget [ns_conn headers] Destination]]
    regsub {https?://[^/]+/} $destination {/} dest
    regsub $dav_url_regexp $dest {} destination
    #my log "--dav conn_setup: destination = $destination"
  }

  dav ad_instproc preauth { args } {
    Check if user_id has permission to perform the WebDAV method on
    the URI
  } {
    #my log "--dav preauth args=<$args>"
    my instvar user_id 
    
    # Restrict to SSL if required
    if { [security::RestrictLoginToSSLP]  && ![security::secure_conn_p] } {
      ns_returnunauthorized
      return filter_return
    }
  
    # set common data for all kind of requests 
    my initialize

    # for now, require for every user authentification
    if {$user_id == 0} {
      ns_returnunauthorized
      return filter_return
    }
    
    #my log "--dav preauth filter_ok"
    return filter_ok    
  }

  dav ad_instproc register { } {
    Register the the aolserver filter and traces.
    This method is typically called via *-init.tcl.
  } {
    set filter_url [my url]*
    set url [my url]/*
    foreach method {
      GET HEAD PUT MKCOL COPY MOVE PROPFIND PROPPATCH
      DELETE LOCK UNLOCK
    } {
      ns_register_filter preauth $method $filter_url  [self]
      ns_register_proc $method $url [self] handle_request
      #my log "--dav ns_register_filter preauth $method $filter_url  [self]"
      #my log "--dav ns_register_proc $method $url [self] handle_request"
    }
  }

  dav instproc GET {} {
    my instvar uri
    my log "--dav handle_request GET method"
    #set with_recurrences [ns_queryget with_recurrences 1]
    # ...
    ns_return 200 text/plain GET-$uri
  }
  dav instproc PUT {} {
    my log "--dav handle_request PUT method [ns_conn content]"
    #set calendar_id_list [ns_queryget calendar_id_list 0]
    #if {[llength $write_calendar_ids] == 0} {
    #ns_return 403 text/plain "no permissions to write to calendar"
    #} else {
    ns_return 201 text/plain "0 items processed"
    #}
  }
  dav instproc PROPFIND {} {
    my log "--dav PROPFIND [ns_conn content]"
    ns_return 204 text/xml {<?xml version="1.0" encoding="utf-8" ?>}
  }

  dav ad_instproc get_package_id {} {
    initialize the given package 
    @return package_id 
  } {
    my instvar uri package
    $package initialize -url $uri
    #my log "--dav [my package] initialize -url $uri"
    return $package_id
  }

  dav ad_instproc handle_request { args } {
    Process the incoming web-dav request. This method
    could be overloaded by the application and
    dispatches the HTTP requests.
  } {
    my instvar uri method user_id
  
    #my log "--dav handle_request method=$method uri=$uri\
    #	userid=$user_id -ns_conn query '[ns_conn query]'"
    if {[my exists package]} {
      my get_package_id
    }
    if {[my procsearch $method] ne ""} {
      my $method
    } else {
      ns_return 404 text/plain "not implemented"
    }
  }
}
