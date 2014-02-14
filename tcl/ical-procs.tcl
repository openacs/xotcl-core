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
    calendar (Sunbird, or Lightning for Thunderbird).
    
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
  ical proc tcl_time_to_utc {time} {
    clock format [clock scan $time] -format "%Y%m%dT%H%M%SZ" -gmt 1
  }
  ical proc tcl_time_to_local_day {time} {
    VALUE=DATE:[my clock_to_local_day [clock scan $time]]
  }
  ical proc utc_to_clock {utc_time} {
    clock scan $utc_time -format "%Y%m%dT%H%M%SZ" -gmt 1
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
  #
  # VCALITEM is the superclass of a VTODO and a VEVENT, intended as an
  # abstract class
  #
  Class create ::xo::ical::VCALITEM -parameter {
    creation_date
    last_modified
    dtstart
    dtstamp
    uid
    priority
    summary
    url
    description
    location
    geo
    status
    {is_day_item false}
  }

  ::xo::ical::VCALITEM instproc tag {-tag -conv -value slot} {
    if {![info exists tag]} {
      set tag [string toupper $slot]
    }
    if {![info exists value]} {
      if {[my exists $slot]} {
    set value [my $slot]
      } else {
    return ""
      }
    }
    if {[info exists conv]} {
      return "$tag:[::xo::ical $conv $value]\r\n"
    } else {
      return "$tag:$value\r\n"
    }
    return ""
  }

  ::xo::ical::VCALITEM instproc start_end {} {
    if {[my is_day_item]} {
      append result \
      [my tag -conv tcl_time_to_local_day dtstart] \
      [my tag -conv tcl_time_to_local_day dtend]
    } else {
      append result \
      [my tag -conv tcl_time_to_utc dtstart] \
      [my tag -conv tcl_time_to_utc dtend]
    }
  }

  ::xo::ical::VCALITEM instproc as_ical {} {
    set item_type [namespace tail [my info class]]
    append t "BEGIN:$item_type\r\n" \
    [my ical_body] \
    "END:$item_type\r\n"
    return $t
  }

  ::xo::ical::VCALITEM instproc ical_body {} {
    #
    # The method ical_body returns the ical-formatted content of the
    # variables. All variables of VEVENTs and VTODOs are listed below,
    # since the names are distinct, and no methods are used.
    #
    # So far there is no handling for the repetition fields (which
    # might occur more than once). An option would be to handle these
    # as lists.
    #
    my instvar creation_date last_modified dtstamp
    #
    # All date/time stamps are provided either by 
    # the ANSI date (from postgres) or by a date
    # which can be processed via clock scan
    #
    if {![info exists dtstamp]}       {set dtstamp $creation_date}
    if {![info exists last_modified]} {set last_modified $dtstamp}
    
    set tcl_stamp         [::xo::db::tcl_date $dtstamp tz]
    set tcl_creation_date [::xo::db::tcl_date $creation_date tz]
    set tcl_last_modified [::xo::db::tcl_date $last_modified tz]

    # status values: 
    #    VEVENT:   TENTATIVE, CONFIRMED, CANCELLED
    #    VTODO:    NEEDS-ACTION, COMPLETED, IN-PROCESS, CANCELLED
    #    VJOURNAL: DRAFT, FINAL, CANCELLED

    append t  \
    [my tag -conv tcl_time_to_utc -value $tcl_creation_date created] \
    [my tag -conv tcl_time_to_utc -value $tcl_last_modified last-modified] \
    [my tag -conv tcl_time_to_utc -value $tcl_stamp dtstamp] \
    [my tag -conv tcl_time_to_utc dtstart] \
    [my tag -conv tcl_time_to_utc dtend] \
    [my tag -conv tcl_time_to_utc completed] \
    [my tag -conv tcl_time_to_utc percent-complete] \
    [my tag transp] \
    [my tag uid] \
    [my tag url] \
    [my tag geo] \
    [my tag priority] \
    [my tag sequence] \
    [my tag CLASS] \
    [my tag location] \
    [my tag status] \
    [my tag -conv text_to_ical description] \
    [my tag -conv text_to_ical summary] \
    [my tag -conv tcl_time_to_utc due]
    
    if {[my exists formatted_recurrences]} {
      append t [my set formatted_recurrences]
    }
    return $t
  }

  #
  # VTODO
  #
  # optional fields, must not occur more than once
  #
  #           class / *completed / *created / *description / *dtstamp /
  #           *dtstart / *geo / *last-mod / *location / organizer /
  #           *percent-complete / *priority / recurid / seq / *status /
  #           *summary / *uid / *url /
  #
  # optional, but mutual exclusive
  #           *due / duration /
  #
  # optional fields, may occur more than once
  #
  #           attach / attendee / categories / comment / contact /
  #           exdate / exrule / rstatus / related / resources /
  #           rdate / rrule / x-prop

  Class create ::xo::ical::VTODO -superclass ::xo::ical::VCALITEM -parameter {
    due
    completed
    percent-complete
  }
  #
  # VEVENT
  #
  # optional fields, must not occur more than once
  #
  #          class / *created / *description / *dtstart / *geo /
  #          *last-mod / *location / organizer / *priority /
  #          *dtstamp / seq / *status / *summary / transp /
  #          *uid / *url / recurid /
  #
  # dtend or duration may appear, but dtend and duration are mutual exclusive
  #           *dtend / duration /
  #
  # optional fields, may occur more than once
  #
  #           attach / attendee / categories / comment / contact /
  #           exdate / exrule / rstatus / related / resources /
  #           rdate / rrule / x-prop
  #
  # just a stub for now
  Class create ::xo::ical::VEVENT -superclass ::xo::ical::VCALITEM -parameter {
    dtend
    sequence
    transp
    formatted_recurrences
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

  #
  # Subclass ::xo::ProtocolHander for dav (as used by ical)
  #
  Class create ::xo::dav -superclass ProtocolHandler -parameter {
    {url /webdav}
  }

}

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
