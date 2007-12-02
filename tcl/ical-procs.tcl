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
  
}