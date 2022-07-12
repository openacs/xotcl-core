::xo::library doc {
  Utility functions for ical data

  @author neumann@wu-wien.ac.at
  @creation-date July 20, 2005

  Incomplete backport from :calendar extensions
}

namespace eval ::xo::ical {}

nx::Object create ::xo::ical {
  # The Object ::calendar::ical provides the methods for
  # importing and exporting single or multiple calendar items
  #  in the ical format (see rfc 2445). Currently only the part
  #  of ical is implemented, which is used by the mozilla
  # calendar (Sunbird, or Lightning for Thunderbird).
  #
  # @author Gustaf Neumann

  :object method debug {msg} {
    #
    # TODO: mabe add Debug(ical)?
    #
    ns_log Debug(caldav) "[uplevel current proc]: $msg"
  }

  #
  # conversion routines from and to the date formats used by ical
  #
  :public object method date_time_to_clock {date time utc} {
    set year  [string range $date 0 3]
    set month [string range $date 4 5]
    set day   [string range $date 6 7]
    set hour  [string range $time 0 1]
    set min   [string range $time 2 3]
    set sec   [string range $time 4 5]
    set TZ [expr {$utc ? "GMT" : ""}]
    return [clock scan "$year-$month-$day $hour:$min $TZ"]
  }

  :public object method tcl_time_to_utc {time} {
    clock format [clock scan $time] -format "%Y%m%dT%H%M%SZ" -gmt 1
  }

  :public object method tcl_time_to_local_day {time} {
    # https://tools.ietf.org/html/rfc5545#section-3.3.4
    return "VALUE=DATE:[:clock_to_local_day [clock scan $time]]"
  }

  :public object method utc_to_clock {utc_time} {
    clock scan $utc_time -format "%Y%m%dT%H%M%SZ" -gmt 1
  }

  :public object method clock_to_utc {seconds:integer} {
    clock format $seconds -format "%Y%m%dT%H%M%SZ" -gmt 1
  }

  :public object method clock_to_iso {seconds:integer} {
    clock format $seconds -format "%Y-%m-%dT%H:%M:%SZ" -gmt 1
  }

  :public object method clock_to_local_day {seconds:integer} {
    clock format $seconds -format "%Y%m%d"
  }

  :public object method clock_to_oacstime {seconds:integer} {
    clock format $seconds -format "%Y-%m-%d %H:%M"
  }

  :public object method dates_valid_p {
    -start_date:required
    -end_date:required
  } {
    # A sanity check that the start time is before the end time.
    # This is a rewrite of calendar::item::dates_valid_p, but
    # about 100 times faster.
    #

    #:log "$start_date <= $end_date = [expr {[clock scan $start_date] <= [clock scan $end_date]}]"
    expr {[clock scan $start_date] <= [clock scan $end_date]}
  }

  :public object method text_to_ical {{-remove_tags:boolean false} text} {
    #
    # Transform arbitrary text to the escaped ical text format
    # (see rfc 2445)
    #

    if {$remove_tags} {
      regsub -all {<[^>]+>} $text "" text
    }
    regsub -all {(\\|\;|\,)} $text {\\\1} text
    regsub -all \n $text {\\n} text
    return $text
  }

  :public object method ical_to_text {text} {
    #
    # Transform the escaped ical text format to plain text
    #
    regsub -all {\\(n|N)} $text \n text
    regsub -all {\\(\\|\;|\,)} $text {\1} text
    return $text
  }

  :public object method reflow_content_line {text} {
    #
    # Perform line folding: According to RFC 5545 section 3.1,
    # SHOULD NOT be longer than 75 octets, excluding the line break.
    # https://www.ietf.org/rfc/rfc5545.txt
    #
    if {[string length $text] > 73} {
      set lines ""
      while {[string length $text] > 73} {
        append lines [string range $text 0 73] \r\n " "
        set text [string range $text 74 end]
      }
      append lines $text
      set text $lines
    }
    return $text
  }
}

namespace eval ::xo {
  nx::Class create ::xo::ical::VCALITEM {
    #
    # VCALITEM is the superclass of a VTODO and a VEVENT, intended as
    # an abstract class.
    #

    :property -accessor public creation_date
    :property -accessor public last_modified
    :property -accessor public dtstart
    :property -accessor public dtstamp
    :property -accessor public uid
    :property -accessor public priority
    :property -accessor public summary
    :property -accessor public url
    :property -accessor public description
    :property -accessor public location
    :property -accessor public geo
    :property -accessor public status
    :property -accessor public {is_day_item false}
    :property -accessor public last-modified   ;# TODO: who (which client) needs this? not a standard ical attribute

    :public method get {property {default ""}} {
	#
	# Return a certain property of an ical items. In case, the
	# item has no such property, return the default value.
	#
	if {[info exists :$property]} {
	    return [set :$property]
	}
	return $default
    }

    :method tag {-tag -param -conv -value slot:required} {
      #
      # Generate a single (line) entry in ical format starting with
      # the specified tag. If no "tag" is specified, the tag name is
      # uppercase of the slot.  The value might be specified literally
      # or it can be the name of an instance variable (named by the
      # "slot" attribute".
      #
      # The functions returns empty (produces no output) when either
      # the value is empty or no instance variable exists.
      # Optionally, the value can be converted.
      #
      #

      #ns_log notice "::xo::ical::VCALITEM tag [self args]"
      if {![info exists tag]} {
        set tag [string toupper $slot]
      }

      #
      # if we get a param, we expect it to include delimiters, such
      # as the leading semicolon
      #
      if {[info exists param]} {
        append tag $param
      }

      if {![info exists value]} {
        if {[info exists :$slot]} {
          set value [set :$slot]
        } else {
          #
          # If we have no value and no instance variable set, do not
          # output this slot.
          #
          return ""
        }
      }

      #
      # When a converter is given, apply it
      #
      if {[info exists conv]} {
        set value [::xo::ical $conv $value]
      }

      if {$value ne ""} {
        set result [::xo::ical reflow_content_line "$tag:$value"]
        append result \r\n
        #ns_log notice "::xo::ical::VCALITEM tag [self args] -> len: [string length $result]"
      } else {
        set result ""
      }
      return $result
    }

    :method start_end {} {
      #
      # Output either a DAY-EVENT (denoted by a single DTSTART with
      # appropriate VALUE format) or a state and end time stamps.
      #
      if {${:is_day_item}} {
        append result \
            "DTSTART;" [::xo::ical tcl_time_to_local_day ${:dtstart}] \r\n
      } else {
        append result \
            [:tag -conv tcl_time_to_utc dtstart] \
            [:tag -conv tcl_time_to_utc dtend]
      }
    }

    :public method as_ical {} {
      #
      # Output a VCALITEM (VTODO or VEVENT) "object" in ical notation.
      #
      set item_type [namespace tail [:info class]]
      append t "BEGIN:$item_type\r\n" \
          [:ical_body] \
          "END:$item_type\r\n"
      return $t
    }

    :public method ical_body {} {
      #
      # The method ical_body returns the ical-formatted content of the
      # variables. All variables of VEVENTs and VTODOs are listed below,
      # since the names are distinct, and no methods are used.
      #
      # So far there is no handling for the repetition fields (which
      # might occur more than once). An option would be to handle these
      # as lists.
      #
      #
      # All date/time stamps are provided either by
      # the ANSI date (from postgres) or by a date
      # which can be processed via clock scan
      #

      if {![info exists :dtstamp]}       {set :dtstamp ${:creation_date}}
      if {![info exists :last_modified]} {set :last_modified ${:dtstamp}}

      set tcl_stamp         [::xo::db::tcl_date ${:dtstamp} tz]
      set tcl_creation_date [::xo::db::tcl_date ${:creation_date} tz]
      set tcl_last_modified [::xo::db::tcl_date ${:last_modified} tz]

      # status values:
      #    VEVENT:   TENTATIVE, CONFIRMED, CANCELLED
      #    VTODO:    NEEDS-ACTION, COMPLETED, IN-PROCESS, CANCELLED
      #    VJOURNAL: DRAFT, FINAL, CANCELLED

      append t  \
          [:tag -conv tcl_time_to_utc -value $tcl_creation_date created] \
          [:tag -conv tcl_time_to_utc -value $tcl_last_modified last-modified] \
          [:tag -conv tcl_time_to_utc -value $tcl_stamp dtstamp] \
          [:start_end] \
          [:tag -conv tcl_time_to_utc completed] \
          [:tag -conv tcl_time_to_utc percent-complete] \
          [:tag transp] \
          [:tag uid] \
          [:tag url] \
          [:tag geo] \
          [:tag priority] \
          [:tag sequence] \
          [:tag CLASS] \
          [:tag -conv text_to_ical location] \
          [:tag status] \
          [:tag -conv text_to_ical description] \
          [:tag -conv text_to_ical summary] \
          [:tag -conv tcl_time_to_utc due]

      if {[info exists :formatted_recurrences]} {
        append t ${:formatted_recurrences}
      }
      return $t
    }

    :public method "parse RRULE" {recurrule} {
      #
      # parse recurrence rule provided in cal syntax. This method
      # assumes that the instance variable dtstart is already set,
      # before this method is called.
      #
      set r_freq ""
      set every_n 1
      set r_error 0
      set r_until ""
      set days_of_week ""
      set r_count 0
      foreach rval [split $recurrule ";"] {
        if { [regexp {^FREQ\=+(.*)$} $rval _ freqval] } {
          switch $freqval {
            DAILY   { set r_freq "day" }
            WEEKLY  { set r_freq "week" }
            MONTHLY { set r_freq "month_by_day"}
            YEARLY  { set r_freq "year"}
            default { set r_error 1 }
          }
        } elseif { [regexp {^COUNT=(.*)$} $rval _ countval] } {
          set r_count $countval
        } elseif { [regexp {^UNTIL=([0-9]+)(T([0-9]+)Z?)?$} $rval _ untildate untiltime] } {
          if {$untiltime eq ""} {
            set untiltime 000000
          }
          append r_until \
              "[string range $untildate 0 3]-[string range $untildate 4 5]-[string range $untildate 6 7]" \
              " " \
              "[string range $untiltime 0 1]:[string range $untiltime 2 3]"
        } elseif { [regexp {^INTERVAL\=+(.*)$} $rval _ intval] } {
          set every_n $intval
        } elseif { [regexp {^BYDAY\=+(.*)$} $rval _ bydayval] } {
          #
          # build days_of_week list
          #
          foreach dayval [split $bydayval ","] {
            switch $dayval {
              SU { lappend days_of_week "0" }
              MO { lappend days_of_week "1" }
              TU { lappend days_of_week "2" }
              WE { lappend days_of_week "3" }
              TH { lappend days_of_week "4" }
              FR { lappend days_of_week "5" }
              SA { lappend days_of_week "6" }
            }
          }
        } elseif { [regexp {^BYMONTHDAY\=+(.*)$} $rval _ bymonthdayval] } {
          set r_freq "month_by_date"
        } else {
          # other rules don't work with OpenACS recurrence model
          :debug "ignore recurrence rule <$rval> of <$recurrule>"
        }
        #check we can make this rule, else ignore
      }

      #
      # We should have now 'r_freq' computed.  If 'r_until' is not
      # provided, calculate it based on 'r_count' (COUNT is not
      # directly supported by OpenACS).  If both UNTIL and COUNT are
      # not set, it is an unlimited event and skipped.
      #
      if { $r_until eq "" && $r_freq ne "" && $r_count > 0 } {
        # current date + r_count * r_freq * every_n (/ num_days)
        # set num seconds per frequency
        switch $r_freq {
          day           { set r_freq_amount 86400 }
          week          { set r_freq_amount 604800 }
          month_by_day  { set r_freq_amount 2419200 }
          month_by_date { set r_freq_amount 2678400 }
          year          { set r_freq_amount 31449600 }
        }
        # start date is count=1, so adjust count
        set r_count [expr {$r_count - 1}]
        set r_extra [expr {$r_count * $r_freq_amount * $every_n}]
        if { $r_freq eq "week" && [llength $days_of_week] > 0} {
          set r_extra [expr {$r_extra / [llength $days_of_week]}]
        }
        set r_until [::xo::ical clock_to_oacstime [expr {[clock scan ${:dtstart}] + $r_extra}]]
      }

      #
      # If we have no errors, and 'r_freq' is computed, then keep the
      # computed values in form of a parameter list for
      # calendar::item::add_recurrence
      #
      if { !$r_error && $r_freq ne ""} {
        set :recurrence_options [list -interval_type $r_freq -every_n $every_n]
        if {$days_of_week ne ""} {
          lappend :recurrence_options -days_of_week $days_of_week
        }
        if {$r_until ne ""} {
          lappend :recurrence_options -recur_until $r_until
        }
      }
    }

    :public method add_recurrence {-cal_item_id:integer} {
      #
      # Call calendar::item::add_recurrence with the options
      # calculated by "parse RRULE"
      #
      if { [info exists :recurrence_options] } {
        calendar::item::add_recurrence \
            -cal_item_id $cal_item_id \
            {*}${:recurrence_options}
      }
    }

    :public method edit_recurrence {
      -cal_item_id:integer
      -recurrence_id:integer,0..1
    } {
      #
      # We might or might not have a recurrence on the old entry.
      # In case we have one, the old one might have started earlier,
      # so we try to terminate it.
      :debug "do we have a recurrence? [info exists :recurrence_options]"
      if { [info exists :recurrence_options] } {
        if {$recurrence_id ne ""} {

          #
          # The current implementation of calendar::item::edit_recurrence is
          # just built for the interface of the calendar package, which does
          # not allow one to change multiple attributes. We have to assume, that a
          # calendar client allows this.
          #
          # When the old start date is the same as the new start date,
          # we could delete the recurrence and add a new one.
          #
          # calendar::item::delete_recurrence -recurrence_id $recurrence_id

          lassign [::xo::dc list -prepare integer get_old_start_date_and_event_id {
            select start_date, e.event_id
            from  acs_events e, timespans t, time_intervals i
            where recurrence_id = :recurrence_id
            and   e.timespan_id = t.timespan_id
            and   i.interval_id = t.interval_id
            order by 1 limit 1
          }] old_start_date old_event_id

          :debug "recurrence_id $recurrence_id old_event_id $old_event_id cal_item_id $cal_item_id \
                old_start_date $old_start_date new start_date ${:dtstart} opts ${:recurrence_options}"

          ::xo::db::sql::acs_event recurrence_timespan_edit \
              -event_id $old_event_id \
              -start_date $old_start_date \
              -end_date ${:dtstart}
        }
        calendar::item::add_recurrence \
            -cal_item_id $cal_item_id \
            {*}${:recurrence_options}
      }
    }

    #
    # End of class definition of ::xo::ical::VCALITEM
    #
  }

  nx::Class create ::xo::ical::VTODO -superclass ::xo::ical::VCALITEM {
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

    :property -accessor public due
    :property -accessor public completed
    :property -accessor public percent-complete
  }



  nx::Class create ::xo::ical::VEVENT -superclass ::xo::ical::VCALITEM {
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

    :property -accessor public dtend
    :property -accessor public sequence
    :property -accessor public transp
    :property -accessor public formatted_recurrences

    :public method finish {parse_error} {
      #
      # In case, there was no :dtend given, set it either to the value
      # of :dtstart, or :dtstart + :duration
      #
      if {![info exists :dtend]} {
        set end_clock [clock scan ${:dtstart}]
        if {[info exists :duration]} {
          incr end_clock ${:duration}
        }
        set :dtend [::xo::ical clock_to_oacstime $end_clock]
      }
      # TODO: not sure, for what purpose we need parse_error
      incr $parse_error $parse_error
    }
  }

  #
  # This class is designed to be a mixin for an ordered composite
  #
  Class create ::xo::ical::VCALENDAR -parameter {prodid version method}
  ::xo::ical::VCALENDAR instproc as_ical {} {
    if {[info exists :prodid]}  {set prodid  "PRODID:[:prodid]\n"} {set prodid ""}
    if {[info exists :method]}  {set method  "METHOD:[string toupper [:method]]\n"} {set method ""}
    if {[info exists :version]} {set version "VERSION:[:version]\n"} {set version "VERSION:2.0\n"}
    set t ""
    append t "BEGIN:VCALENDAR\n" $prodid $version $method
    foreach i [:children] {
      append t [$i as_ical]
    }
    append t "END:VCALENDAR\n"
    return $t
  }
  
  #
  # Subclass ::xo::ProtocolHandler for dav (as used by ical)
  #
  Class create ::xo::dav -superclass ProtocolHandler -parameter {
    {url /webdav}
  }
}

::xo::library source_dependent

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
#    eval: (setq tcl-type-alist (remove* "method" tcl-type-alist :test 'equal :key 'car))
# End:
