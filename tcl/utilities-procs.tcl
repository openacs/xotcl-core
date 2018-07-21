::xo::library doc {

  XoTcl - Utility procs

}

namespace eval ::xo {

  proc read_file {fn} {
    set F [open $fn]
    fconfigure $F -translation binary
    set content [read $F]
    close $F
    return $content
  }
  
  proc write_file {fn content} {
    set F [::open $fn w]
    ::fconfigure $F -translation binary -encoding binary
    ::puts -nonewline $F $content
    ::close $F
  }    
    
  nsf::proc ::xo::get_raw_request_body {-as_string:switch -as_file:switch} {
    if {$as_string eq $as_file} {
      error "either -as_string or -as_file must be specified"
    }
    set contentfile [ns_conn contentfile]
    if {$as_file} {
      #
      # If the file was not spooled, obtained it via [ns_conn content]
      # as write it to a file.
      #
      if {$contentfile eq ""} {
        set contentfile [ad_tmpnam]
        write_file $contentfile [ns_conn content -binary]
      }
      set result $contentfile
    } else {
      #
      # Return the result as a string
      #
      if {$contentfile eq ""} {
        set result [ns_conn content -binary]
      } else {
        set result [read_file $contentfile]
      }
    }
    return $result
  }

}
