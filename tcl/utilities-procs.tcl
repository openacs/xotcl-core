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


}
