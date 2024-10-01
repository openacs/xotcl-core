::xo::library doc {

  XOTcl - Utility procs for file I/O. Should not be necessary on the
  longer run.

  @author Gustaf Neumann
}

namespace eval ::xo {

  proc read_file {fn} {
    set F [open $fn]
    ::fconfigure $F -translation binary
    ::set content [read $F]
    ::close $F
    return $content
  }

  proc write_file {fn content} {
    set F [::open $fn w]
    ::fconfigure $F -translation binary
    ::puts -nonewline $F $content
    ::close $F
  }

  proc write_tmp_file {varFilename content} {
    upvar $varFilename tmpFileName
    set F [file tempfile tmpFileName]
    fconfigure $F -translation binary
    puts -nonewline $F $content
    close $F
  }
}

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
