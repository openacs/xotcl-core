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

  ad_proc -public write_tmp_file {varFilename content} {
    Write binary content to a temporary file and set its path in the caller.
    @param varFilename Name of the caller variable to receive the path.
    @param content Bytes to write.
  } {
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
