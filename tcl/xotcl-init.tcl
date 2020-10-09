if {[::acs::icanuse "ns_ictl trace idle"]} {
  #
  # In case the server has (too) many connection threads defined,
  # broadcast messages might pile up. In these situations, the idle
  # callback provides a means to keep these idle connection threads
  # up to date.
  #
  ns_ictl trace idle {
    ns_log notice =====IDLE=====
    ::xo::broadcast receive
  }
}


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
