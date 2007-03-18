namespace eval ::xotcl {}

ad_proc ::xotcl::before_install_callback {} {
  
  Callback for checking whether xotcl is installed for OpenACS
  
  @author Gustaf Neumann (neumann@wu-wien.ac.at)
} {
  ns_log notice "-- before-install callback"
  if {[info command ::xotcl::Class] eq ""} {
    error " XOTcl does not appear to be installed on your system!\n\
     Please follow the install instructions on http://www.openacs.org/xowiki/pages/en/xotcl-core"
  } else {
    ns_log notice "XOTcl [package require XOTcl] appears to be installed on your system."
  }
}
