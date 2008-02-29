namespace eval ::xotcl {}

ad_proc ::xotcl::before_install_callback {} {
  
  Callback for checking whether xotcl is installed for OpenACS
  
  @author Gustaf Neumann (neumann@wu-wien.ac.at)
} {
  ns_log notice "-- before-install callback"
  if {[info command ::xotcl::Class] eq ""} {
    error " XOTcl does not appear to be installed on your system!\n\
     Please follow the install instructions on http://www.openacs.org/xowiki/xotcl-core"
  } elseif {$::xotcl::version < 1.5} {
    error " XOTcl 1.5 or newer required. You are using $::xotcl::version$::xotcl::patchlevel.\n\
	Please install a new version of XOTcl (see http://www.openacs.org/xowiki/xotcl-core)"
  } else {
    ns_log notice "XOTcl $::xotcl::version$::xotcl::patchlevel is installed on your system."
  }
}
