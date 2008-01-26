# $Id$
if {$::xotcl::version < 1.5} {
  ns_log notice "**********************************************************"
  ns_log notice "This version of xotcl-core requires at least XOTcl 1.5.0."
  ns_log notice "The installed version ($::xotcl::version$::xotcl::patchlevel appears to be older."
  ns_log notice "Please updgrade to a new version (see http://openacs.org/xowiki/xotcl-core)"
  ns_log notice "**********************************************************"
}
