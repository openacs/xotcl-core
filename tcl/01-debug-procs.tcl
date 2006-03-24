# tell serializer to export methods, allthough these are methods of 
# ::xotcl::Object
::Serializer exportMethods {
  ::xotcl::Object instproc log 
  ::xotcl::Object instproc debug
  ::xotcl::Object instproc contains
}

::xotcl::Object instproc contains cmds {
  my requireNamespace
  namespace eval [self] $cmds
}

::xotcl::Object instproc log msg {
  ns_log notice "[self] [self callingclass]->[self callingproc]: $msg"
}
::xotcl::Object instproc debug msg {
  ns_log debug "[self] [self callingclass]->[self callingproc]: $msg"
}

# ::xotcl::Class instproc import {class pattern} {
#   namespace eval [self] [list \
#   namespace import [list import [$class self]]::$pattern;
#     my log "--namespace [list import [$class self]]::$pattern"
#   ]
# }

# ::xotcl::Class instproc export args {
#   my log "--namespace eval [self] {eval namespace export $args}"
#   namespace eval [self] [list eval namespace export $args]
# }

#ns_log notice "--T [info command ::ttrace::isenabled]"
# tell ttrace to put these to the blueprint
#if {[info command ::ttrace::isenabled] ne "" && [::ttrace::isenabled]} {
#  ns_log notice "--T :ttrace::isenabled"
#  set blueprint [ns_ictl get]
#  ns_ictl save [append blueprint [::Serializer serializeExportedMethods \
#				      [::Serializer new -volatile]]]
#  unset blueprint
#  ns_log notice "--T [ns_ictl get]"
#}

