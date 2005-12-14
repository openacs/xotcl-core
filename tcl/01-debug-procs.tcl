# tell serializer to export methods, allthough these are methods of ::xotcl::Object
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