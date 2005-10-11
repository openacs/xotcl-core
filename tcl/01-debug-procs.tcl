# tell serializer to export methods, allthough these are methods of ::xotcl::Object
::Serializer exportMethods {
  ::xotcl::Object instproc log 
  ::xotcl::Object instproc debug
}

::xotcl::Object instproc log msg {
  ns_log notice "[self] $msg"
}
::xotcl::Object instproc debug msg {
  ns_log debug "[self] $msg"
}