ad_library {
    Test the availability of xotcl
}


aa_register_case -cats {api smoke} check_xotcl {
    Basic test of the availability of xotcl
} {
  proc ? {cmd expected {msg ""}} {
                                  set r [uplevel $cmd]
                                  if {$msg eq ""} {set msg $cmd}
                                  aa_true $msg [expr {$r eq $expected}]
                                  #if {$r ne $expected} {
                                  #  test errmsg "$msg returned '$r' ne '$expected'"
                                  #} else {
                                  #  test okmsg "$msg - passed ([t1 diff] ms)"
                                  #}
                                }
  ? {expr {$::xotcl::version < 1.4}} 0 "XOTcl Version $::xotcl::version >= 1.4"
  set ns_cache_version_old [catch {ns_cache names xowiki_cache xxx}]
  if {$ns_cache_version_old} {
    ? {set x old} new "upgrade ns_cache: cvs -z3 -d:pserver:anonymous@aolserver.cvs.sourceforge.net:/cvsroot/aolserver co nscache"
  } else {
    ? {set x new} new "ns_cache version seems up to date"
  }
  
}