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

  ? {expr {$::xotcl::version < 1.5}} 0 "XOTcl Version $::xotcl::version >= 1.5"

  ? {expr {$::tcl_version < 8.5}} 0 "Tcl Version $::tcl_version >= 8.5.4"
  if {[regexp {8[.]5[.]([0-9]+)$} $::tcl_patchLevel _ minor]} {
    ? {expr {$minor < 4}} 0 "Tcl Version $::tcl_patchLevel >= 8.5.4"
  }

  set ns_cache_version_old [catch {ns_cache names util_memoize xxx}]
  if {$ns_cache_version_old} {
    ? {set x old} new "upgrade ns_cache: cvs -z3 -d:pserver:anonymous@aolserver.cvs.sourceforge.net:/cvsroot/aolserver co nscache"
  } else {
    ? {set x new} new "ns_cache version seems sufficiently up to date"
  }
  
  ? {expr {[::xotcl::Object info methods serialize] ne ""}} 1 "Serialize method available"

  set errorMsg ""
  if {[catch {Serializer all} errorMsg]} {
    aa_true "Serializer not avalilable $errorMsg" 0
  } else {
    aa_true "Serializer avalilable" 1
  }
  
}
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
