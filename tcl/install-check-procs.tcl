namespace eval ::xotcl-core {

  ad_proc -private ::xotcl-core::before-install {} {
  
    Callback for checking whether xotcl is installed for OpenACS
    
    @author Gustaf Neumann (neumann@wu-wien.ac.at)
  } {
    ns_log notice "-- before-install callback"
    if {[info commands ::xotcl::Class] eq ""} {
      error " XOTcl does not appear to be installed on your system!\n\
     Please follow the install instructions on http://www.openacs.org/xowiki/xotcl-core"
    } elseif {$::xotcl::version < 1.5} {
      error " XOTcl 1.5 or newer required. You are using $::xotcl::version$::xotcl::patchlevel.\n\
      Please install a new version of XOTcl (see http://www.openacs.org/xowiki/xotcl-core)"
    } else {
      ns_log notice "XOTcl $::xotcl::version$::xotcl::patchlevel is installed on your system."
    }
  }
  
  ad_proc -private ::xotcl-core::after-upgrade {
    {-from_version_name:required}
    {-to_version_name:required}
  } {
    
    Callback for upgrading
    
    @author Gustaf Neumann (neumann@wu-wien.ac.at)
  } {
    ns_log notice "-- UPGRADE $from_version_name -> $to_version_name"
    set v 0.88
    if {[apm_version_names_compare $from_version_name $v] == -1 &&
        [apm_version_names_compare $to_version_name $v] > -1} {
      ns_log notice "-- upgrading to $v"
      set dir [acs_package_root_dir xotcl-core]
      foreach file {
        tcl/05-doc-procs.tcl 
        tcl/10-recreation-procs.tcl-old
        tcl/thread_mod-procs.tcl 
      } {
        if {[file exists $dir/$file]} {
          ns_log notice "Deleting obsolete file $dir/$file"
          file delete -- $dir/$file
        }
      }
    }
  }

}


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
