ad_library {
  XOTcl API for library file management (handling file-level dependencies)

  @author Gustaf Neumann
  @creation-date 2007-10-11
  @cvs-id $Id$
}

#
# Support for loading files of a package in a non-alphabetical order
#
# Usage:
# Top of file:
#
#     ::xo::library doc {
#         .....your comment goes here ....
#     }
#
# Load a required file:
#
#   Source a file, which is required by the current file
#   Filename is without path and .tcl
#
#     ::xo::library require filename
#
#   The library to be loaded must be defined with a
#   ::xo::library doc {...}
#
# Source files extending classes of the current file.
#
#    When classes are defined in the current file and (some) of their methods
#    are defined in other files, one has to load the methods of the
#    other files after the classes are recreated in the current file
#    (recreation of classes deletes the old methods).
#
#     ::xo::library source_dependent
#

namespace eval ::xo {
  Object library

  library proc doc {comment} {
    ad_library $comment
    nsv_set [self]-loaded [info script] 1
    #:log "--loaded nsv_set [self]-loaded [info script] 1"
  }

  library ad_proc require {{-package ""} filename} {

    Use this method to indicate when some other files (from the same
    package) are needed to be sourced before the current file.  This
    method is useful in cases where the alphabetical loading order is
    a problem.

    A typical use-case is a file defining a subclass of another
    class. In this case, the file defining the subclass will require
    the definition of the base class.

    @param filename filename without path and .tcl suffix
  } {
    #:log "--loaded nsv_set [self]-loaded [info script] 1"
    nsv_set [self]-loaded [info script] 1
    set myfile [file tail [info script]]
    set dirname [file dirname [info script]]
    if {$package eq ""} {
      set otherfile $dirname/$filename.tcl
    } else {
      set otherfile $::acs::rootdir/packages/$package/tcl/$filename.tcl
    }
    set vn [self]
    #:log "--exists otherfile $otherfile => [nsv_exists $vn $otherfile]"
    if {[nsv_exists $vn $otherfile]} {
      nsv_set $vn $otherfile [lsort -unique [concat [nsv_get $vn $otherfile] [info script]]]
      #:log "--setting nsv_set $vn $otherfile [lsort -unique [concat [nsv_get $vn $otherfile] $myfile]]"
    } else {
      nsv_set $vn $otherfile [info script]
      #:log "--setting nsv_set $vn $otherfile $myfile"
    }
    #:log "--source when not loaded [self]-loaded $otherfile: [nsv_exists [self]-loaded $otherfile]"
    #:log "--loaded = [lsort [nsv_array names [self]-loaded]]"

    if {![nsv_exists [self]-loaded $otherfile]} {
      :log "--sourcing first $otherfile"
      apm_source $otherfile
      nsv_set [self]-loaded $otherfile 1
    }
  }

  library ad_proc source_dependent {} {
    Source files extending classes of the current file.

    When classes are defined in this file and (some) of their methods
    are defined in other files, we have to load the methods of the
    other files after the classes are recreated in this file
    (recreation of classes deletes the old methods).

    Use "::xo::library source_dependent" at the end of a file
    when the classes are defined.

  } {
    set myfile [file tail [info script]]
    set dirname [file dirname [info script]]
    set vn [self]
    #:log "--check nsv_exists $vn $dirname/$myfile [nsv_exists $vn $dirname/$myfile]"
    if {[nsv_exists $vn $dirname/$myfile]} {
      foreach file [nsv_get $vn $dirname/$myfile] {
        incr ::__source_dependent_level
        ns_log notice "--sourcing dependent ($::__source_dependent_level) $myfile -> $file"
        apm_source $file
        incr ::__source_dependent_level -1
      }
    }
  }
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
