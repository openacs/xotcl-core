::xo::library doc {

  Handling ordered Composites

  ::xo::OrderedComposite to create tree structures with aggregated
  objects. This is similar to object aggregations, but
  preserves the order. The OrderedComposite supports
  hierarchical sorting.

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @creation-date 2005-11-26
  @cvs-id $Id$
}

namespace eval ::xo {
  Class create OrderedComposite

  OrderedComposite instproc show {} {
    next
    foreach child [:children] {
      $child show
    }
  }

  OrderedComposite instproc orderby {{-order "increasing"} {-type dictionary} attribute} {
    #
    # Specify the sorting properties order in OrderedComposites. The
    # sorting is defined via sorting attribute, sorting order and the
    # sorting type (defining the comparison operators).
    #
    # @param order one of "increasing" or "decreasing"
    # @param type one of "integer", "real", "index" or "dictionary" (default "dictionary")
    #
    #ns_log notice "OrderedComposite called with order '$order' type '$type' attribute '$attribute'"
    set :__order $order
    set :__orderby $attribute
    set :__ordercompare [ad_decode $type real __compare_tcl integer __compare_tcl __compare]
    if {$type eq "index"} {
      :mixin add ::xo::OrderedComposite::IndexCompare
    }
  }

  OrderedComposite instproc __compare_tcl {a b} {
    #
    # Comparison based on plain Tcl compare. This behaves reasonable
    # on numbers (integer or real) and in mixed cases of numbers and
    # strings.
    #
    set by ${:__orderby}
    set x [$a set $by]
    set y [$b set $by]
    if {$x < $y} {
      return -1
    } elseif {$x > $y} {
      return 1
    } else {
      return 0
    }
  }

  if {[::acs::icanuse "ns_strcoll"]} {
    OrderedComposite instproc __compare {a b} {
      set by ${:__orderby}
      set x [$a set $by]
      set y [$b set $by]
      return [ns_strcoll $x $y]
    }
  } else {
    OrderedComposite instproc __compare {a b} {
      set by ${:__orderby}
      set x [$a set $by]
      set y [$b set $by]
      if {$x < $y} {
        return -1
      } elseif {$x > $y} {
        return 1
      } else {
        return 0
      }
    }
  }

  OrderedComposite instproc children {} {
    if {![info exists :__children]} {
      return ""
    }

    if {[info exists :__orderby] && [llength ${:__children}] > 0} {
      set firstChild [lindex ${:__children} 0]
      if {[$firstChild exists ${:__orderby}]} {
        set order [expr {[info exists :__order] ? ${:__order} : "increasing"}]
        set compare [expr {[info exists :__ordercompare] ? ${:__ordercompare} : "__compare"}]
        #ns_log notice SORT=[list lsort -command :$compare -$order ${:__children}]
        return [lsort -command :$compare -$order ${:__children}]
      } else {
        ad_log warning "ignore invalid sorting criterion '${:__orderby}'"
      }
    }
    return ${:__children}
  }

  OrderedComposite instproc add obj {
    lappend :__children $obj
    $obj set __parent [self]
    #:log "-- adding __parent [self] to $obj -- calling after_insert"
    #$obj __after_insert
  }
  OrderedComposite instproc delete obj {
    set p [lsearch -exact ${:__children} $obj]
    if {$p == -1} {error "can't delete '$obj' from ${:__children}"}
    set :__children [lreplace ${:__children} $p $p]
    $obj destroy
  }

  OrderedComposite instproc last_child {} {
    lindex ${:__children} end
  }

  OrderedComposite instproc deep_copy {} {
    set copy [:copy [::xotcl::Object new]]
    if {[info exists :__children]} {
      $copy set __children {}
      foreach c ${:__children} {
        $copy add [$c copy [::xotcl::Object new]]
      }
    }
    return $copy
  }

  OrderedComposite instproc destroy {} {
    # destroy all children of the ordered composite
    if {[info exists :__children]} {
      #:log "--W destroying children ${:__children}"
      foreach c ${:__children} {
        if {[nsf::is object $c]} {$c destroy}
      }
    }
    #show_stack;:log "--W children murdered, now next, chlds=[:info children]"
    #namespace eval [self] {namespace forget *}  ;# for pre 1.4.0 versions
    next
  }

  OrderedComposite instproc contains cmds {
    :requireNamespace ;# legacy for older XOTcl versions
    set m [Object info instmixin]
    if {"[self class]::ChildManager" ni $m} {
      set insert 1
      Object instmixin add [self class]::ChildManager
    } else {
      set insert 0
    }
    #
    [self class]::ChildManager instvar composite
    # push the active composite
    lappend composite [self]
    set errorOccurred 0
    # check, if we have Tcl's apply available
    if {[info procs ::apply] eq ""} {
      set applyCmd [list ::apply [list {} $cmds [self]]]
    } else {
      set applyCmd [list namespace eval [self] $cmds]
    }
    try {
      {*}$applyCmd
    } on error {errorMsg} {
      set errorOccurred 1
    } finally {
      # pop the last active composite
      set composite [lrange $composite 0 end-1]

      if {$insert} {
        Object instmixin delete [self class]::ChildManager
      }
    }
    if {$errorOccurred} {error $errorMsg}
  }

  if {$::tcl_version < 8.6} {
    #
    # It seems that the scripted emulation of "try" in Tcl 8.5 is not
    # fully compatible with 8.6, so we fall back to the prior
    # implementation of the method contains, that does NOT use try.
    #
    OrderedComposite instproc contains cmds {
      :requireNamespace ;# legacy for older XOTcl versions
      set m [Object info instmixin]
      if {"[self class]::ChildManager" ni $m} {
        set insert 1
        Object instmixin add [self class]::ChildManager
      } else {
        set insert 0
      }
      #
      [self class]::ChildManager instvar composite
      # push the active composite
      lappend composite [self]
      # Check, if we have Tcl's apply cmd available (not the old OpenACS apply proc)
      if {[info commands ::apply] ne "" && [info procs ::apply] eq ""} {
        set errorOccurred [catch {::apply [list {} $cmds [self]]} errorMsg]
      } else {
        set errorOccurred [catch {namespace eval [self] $cmds} errorMsg]
      }

      # pop the last active composite
      set composite [lrange $composite 0 end-1]

      if {$insert} {
        Object instmixin delete [self class]::ChildManager
      }
      if {$errorOccurred} {error $errorMsg}
    }
  }

  Class create OrderedComposite::ChildManager -instproc init args {
    set r [next]
    #set parent [self callingobject] ;# not a true calling object (ns-eval), but XOTcl 1 honors it
    #set parent [:info parent] ;# is ok in XOTcl 2, since the namespace is honored correctly
    #set parent [uplevel 2 self] ;# should work everywhere
    #puts stderr "-- CONTAINS p=$parent, co=[self callingobject] n=[uplevel 2 self]"
    #
    # get the top-most composite context as parent
    set parent [lindex [[self class] set composite] end]
    $parent lappend __children [self]
    set :__parent $parent
    #:__after_insert
    #:log "-- adding __parent  $parent to [self]"
    return $r
  }

  Class create OrderedComposite::Child -instproc __after_insert {} {;}

  Class create OrderedComposite::IndexCompare
  OrderedComposite::IndexCompare instproc __compare {a b} {
    set by ${:__orderby}
    set x [$a set $by]
    set y [$b set $by]
    #:log "--value compare  $x $y] => [:__value_compare $x $y 0]"
    return [:__value_compare $x $y 0]
  }
  OrderedComposite::IndexCompare instproc __value_compare {x y def} {
    set xp [string first . $x]
    set yp [string first . $y]
    if {$xp == -1 && $yp == -1} {
      if {$x < $y} {
        return -1
      } elseif {$x > $y} {
        return 1
      } else {
        return $def
      }
    } elseif {$xp == -1} {
      set yh [string range $y 0 $yp-1]
      return [:__value_compare $x $yh -1]
    } elseif {$yp == -1} {
      set xh [string range $x 0 $xp-1]
      return [:__value_compare $xh $y 1]
    } else {
      set xh [string range $x 0 $xp]
      set yh [string range $y 0 $yp]
      #:log "xh=$xh yh=$yh"
      if {$xh < $yh} {
        return -1
      } elseif {$xh > $yh} {
        return 1
      } else {
        incr xp
        incr yp
        #:log "rest [string range $x $xp end] [string range $y $yp end]"
        return [:__value_compare [string range $x $xp end] [string range $y $yp end] $def]
      }
    }
  }

  Class create OrderedComposite::MethodCompare
  OrderedComposite::MethodCompare instproc __compare {a b} {
    set by ${:__orderby}
    set x [$a $by]
    set y [$b $by]
    if {$x < $y} {
      return -1
    } elseif {$x > $y} {
      return 1
    } else {
      return 0
    }
  }
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
