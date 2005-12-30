ad_library {
  XOTcl HTML Widget Classes based on tdom

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @author Neophytos Demetriou (k2pts@phigita.net)
  @creation-date 2005-11-26
  @cvs-id $Id$
}

::Serializer exportMethods {
  ::xotcl::Object instproc asHTML
}

Object instproc asHTML {{-master defaultMaster} -page:switch} {
  require_html_procs
  dom createDocument html doc
  set root [$doc documentElement]
  if {!$page} {
    $root appendFromScript {my render}
    return [[$root childNode] asHTML]
  } else {
    set slave [$master decorate $root]
    $slave appendFromScript {my render}
    ns_return 200 text/html [$root asHTML]
  }
}

#
# Define Widget classes
#
# ::xo::Table, somewhat similar to the classical multirow 

namespace eval ::xo {
  Class Table -superclass OrderedComposite \
      -parameter {{no_data  "No Data"} {renderer TABLE2}}

  Table instproc destroy {} {
    #my log "-- "
    foreach c {__actions __columns} {
      namespace eval [self]::$c {namespace forget [self class]::*}
    }
    next
  }
  Table instproc actions {cmd} {
    set M [OrderedComposite create [self]::__actions]
    namespace eval $M {namespace import -force [self class]::*}
    $M contains $cmd
  }
  Table instproc columns {cmd} {
    set M [OrderedComposite create [self]::__columns]
    namespace eval $M {namespace import -force [self class]::*}
    $M contains $cmd
    set slots [list]
    foreach c [$M children] {
      eval lappend slots [$c get-slots]
    }
    my proc add $slots {
      set __self [::xo::Table::Line new]
      foreach __v [info vars] {$__self set $__v [set $__v]}
      next $__self
    }
  }

  Table instproc render_with {renderer} {
    my log "--"
    set cl [self class]
    [self] mixin ${cl}::$renderer 
    foreach child [$cl info classchildren] {
      #my log "-- $child heritage [$child info heritage]"
      if {[$child info heritage ::xo::OrderedComposite::Child] eq ""} continue
      set mixinname ${cl}::${renderer}::[namespace tail $child]
      if {[::xotcl::Object isclass $mixinname]} {
	$child instmixin $mixinname
	#my log "-- using mixin $mixinname"
      } else {
	#my log "-- no mixin $mixinname"
      }
    }
    my init_renderer
  }

  Table instproc write_csv {} {
    set output ""
    set line [list]
    foreach column [[self]::__columns children] {
      set value [string map {\" \\\"} [$column name]]
      lappend line \"$value\"
    }
    append output [join $line ,] \n
    foreach row [my children] {
      set line [list]
      foreach column [[self]::__columns children] {
	set value [string map {\" \\\"} [$row set [$column name]]]
	lappend line \"$value\"
      }
      append output [join $line ,] \n
    }
    ns_return 200 text/csv $output
  }

  Class create Table::Line \
      -instproc attlist {name atts {extra ""}} {
	set result [list] 
	foreach att $atts {
	  set varname $name.$att
	  if {[my exists $varname]} {lappend result $att [my set $varname]}
	}
	foreach {att val} $extra {lappend result $att $val}
	return $result
      }
  

  #
  # Define elements of a Table
  #
  namespace eval ::xo::Table {
    Class Action \
	-superclass ::xo::OrderedComposite::Child \
	-parameter {label url {tooltip {}}}

    Class Field \
	-superclass ::xo::OrderedComposite::Child \
	-parameter {label {html {}} {orderby ""} name} \
	-instproc init {} {
	  my set name [namespace tail [self]]
	} \
	-instproc get-slots {} {
	  return -[my name]
	}

    Class AnchorField \
	-superclass ::xo::Table::Field \
	-instproc get-slots {} {
	  set slots [list -[my name]]
	  foreach subfield {href text} {
	    lappend slots [list -[my name].$subfield ""]
	  }
	  return $slots
	}

    Class ImageField \
	-parameter {src width height border title alt} \
	-superclass ::xo::Table::Field \
	-instproc get-slots {} {
	  set slots [list -[my name]]
	  lappend slots [list -[my name].src [my src]]
	  lappend slots [list -[my name].href ""]
	  foreach att {width height border title alt} {
	    if {[my exists $att]} {
	      lappend slots [list -[my name].$att [my $att]]
	    } else {
	      lappend slots [list -[my name].$att]
	    }
	  }
	  return $slots
	}

    Class ImageField_EditIcon \
	-superclass ImageField -parameter {
	  {src /resources/acs-subsite/Edit16.gif} {width 16} {height 16} {border 0} 
	  {title "Edit Item"} {alt "edit"}
	}
    Class ImageField_ViewIcon \
	-superclass ImageField -parameter {
	  {src /resources/acs-subsite/Zoom16.gif} {width 16} {height 16} {border 0} 
	  {title "View Item"} {alt "view"}
	}
    Class ImageField_DeleteIcon \
	-superclass ImageField -parameter {
	  {src /resources/acs-subsite/Delete16.gif} {width 16} {height 16} {border 0} 
	  {title "Delete Item"} {alt "delete"}
	}
    
    # export table elements
    namespace export Field AnchorField  Action ImageField \
	ImageField_EditIcon ImageField_ViewIcon ImageField_DeleteIcon
  }
}


namespace eval ::xo::Table {
  #
  # Class for rendering ::xo::Table as the html TABLE
  #
  Class TABLE \
      -instproc init_renderer {} {
	#my log "--"
	my set __rowcount 0
      }

  TABLE instproc render-actions {} {
    html::tr -class list-button-bar  {
      set cols [llength [[self]::__columns children]]
      html::td -colspan $cols -class list-button-bar {
	set children [[self]::__actions children]
	set last [lindex $children end]
	foreach o $children {
	  $o render
	  if {$o ne $last} {
	    html::t -disableOutputEscaping "&middot;"
	  }
	}
      } 
    }
  }
  
  TABLE instproc render-body {} {
    html::tr -class list-header {
      foreach o [[self]::__columns children] {
	$o render
      }
    }
    set children [my children]
    if {[llength $children] == 0} {
      html::tr {html::td { html::t [my set no_data]}}
    } else {
      foreach line [my children] {
	html::tr -class [expr {[my incr __rowcount]%2 ? "list-odd" : "list-even" }] {
	  foreach field [[self]::__columns children] {
	    html::td  [concat [list class list] [$field html]] { 
	      $field render-data $line
	    }
	  }
	}
      }
    }
  }
  
  TABLE instproc render {} {
    if {![my isobject [self]::__actions]} {my actions {}}
    html::table -class list {
      my render-actions
      my render-body
    }
  }

  #
  # Define renderer for elements of a Table
  # 
  # ::xo:Table requires the elements to have the methods render and render-data 
  #

  Class create TABLE::Action -instproc render {} {
    html::a -class button -title [my tooltip] -href [my url] { html::t [my label] } 
  }

  Class create TABLE::Field
  TABLE::Field instproc render-data {line} {
    html::t [$line set [my name]] 
  }

  TABLE::Field instproc render {} {
    html::th [concat [list class list] [my html]] { 
      if {[my set orderby] eq ""} {
	html::t [my set label] 
      } else {
	my renderSortLabels
      }
    }
  }

  TABLE::Field instproc renderSortLabels {} {
    set field [my set orderby]
    upvar #[template::adp_level] orderby orderby
    if {![info exists orderby]} {set orderby ""}
    set new_orderby $orderby
    if {$orderby eq "$field,desc"} {
      set new_orderby $field,asc
      set title "Sort by this column ascending"
      set img /resources/acs-templating/sort-ascending.png
    } elseif {$orderby eq "$field,asc"} {
      set new_orderby $field,desc
      set title "Sort by this column descending"
      set img /resources/acs-templating/sort-descending.png
    } else {
      set new_orderby $field,asc
      set title "Sort by this column"
      set img /resources/acs-templating/sort-neither.png
    }
    set query [list [list orderby $new_orderby]]
    foreach pair [split [ns_conn query] &] {
      foreach {key value} [split $pair =] break
      if {$key eq "orderby"} continue
      lappend query [list [ns_urldecode $key] [ns_urldecode $value]]
    }
    set href [export_vars -base [ad_conn url] $query]
    html::a -href $href -title $title {
      html::t [my set label]
      html::img -src $img -alt ""
    }
  }

  Class create TABLE::AnchorField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
	if {[$line exists [my name].href] && 
	    [set href [$line set [my name].href]] ne ""} {
	  html::a -href $href { 
	    return [next]
	  }
	}
	next
      }

 
  Class create TABLE::ImageField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
	html::a -href [$line set [my name].href] -style "border-bottom: none;" {
	  html::img [$line attlist [my name] {src width height border title alt}] {}
	}
      }

  Class TABLE2 \
      -superclass TABLE \
      -instproc render-actions {} {
	html::div -id "actions" -style "float: left" {
	  html::ul -style "list-style:none; padding: 10px;" {
	    foreach o [[self]::__actions children] {
	      html::li -class "button" {$o render}
	    }
	  }
	}
      } \
      -instproc render {} {
	if {![my isobject [self]::__actions]} {my actions {}}
	html::div  {
	  my render-actions
	  html::div -class table {
	    html::table -class list {my render-body}
	  }
	}
      }

  Class create TABLE2::Action -superclass TABLE::Action
  Class create TABLE2::Field -superclass TABLE::Field
  Class create TABLE2::AnchorField -superclass TABLE::AnchorField
  Class create TABLE2::ImageField -superclass TABLE::ImageField
  
}

Class TableWidget \
    -superclass ::xo::Table \
    -instproc init {} {
      my render_with [my renderer]
      next
    }



#
# Pure List widget
#

Class ListWidget -superclass ::xo::OrderedComposite -instproc render {} {
  html::ul {
    foreach o [my children] {
      html::li {
        $o render
      }
    }
  }
}


#
# Define two Master templates, an empty one and one page master
#

Object defaultMaster -proc decorate {node} {
   $node appendFromScript {
     set slave [tmpl::div]
   }
  return $slave
}

Object pageMaster -proc decorate {node} {
  $node appendFromScript {
    html::div -class defaultMasterClass {
      html::t "hello header"
      set slave [tmpl::body]
      html::t "hello footer"
    }
  }
  return $slave
}
