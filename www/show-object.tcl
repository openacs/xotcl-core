ad_page_contract {
  Show an xotcl class or object
  
  @author Gustaf Neumann
  @cvs-id $Id$
} -query {
  {object:nohtml,trim ::xotcl::Object}
  {show_methods:naturalnum 1}
  {show_source:naturalnum 0}
  {show_variables:naturalnum 0}
  {as_img:boolean 0}
  {with_children:boolean 0}
  {with_instance_relations:boolean 0}
  {above:naturalnum 1}
  {below:naturalnum 2}
} -properties {
  title:onevalue
  context:onevalue
  output:onevalue
}

set context [list "XOTcl Object"]
set output ""

::xotcl::api scope_from_object_reference scope object
#
# scope must be an object, otherwise something is wrong.
#
if {$scope ne "" && ![xo::getObjectProperty $scope isobject]} {
  set isobject 0
} else {
  set isobject [::xotcl::api isobject $scope $object]
}

if {$scope ne ""} {
  auth::require_login
}

if {!$isobject} {
  ad_return_complaint 1 "Unable to access object $object. 
    Might this be a temporary object?"
  ad_script_abort
}

interp alias {} DO {} ::xotcl::api inscope $scope 

set my_class [DO $object info class]
set title "[::xotcl::api object_link $scope $my_class] $object"
set isclass [::xotcl::api isclass $scope $object]
set isnx [xo::getObjectProperty $object isnxobject]

set s [DO Serializer new]

set dimensional_slider [ad_dimensional {
  {
    show_methods "Methods:" 1 {
      { 2 "All Methods" }
      { 1 "Documented Methods" }
      { 0 "Hide Methods" }
    }
  }
  {
    show_source "Source:" 0 {
      { 1 "Display Source" }
      { 0 "Hide Source" }
    }
  }
  {
    show_variables "Variables:" 0 {
      { 1 "Show Variables" }
      { 0 "Hide Variables" }
    }
  }
}]


proc api_documentation {scope object kind method} {
  upvar show_methods show_methods 
  set proc_index [::xotcl::api proc_index $scope $object $kind $method]
  if {[nsv_exists api_proc_doc $proc_index]} {
    set documentation [api_proc_documentation \
                           -first_line_tag "<h4>" \
                           -label "$kind <em>$method</em>" \
                           $proc_index]
    set result $documentation
  } else {
    if {$show_methods == 2} {
      set result "<h4>$kind <em>$method</em></h4>"
    } else {
      set result ""
    }
  }
  return $result
}

proc info_option {scope object kind {dosort 0}} {
  upvar class_references class_references

  set isnx [DO xo::getObjectProperty $object isnxobject]
  set list [DO xo::getObjectProperty $object $kind]

  if {$dosort} {set list [lsort $list]}

  set refs [list]
  foreach e $list {
    lappend refs [::xotcl::api object_link $scope $e]
  }
  if {[llength $refs] > 0 && $list ne ""} {
    append class_references "<li>$kind: [join $refs {, }]</li>\n"
  }
  if {[llength $list] > 0 && $list ne ""} {
    return " \\\n     -$kind [list $list]"
  }
  return ""
}

proc draw_as_tree {nodes} {
  if {$nodes eq ""} return ""
  set tail [draw_as_tree [lrange $nodes 1 end]]
  if {$tail eq ""} {
    set style "style = 'border: 1px solid; padding: 5px; background-color: #fbfbfb;'"
  } else {
    set style "style = 'border: 1px solid; margin: 3px; padding: 5px; background-color: #fefefe; color: #555555;'"
  }
  append output <ul> "<li $style>" [lindex $nodes 0]</li> $tail </ul>
}

proc class_summary {c scope} {
  set result ""
  set parameters [lsort [DO xo::getObjectProperty $c parameter]]
  append result "<dt><em>Meta-class:</em></dt> <dd>[::xotcl::api object_link $scope [DO xo::getObjectProperty $c class]]</dd>\n"
  if {$parameters ne ""} { 
    set pretty [list]
    foreach p $parameters {
      if {[llength $p]>1} {
        lassign $p p default
        lappend pretty "$p (default <span style='color: green; font-style: italic'>\"$default\"</span>)"
      } else {
        lappend pretty "$p"
      }
      set param($p) 1
    }
    append result "<dt><em>Parameter for instances:</em></dt> <dd>[join $pretty {, }]</dd>\n" 
  }
  set methods [lsort [DO xo::getObjectProperty $c instcommand]]
  set pretty [list]
  foreach m $methods {
    if {[info exists param($m)]} continue
    set entry [::xotcl::api method_link $c instproc $m]
    lappend pretty $entry
  }
  if {[llength $pretty]>0} {
    append result "<dt><em>Methods for instances:</em></dt> <dd>[join $pretty {, }]</dd>"
  }
  set methods [lsort [DO xo::getObjectProperty $c command]]
  set pretty [list]
  foreach m $methods {
    if {![DO ::xotcl::Object isobject ${c}::$m]} {
      lappend pretty [::xotcl::api method_link $c proc $m]
    }
  }
  if {[llength $pretty]>0} {
    append result "<dt><em>Methods to be applied on the class (in addition to the methods provided by the meta-class):</em></dt> <dd>[join $pretty {, }]</dd>"
  } else {
    append result "<dt><em>Methods to be applied on the class:</em></dt><dd>Methods provided by the meta-class</dd>"
  }

  if {$result ne ""} {
    set result <dl>$result</dl>
  }
  return "<strong> [::xotcl::api object_link $scope $c] </strong> $result"
}

proc superclass_hierarchy {cl scope} {
  set l [list]
  foreach c [lreverse [concat $cl [DO $cl info heritage]]] {
    lappend s [class_summary $c $scope]
  }
  return $s
}

#
# document the class or the object"
#
set index [::xotcl::api object_index $scope $object]
append output "<blockquote>\n"

set class_hierarchy [list]

if {$isclass} {
  set hierarchy 0
  if {$hierarchy} {
    append output "<h4>Class Hierarchy of $object</h4>"
    append output [draw_as_tree [superclass_hierarchy $object $scope]]
  } else {
    append output "</blockquote>\n"
    append output "<h4>Class $object</h4>"
    append output "<blockquote>\n"
    append output [class_summary $object $scope]
  }

  #
  # compute list of classes with siblings
  foreach c [DO xo::getObjectProperty $object superclass] {
    if {$c eq "::xotcl::Object"} {continue}
    lappend class_hierarchy {*}[DO xo::getObjectProperty $c subclass]
  }
  if {[llength $class_hierarchy]>5} {set class_hierarchy {}}

  # Display just up to two extra two levels of heritage to keep the
  # class in quesiton in focus.
  set heritage [DO xo::getObjectProperty $object heritage]
  set subclasses [DO xo::getObjectProperty $object subclass]
  
  if {[llength $heritage] > $above} {
    # In case we have nothing to show from the subclasses,
    # show one more superclass to provide a better overview.
    if {$below > 0 && [llength $subclasses] == 0} {
      incr above
    }
    if {[llength $heritage] > $above} {
      set heritage [lrange $heritage 0 $above-1]
    }
  } 
  lappend class_hierarchy {*}$heritage
  
  if {$object ni $class_hierarchy} {lappend class_hierarchy $object}

  if {$below > 0} {

    for {set level 1} {$level < $below} {incr level} {
      foreach sc $subclasses {
        foreach c [DO xo::getObjectProperty $sc subclass] {
          if {$c ni $subclasses} {
            lappend subclasses $c
          }
        }
      }
    }
    lappend class_hierarchy {*}$subclasses
  }
}
set documented_only [expr {$show_methods < 2}]

if {[nsv_exists api_library_doc $index]} {
  array set doc_elements [nsv_get api_library_doc $index]
  append output [lindex $doc_elements(main) 0]
  append output "<dl>\n"
  if { [info exists doc_elements(param)] } {
    append output "<dt><b>Documented Parameters:</b>\n"
    foreach par $doc_elements(param) {
      append output "<dd><em>-[lindex $par 0]</em> [lrange $par 1 end]\n"
    }
  }
  if { [info exists doc_elements(see)] } {
    append output "<dt><b>See Also:</b>\n"
    foreach seeref $doc_elements(see) {
      append output "<dd>[::apidoc::format_see $seeref]\n"
    }
  }
  if { [info exists doc_elements(creation-date)] } {
    append output "<dt><b>Created:</b>\n<dd>[lindex $doc_elements(creation-date) 0]\n"
  }
  if { [info exists doc_elements(author)] } {
    append output "<dt><b>Author[ad_decode [llength $doc_elements(author)] 1 "" "s"]:</b>\n"
    foreach author $doc_elements(author) {
      append output "<dd>[::apidoc::format_author $author]\n"
    }
  }
  if { [info exists doc_elements(cvs-id)] } {
    append output "<dt><b>CVS Identification:</b>\n<dd>\
    <code>[ns_quotehtml [lindex $doc_elements(cvs-id) 0]]</code>\n"
  }
  append output "</dl>\n"

  set url "/api-doc/procs-file-view?path=[ns_urlencode $doc_elements(script)]"
  append output "Defined in <a href='[ns_quotehtml $url]'>$doc_elements(script)</a><p>"

  array unset doc_elements
}

set obj_create_source "$my_class create $object"

set class_references ""

if {$isclass} {
  append obj_create_source \
      [info_option $scope $object superclass] \
      [info_option $scope $object instmixin] \
      [info_option $scope $object subclass 1]
}

append obj_create_source \
    [info_option $scope $object mixin]

if {$class_references ne ""} {
  append output "<h4>Class Relations</h4><ul>\n$class_references</ul>\n"
}

if {$show_source} {
  append output [::xotcl::api source_to_html $obj_create_source] \n
}

proc api_src_doc {out show_source scope object proc m} {
  set output "<a name='$proc-$m'></a><li>$out"
  if { $show_source } { 
    append output \
        "<pre class='code'>" \
        [::apidoc::tcl_to_html [::xotcl::api proc_index $scope $object $proc $m]] \
        </pre>
  }
  return $output
}

if {$show_methods} {
  append output "<h3>Methods</h3>\n" <ul> \n
  foreach m [lsort [DO ::xo::getObjectProperty $object proc]] {
    set out [api_documentation $scope $object proc $m]
    if {$out ne ""} {
      append output [api_src_doc $out $show_source $scope $object proc $m]
    }
  }
  foreach m [lsort [DO ::xo::getObjectProperty $object forward]] {
    set out [api_documentation $scope $object forward $m]
    if {$out ne ""} {
      append output [api_src_doc $out $show_source $scope $object forward $m]
    }
  }

  if {$isclass} {
    set cls [lsort [DO ::xo::getObjectProperty $object instproc]]
    foreach m $cls {
      set out [api_documentation $scope $object instproc $m]
      if {$out ne ""} {
        append output "<a name='instproc-$m'></a><li>$out"
        if { $show_source } { 
          append output \
              "<pre class='code'>" \
              [::apidoc::tcl_to_html [::xotcl::api proc_index $scope $object instproc $m]] \
              </pre>
        }
      }
    }
  }
  append output </ul> \n
}

if {$show_variables && !$isnx} {
  set vars ""
  foreach v [lsort [DO $object info vars]] {
    if {[DO ::xo::getObjectProperty $object array-exists $v]} {
      append vars "$object array set $v [list [DO ::xo::getObjectProperty $object array-get $v]]\n"
    } else {
      append vars "$object set $v [list [DO ::xo::getObjectProperty $object set $v]]\n"
    }
  }
  if {$vars ne ""} {
    append output "<h3>Variables</h3>\n" \
        [::xotcl::api source_to_html $vars] \n
  }
}

if {$isclass} {
  set instances ""
  foreach o [lsort [DO $object info instances]] {
    append instances [::xotcl::api object_link $scope $o] ", "
  }
  set instances [string trimright $instances ", "]
  if {$instances ne ""} {
    append output "<h3>Instances</h3>\n" \
        <blockquote>\n \
        $instances \
        </blockquote>\n
  }
}

if {!$as_img} {
  #
  # Construct the dot code from the provided classes.
  #
  # TODO: it would be nice to pass the selected options from the
  # dimensional slide to dotcode, since with svg, the dot code
  # constructs URLS for navigation in the class tree.
  #
  set dot_code [::xo::dotcode -dpi 72 \
                    -with_children $with_children \
                    -with_instance_relations $with_instance_relations \
                    -omit_base_classes 0 \
                    -current_object $object \
                    -documented_methods $documented_only \
                    $class_hierarchy]

  set dot ""
  catch {set dot [::util::which dot]}
  # final ressort for cases, where ::util::which is not available
  if {$dot eq "" && [file executable /usr/bin/dot]} {set dot /usr/bin/dot}
  if {$dot eq ""} {ns_return 404 plain/text "dot not found"; ad_script_abort}
 
  set tmpnam [ad_tmpnam]
  set tmpfile $tmpnam.svg
  set f [open $tmpnam.dot w]; puts $f $dot_code; close $f

  #ns_log notice "svg $tmpnam dot $tmpnam.dot"
  set f [open "|$dot  -Tsvg -o $tmpfile" w]; puts $f $dot_code; close $f
  set f [open  $tmpfile]; set svg [read $f]; close $f

  # delete the first three lines generated from dot
  regsub {^[^\n]+\n[^\n]+\n[^\n]+\n} $svg "" svg
  set css {
    svg g a:link {text-decoration: none;}
    div.inner {width: 100%; margin: 0 auto;}
  }
  set svg "<style>$css</style><div><div class='inner'>$svg</div></div>"

  file delete $tmpfile
  file delete $tmpnam.dot
}

append output "</blockquote>\n"


DO $s destroy

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
