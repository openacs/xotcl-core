ad_page_contract {
  Show an xotcl class or object
  
  @author Gustaf Neumann
  @cvs-id $id:$
} -query {
  {object:optional ::xotcl::Object}
  {show_methods:optional 1}
  {show_source:optional 0}
  {show_variables:optional 0}
} -properties {
  title:onevalue
  context:onevalue
  output:onevalue
}

set context [list "XOTcl"]
set output ""
::xotcl::api scope_from_object_reference scope object
set isobject [::xotcl::api isobject $scope $object]

if {!$isobject} {
  ad_return_complaint 1 "Unable to access object $object. 
	Might this be a temporary object?"
  ad_script_abort
}

interp alias {} DO {} ::xotcl::api inscope $scope 

set my_class [DO $object info class]
set title "[::xotcl::api object_link $scope $my_class] $object"
set isclass [::xotcl::api isclass $scope $object]

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
  if {$dosort} {
    set list [lsort [DO $object info $kind]]
  } else {
    set list [DO $object info $kind]
  }
  set refs [list]
  foreach e $list {
    if {[DO $object isclass $e]} {
      lappend refs [::xotcl::api object_link $scope $e]
    }
  }
  if {[llength $refs]>0 && $list ne ""} {
    append class_references "<li>$kind: [join $refs {, }]</li>\n"
  }
  if {[llength $list]>0 && $list ne ""} {
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
  set parameters [lsort [$c info parameter]]
  append result "<dt><em>Meta-class:</em></dt> <dd>[::xotcl::api object_link $scope [$c info class]]</dd>\n"
  if {$parameters ne ""} { 
    set pretty [list]
    foreach p $parameters {
      if {[llength $p]>1} {
        foreach {p default} $p break
        lappend pretty "$p (default <span style='color: green; font-style: italic'>\"$default\"</span>)"
      } else {
        lappend pretty "$p"
      }
      set param($p) 1
    }
    append result "<dt><em>Parameter for instances:</em></dt> <dd>[join $pretty {, }]</dd>\n" 
  }
  set methods [lsort [$c info instcommands]]
  set pretty [list]
  foreach m $methods {
    if {[info exists param($m)]} continue
    set entry [::xotcl::api method_link $c instproc $m]
    lappend pretty $entry
  }
  if {[llength $pretty]>0} {
    append result "<dt><em>Methods for instances:</em></dt> <dd>[join $pretty {, }]</dd>"
  }
  set methods [lsort [$c info commands]]
  set pretty [list]
  foreach m $methods {
    if {![::xotcl::Object isobject ${c}::$m]} {
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

proc reverse list {
  set result [list]
  for {set i [expr {[llength $list] - 1}]} {$i >= 0} {incr i -1}      {
    lappend result [lindex $list $i]
  }
  return $result
}
proc superclass_hierarchy {cl scope} {
  set l [list]
  foreach c [reverse [concat $cl [$cl info heritage]]] {
    lappend s [class_summary $c $scope]
  }
  return $s
}

#
# document the class or the object"
#
set index [::xotcl::api object_index $scope $object]
append output "<blockquote>\n"

if {$isclass} {
  append output "<h4>Class Hierarchy of $object</h4>"
  #append output [superclass_hierarchy $object]
  append output [draw_as_tree [superclass_hierarchy $object $scope]]
  #set class_hierarchy [ns_urlencode [concat $object [$object info heritage]]]
  #
  # compute list of classes with siblings
  set class_hierarchy [list]
  foreach c [$object info superclass] {
    if {$c eq "::xotcl::Object"} {continue}
    eval lappend class_hierarchy [$c info subclass]
  }
  if {[llength $class_hierarchy]>5} {set class_hierarchy {}}
  eval lappend class_hierarchy [$object info heritage]
  if {[lsearch -exact $class_hierarchy $object] == -1} {lappend class_hierarchy $object}
  #::xotcl::Object msg class_hierarchy=$class_hierarchy
  set class_hierarchy [ns_urlencode $class_hierarchy]
  set documented_only [expr {$show_methods < 2}]
  #set class_hierarchy [ns_urlencode [concat $object [$object info heritage]]]
}

if {[nsv_exists api_library_doc $index]} {
  array set doc_elements [nsv_get api_library_doc $index]
  append output [lindex $doc_elements(main) 0]
  append output "<dl>\n"
  if { [info exists doc_elements(creation-date)] } {
    append output "<dt><b>Created:</b>\n<dd>[lindex $doc_elements(creation-date) 0]\n"
  }
  if { [info exists doc_elements(author)] } {
    append output "<dt><b>Author[ad_decode [llength $doc_elements(author)] 1 "" "s"]:</b>\n"
    foreach author $doc_elements(author) {
      append output "<dd>[api_format_author $author]\n"
    }
  }
  if { [info exists doc_elements(cvs-id)] } {
    append output "<dt><b>CVS Identification:</b>\n<dd>\
	<code>[ns_quotehtml [lindex $doc_elements(cvs-id) 0]]</code>\n"
  }
  append output "</dl>\n"

  set url "/api-doc/procs-file-view?path=[ns_urlencode $doc_elements(script)]"
  append output "Defined in <a href='$url'>$doc_elements(script)</a><p>"

  array unset doc_elements
}

set obj_create_source "$my_class create $object"

set class_references ""

if {$isclass} {
  append obj_create_source \
      [info_option $scope $object superclass] \
      [info_option $scope $object parameter 1] \
      [info_option $scope $object instmixin] 
  info_option $scope $object subclass 1
}

append obj_create_source \
    [info_option $scope $object mixin]

if {$class_references ne ""} {
  append output "<h4>Class Relations</h4><ul>\n$class_references</ul>\n"
}
append output "</blockquote>\n"

if {$show_source} {
  append output [::xotcl::api source_to_html $obj_create_source] \n
}

proc api_src_doc {out show_source scope object proc m} {
  set output "<a name='$proc-$m'></a><li>$out"
  if { $show_source } { 
    append output \
	"<pre class='code'>" \
	[api_tcl_to_html [::xotcl::api proc_index $scope $object $proc $m]] \
	</pre>
  }
  return $output
}

if {$show_methods} {
  append output "<h3>Methods</h3>\n" <ul> \n
  foreach m [lsort [DO $object info procs]] {
    set out [api_documentation $scope $object proc $m]
    if {$out ne ""} {
      append output [api_src_doc $out $show_source $scope $object proc $m]
    }
  }
  foreach m [lsort [DO $object info forward]] {
    set out [api_documentation $scope $object forward $m]
    if {$out ne ""} {
      append output [api_src_doc $out $show_source $scope $object forward $m]
    }
  }

  if {$isclass} {
    set cls [lsort [DO $object info instprocs]]
    foreach m $cls {
      set out [api_documentation $scope $object instproc $m]
      if {$out ne ""} {
        append output "<a name='instproc-$m'></a><li>$out"
	if { $show_source } { 
	  append output \
	      "<pre class='code'>" \
	      [api_tcl_to_html [::xotcl::api proc_index $scope $object instproc $m]] \
	      </pre>
	}
      }
    }
  }
  append output </ul> \n
}

if {$show_variables} {
  set vars ""
  foreach v [lsort [DO $object info vars]] {
    if {[DO $object array exists $v]} {
      append vars "$object array set $v [list [DO $object array get $v]]\n"
    } else {
      append vars "$object set $v [list [DO $object set $v]]\n"
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


DO $s destroy
