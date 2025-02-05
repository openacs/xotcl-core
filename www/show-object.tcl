ad_page_contract {
  Show an XOTcl class or object

  @param as_img do NOT include svg content in the HTML rendering

  @author Gustaf Neumann

  @cvs-id $Id$
} -query {
  {object:nohtml,trim ::xotcl::Object}
  {show_methods:range(0|2),notnull 1}
  {show_source:range(0|1),notnull 0}
  {show_variables:range(0|1),notnull 0}
  {as_img:boolean,notnull 0}
  {with_children:boolean,notnull 0}
  {with_instances:boolean,notnull 0}
  {with_instance_relations:boolean,notnull 0}
  {above:naturalnum,notnull 1}
  {below:naturalnum,notnull 2}
} -properties {
  title:onevalue
  context:onevalue
  output:onevalue
}

#ns_log notice "SHOW OBJECT object=$object show_methods=$show_methods show_source=$show_source show_variables=$show_variables"
set keys [ns_set keys [ns_parsequery [ns_conn query]]]
#ns_log notice "... keys $keys"
if {[::util::suspicious_query_variable -proc xo::update_query $keys]} {
  ad_return_complaint 1 "invalid query parameters: $keys"
  ns_log notice "... aborting show-object due to suspicious query variables [list $keys]"
  ad_script_abort
}

set context [list "XOTcl Object"]
set output ""

::xo::api scope_from_object_reference scope object

if {$scope ne ""} {
  #
  # "scope" must be an object, otherwise something is wrong.
  #
  set isobject [expr {[::xo::api isobject "" $scope]
                      && [::xo::api isobject $scope $object]}]
} else {
  set isobject [::xo::api isobject "" $object]
}

if {!$isobject} {
  ad_return_complaint 1 "Unable to access object '$object'.
    Might this be a temporary object?"
  ad_script_abort
}

if {$scope ne ""} {
  auth::require_login
}

interp alias {} DO {} ::xo::api scope_eval $scope

# get object fully qualified
set object [DO namespace origin $object]

set my_class [DO apidoc::get_object_property $object class]
set title "$my_class $object"
set isclass [::xo::api isclass $scope $object]
set isnx [DO apidoc::get_object_property $object isnxobject]
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


nsf::proc local_api_documentation {{-proc_type scripted} show_methods scope object kind method} {
  set proc_index [::xo::api proc_index $scope $object $kind $method]
  set kind_label [::xo::api method_label -kind $proc_index]
  if {[nsv_exists api_proc_doc $proc_index]} {
    set documentation [api_proc_documentation \
                           -first_line_tag "<h4>" \
                           -proc_type $proc_type \
                           -label "<em>$method</em>" \
                           $proc_index]
    set result $documentation
  } else {
    #
    # We have no enty in api_proc_doc; provide minimal info
    #
    if {$show_methods > 1} {
      set result "<h4><em>$method</em> ($proc_type)</h4>\n"
      append result [::xo::api debug_widget [list {*}$scope $object $kind $method]]
    } else {
      set result ""
    }
  }
  return $result
}

proc class_relation {scope object kind {dosort 0}} {
  upvar class_references class_references

  set isnx [DO apidoc::get_object_property $object isnxobject]
  set list [DO apidoc::get_object_property $object $kind]

  if {$dosort} {set list [lsort $list]}

  set refs [list]
  foreach e $list {
    lappend refs [::xo::api object_link $scope $e]
  }
  if {[llength $refs] > 0 && $list ne ""} {
    append class_references "<li>$kind: [join $refs {, }]</li>\n"
  }
  if {[llength $list] > 0 && $list ne ""} {
    return " \\\n     -$kind [list $list]"
  }
  return ""
}


proc class_summary {c scope} {
  set result ""
  if {0} {
    set methods [lsort [DO apidoc::get_object_property $c instcommand]]
    set pretty [list]
    foreach m $methods {
      if {[info exists param($m)]} continue
      set entry [::xo::api method_link $c instproc $m]
      lappend pretty $entry
    }
    if {[llength $pretty]>0} {
      append result "<dt><em>Methods for instances:</em></dt> <dd>[join $pretty {, }]</dd>"
    }
    set methods [lsort [DO apidoc::get_object_property $c command -callprotection all]]
    set pretty [list]
    foreach m $methods {
      if {![DO apidoc::get_object_property ${c}::$m isobject]} {
        lappend pretty [::xo::api method_link $c proc $m]
      }
    }
    if {[llength $pretty]>0} {
      append result "<dt><em>Methods to be applied on the class object (in addition to the methods provided by the meta-class):</em></dt> <dd>[join $pretty {, }]</dd>"
    } else {
      #append result "<dt><em>Methods to be applied on the class:</em></dt><dd>Methods provided by the meta-class</dd>"
    }
  }
  if {$result ne ""} {
    set result <dl>$result</dl>
  }

  set pretty_parameter ""
  set line "[::xo::api object_link $scope $c] create ..."
  set parameters [lsort [DO apidoc::get_object_property $c parameter]]
  if {[llength $parameters] > 0} {
    #
    # Initial line length is length of class name + "create" + "..." +
    # white space
    #
    set llength [expr {8 + [string length $c]}]
    set pstart "&nbsp;\\<br>[string repeat {&nbsp;} 10]"

    foreach p $parameters {
      if {[llength $p]>1} {
        lassign $p p default
        append line $pstart " \[ -$p (default <span style='color: green; font-style: italic'>\"$default\"</span>) \]"
      } else {
        append line $pstart " \[ -$p <i>$p</i> \]"
      }
      #set param($p) 1
    }
  }
  append line "<p>\n"

  return "<pre>$line</pre>"
}

#
# document the class or the object"
#
set index [::xo::api object_index $scope $object]

set class_hierarchy [list]

if {$isclass} {

  append output "<h4>Class $object</h4>"
  append output "<blockquote>\n"
  append output [class_summary $object $scope]

  #
  # compute list of classes with siblings
  foreach c [DO apidoc::get_object_property $object superclass] {
    if {[DO apidoc::get_object_property $object isbaseclass]} continue
    lappend class_hierarchy {*}[DO apidoc::get_object_property $c subclass]
  }
  if {[llength $class_hierarchy]>5} {
    set class_hierarchy {}
  }

  # Display just up to two extra two levels of heritage to keep the
  # class in question in focus.
  set heritage [DO apidoc::get_object_property $object heritage]
  set subclasses [DO apidoc::get_object_property $object subclass]

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

  if {$object ni $class_hierarchy} {
    lappend class_hierarchy $object
  }

  if {$below > 0} {
    for {set level 1} {$level < $below} {incr level} {
      foreach sc $subclasses {
        foreach c [DO apidoc::get_object_property $sc subclass] {
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
set hide_methods [expr {$show_methods == 0}]

if {[nsv_exists api_library_doc $index]} {
  array set doc_elements [nsv_get api_library_doc $index]
  append output [lindex $doc_elements(main) 0]
  append output "<dl>\n"
  if { [info exists doc_elements(param)] && [llength $doc_elements(param)] > 0} {
    append output "<dt><b>Documented Parameters:</b></dt><dd><dl>\n"
    foreach par $doc_elements(param) {
      if {[regexp {^\s*(\S+)\s*(.*)$} $par . param desc]} {
        append output "<dt><em>$param</em></dt><dd>$desc</dd>\n"
      } else {
        ad_log warning "show_object: ignoring invalid parameter description <$par>"
      }
    }
    append output "</dl></dd>"
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
    append output "<dt><b>Author[expr {[llength $doc_elements(author)] > 1 ? "s" : ""}]:</b>\n"
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

  unset -nocomplain doc_elements
}

set obj_create_source "$my_class create $object"

set class_references ""
class_relation $scope $object class

if {$isclass} {
  append obj_create_source \
      [class_relation $scope $object superclass] \
      [class_relation $scope $object instmixin]

  class_relation $scope $object subclass
  class_relation $scope $object instmixinof
  class_relation $scope $object mixinof
}

append obj_create_source \
    [class_relation $scope $object mixin]

if {$class_references ne ""} {
  append output "<h4>Class Relations</h4><ul>\n$class_references</ul>\n"
}

if {$show_source} {
  append output [::xo::api source_to_html $obj_create_source] \n
}

proc api_src_doc {out show_source scope object proc m} {
  set output "<a name='$proc-$m'></a><li>$out"
  if { $show_source } {
    append output \
        "<pre class='code'>" \
        [::apidoc::tcl_to_html [::xo::api proc_index $scope $object $proc $m]] \
        </pre>
  }
  return $output
}

if {$show_methods} {
  #
  # per-object methods
  #
  set methods [lsort [DO ::apidoc::get_object_property $object command]]
  if {[llength $methods] > 0} {
    set method_output ""
    foreach m $methods {
      set type [DO ::apidoc::get_object_property $object methodtype $m]
      if {$type eq "object"} {
        #
        # filter (sub)objects, which are callable via the method interface
        #
        continue
      }
      set out [local_api_documentation -proc_type $type $show_methods $scope $object proc $m]
      if {$out ne ""} {
        #ns_log notice "CALL [list api_src_doc $out $show_source $scope $object proc $m]"
        append method_output [api_src_doc $out $show_source $scope $object proc $m]
        #ns_log notice "CALL [list api_src_doc $out $show_source $scope $object proc $m] DONE"
      }
    }
    if {$method_output ne ""} {
      append output \
          "<h3>Methods (to be applied on the object)</h3>\n" \
          <ul> \n $method_output </ul> \n
    }
  }

  if {$isclass} {
    #
    # instance methods
    #
    set methods [lsort [DO ::apidoc::get_object_property $object instcommand]]
    if {[llength $methods] > 0} {
      set method_output ""
      foreach m $methods {
        set type [DO ::apidoc::get_object_property $object instmethodtype $m]
        set out [local_api_documentation -proc_type $type $show_methods $scope $object instproc $m]
        if {$out ne ""} {
          append method_output "<a name='instproc-$m'></a><li>$out"
          if { $show_source } {
            append method_output \
                "<pre class='code'>" \
                [::apidoc::tcl_to_html [::xo::api proc_index $scope $object instproc $m]] \
                </pre>
          }
        }
      }
      if {$method_output ne ""} {
        append output \
            "<h3>Methods (to be applied on instances)</h3>\n" \
            <ul> \n $method_output </ul> \n
      }
    }
  }
}

if {$show_variables && !$isnx} {
  set vars ""
  foreach v [lsort [DO ::apidoc::get_object_property $object vars]] {
    if {[DO ::apidoc::get_object_property $object array-exists $v]} {
      append vars "$object array set $v [list [DO ::apidoc::get_object_property $object array-get $v]]\n"
    } else {
      append vars "$object set $v [list [DO ::apidoc::get_object_property $object set $v]]\n"
    }
  }
  if {$vars ne ""} {
    append output "<h3>Variables</h3>\n" \
        [::xo::api source_to_html $vars] \n
  }
}

if {$isclass && $with_instances} {
  set instances ""
  foreach o [lsort [DO $object info instances]] {
    append instances [::xo::api object_link $scope $o] ", "
  }
  set instances [string trimright $instances ", "]
  if {$instances ne ""} {
    append output "<h3>Instances</h3>\n" \
        <blockquote>\n \
        $instances \
        </blockquote>\n
  }
}

#
# "as_img" true means: do not include SVG in the code.
#
if {!$as_img} {
  #
  # Construct the dot code from the provided classes as embedded svg
  # code.
  #
  set dot_code [::xo::dotcode -dpi 72 \
                    -with_children $with_children \
                    -with_instance_relations $with_instance_relations \
                    -omit_base_classes 0 \
                    -current_object $object \
                    -documented_methods $documented_only \
                    -hide_methods $hide_methods \
                    $class_hierarchy]

  set svg [util::inline_svg_from_dot -css {
    svg g a:link {text-decoration: none;}
    div.inner svg {width: 100%; margin: 0 auto;}
  } $dot_code]
}

if {$isclass} {
  append output "</blockquote>\n"
}


DO $s destroy

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
