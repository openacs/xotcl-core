ad_page_contract {
  Show an xotcl class or object
  
  @author Gustaf Neumann
  @cvs-id $Id$
} -query {
  {classes}
  {documented_only 1}
}

::xotcl::Object instproc dotquote {e} {
  return \"$e\" 
}
::xotcl::Object instproc dotquotel {l} {
  set result [list]
  foreach e $l { lappend result \"$e\" }
  return $result
}
::xotcl::Object instproc dot_append_method {{-documented_methods 1} e methods_ref kind} {
  my upvar $methods_ref methods
  set infokind $kind
  if {$kind eq "instproc"} {append infokind s}
  ::xotcl::api scope_from_object_reference scope e
  foreach method [$e info $infokind] {
    if {$documented_methods} {
      set proc_index [::xotcl::api proc_index $scope $e $kind $method]
      #my msg "check $method => [nsv_exists api_proc_doc $proc_index]"
      if {[nsv_exists api_proc_doc $proc_index]} {
        lappend methods $method
      }
    } else {
      lappend methods $method
    }
  }
}
::xotcl::Object instproc dotcode {{-omit_base_classes 1} {-documented_methods 1} things} {
  set classes [list]
  set objects [list]

  foreach e $things {
    if {![my isobject $e]} continue
    if {$omit_base_classes && $e eq "::xotcl::Object" || $e eq "::xotcl::Class"} continue
    lappend [expr {[my isclass $e] ? "classes" : "objects"}] $e
  }
  set instances ""
  foreach e $things {
    if {![my isobject $e]} continue
    if {$omit_base_classes && $e eq "::xotcl::Object" || $e eq "::xotcl::Class"} continue
    set c [$e info class]
    if {$omit_base_classes && $c eq "::xotcl::Object" || $c eq "::xotcl::Class"} continue
    append instances "[my dotquote $e]->[my dotquote $c];\n"
  }
  set superclasses ""
  foreach e $classes {
    if {![my isobject $e]} continue
    if {$e eq "::xotcl::Object"} continue
    set reduced_sc [list]
    foreach sc [$e info superclass] {
      if {$omit_base_classes && $sc eq "::xotcl::Object"
	  || $sc eq "::xotcl::Class"} continue
      lappend reduced_sc $sc
    }
    if {$reduced_sc eq {}} continue
    append superclasses "[my dotquote $e]->[my dotquotel $reduced_sc];\n"
  }
  set children ""
  set mixins ""
  foreach e $things {
    if {![my isobject $e]} continue
    if {$omit_base_classes && $e eq "::xotcl::Object" || $e eq "::xotcl::Class"} continue
    foreach c [$e info children] {
      if {[lsearch $things $c] == -1} continue
      append children "[my dotquote $c]->[my dotquote $e];\n"
    }
    set m [$e info mixin]
    #puts "-- $e mixin $m"
    if {$m eq ""} continue
    append mixins "[my dotquote $e]->[my dotquotel $m];\n"
  }
  set instmixins ""
  foreach e $classes {
    set m [$e info instmixin]
    #puts "-- $e instmixin $m"
    if {$m eq ""} continue
    append instmixins "[my dotquote $e]->[my dotquotel $m];\n"
  }
  set tclasses ""
  foreach e $classes {
    append tclasses "[my dotquote $e] \[label=\"\{$e|"
    foreach slot [$e info slots] {
      append tclasses "[$slot name]\\l"
    }
    append tclasses "|"
    ::xotcl::api scope_from_object_reference scope e
    set methods [list]
    my dot_append_method -documented_methods $documented_methods $e methods instproc
    my dot_append_method -documented_methods $documented_methods $e methods instforward
    foreach method [lsort $methods] {
      append tclasses "$method\\l"
    }
    append tclasses "\}\"\];"
  }
  #label = \".\\n.\\nObject relations of [self]\"
  #edge \[dir=back, constraint=0\] \"::Decorate_Action\" -> \"::Action\";
  set objects  [join [my dotquotel $objects] {; }]
  set classes  [join [my dotquotel $classes] {; }]
  set imcolor hotpink4

  set font "fontname = \"Helvetica\",fontsize = 8,"
  #set font "fontname = \"Bitstream Vera Sans\",fontsize = 8,"
# rankdir = BT; labeldistance = 20;
  return "digraph {
   rankdir = BT;
   node \[$font shape=record\]; $tclasses
   edge \[arrawohead=empty\]; $superclasses
   node \[color=Green,shape=ellipse,fontcolor=Blue, style=filled, fillcolor=darkseagreen1\]; $objects
   edge \[color=Blue,style=dotted\]; $instances
   edge \[color=pink,arrowhead=diamond, style=dotted\]; $children
   edge \[label=instmixin,fontsize=10,color=$imcolor,fontcolor=$imcolor,arrowhead=none,arrowtail=vee, style=dashed,dir=back, constraint=0\]; $instmixins
   edge \[label=mixin,fontsize=10,color=$imcolor,fontcolor=$imcolor,arrowhead=none,arrowtail=vee, style=dashed,dir=back, constraint=0\]; $mixins

}"
}

set dot_code [::xotcl::Object dotcode -documented_methods $documented_only $classes]
set dot ""
catch {set dot [::util::which dot]}
# final ressort for cases, where ::util::which is not available
if {$dot eq "" && [file executable /usr/bin/dot]} {set dot /usr/bin/dot}
if {$dot eq ""} {ns_return 404 plain/text "do dot found"}

set tmpfile [ns_tmpnam].png
set f [open "|$dot  -Tpng -o $tmpfile" w]
puts $f $dot_code
close $f
ns_returnfile 200 [ns_guesstype $tmpfile] $tmpfile
file delete $tmpfile