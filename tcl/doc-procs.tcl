ad_library {
    generic doc procs

    @creation-date 2015-04-30
    @author Gustaf Neumann
    @cvs-id $Id$  
}

namespace eval ::xo {

    proc dotquote {e} {
	return \"$e\" 
    }

    proc dotquotel {l} {
	set result [list]
	foreach e $l { lappend result \"$e\" }
	return $result
    }
    
    ad_proc dot_append_method {{-documented_methods 1} e methods_ref kind} {
    } {
	upvar $methods_ref methods
	set infokind $kind
	if {$kind eq "instproc"} {append infokind s}
	::xo::api scope_from_object_reference scope e
        if {$kind eq "proc"} {set prefix "&rarr; "} {set prefix ""}
	foreach methodName [xo::getObjectProperty $e $kind] {
            if {$documented_methods} {
                set proc_index [::xo::api proc_index $scope $e $kind $methodName]
                #my msg "check $methodName => [nsv_exists api_proc_doc $proc_index]"
                if {[nsv_exists api_proc_doc $proc_index]} {
                    lappend methods $prefix$methodName
                }
            } else {
                lappend methods $prefix$methodName
            }
        }
    }
    
    ad_proc dotclass {{-is_focus 0} {-documented_methods 1} e} {
    } {
	set definition ""
        if {$is_focus} {
	    set style "style=filled,penwidth=1.5,color=bisque4,fillcolor=beige,"
	} else {
	    set style ""
	}
	set url [export_vars -base show-object [list [list object $e]]]
	append definition "[dotquote $e] \[${style}URL=\"$url\",label=\"\{$e|"
	foreach slot [$e info slots] {
	    set name ""
	    catch {set name $slot name}
	    if {$name ne ""} {
		append definition "[$slot name]\\l"
	    }
	}
	append definition "|"
	::xo::api scope_from_object_reference scope e
	set methods [list]
        dot_append_method -documented_methods $documented_methods $e methods proc
        dot_append_method -documented_methods $documented_methods $e methods instproc
	dot_append_method -documented_methods $documented_methods $e methods instforward
	foreach method [lsort $methods] {append definition "$method\\l" }
	append definition "\}\"\];\n"
    }

    ad_proc dotobject {e} {
    } {
	set url [export_vars -base show-object [list [list object $e]]]
     	set definition "[dotquote $e] \[URL=\"$url\"\];\n";
    }

    ad_proc dotcode {
	{-with_children 0}
	{-with_instance_relations 0} 
	{-omit_base_classes 1} 
	{-documented_methods 1}
	{-current_object ""} 	
	{-dpi 96} 
	things
    } {
    } {
	set classes {}
	set objects {}
	set iclasses {}
	set mclasses {}
	
	foreach e $things {
            if {![::nsf::is object $e] || ($omit_base_classes && [::nsf::is baseclass $e])} continue
            lappend [expr {[::nsf::is class $e] ? "classes" : "objects"}] $e
	}
	set instances ""
	if {$with_instance_relations} {
            foreach e $things {
                if {![::nsf::is object $e] || ($omit_base_classes && [::nsf::is baseclass $e])} continue
                set c [$e info class]
                if {$omit_base_classes && [::nsf::is baseclass $c]} continue
                if {$c ni $things} {lappend iclasses $c}
                append instances "[dotquote $e]->[dotquote $c];\n"
            }
	}
	set superclasses ""
	foreach e $classes {
            if {![::nsf::is object $e]} continue
            set reduced_sc [list]
            foreach sc [::xo::getObjectProperty $e superclass] {
                if {$omit_base_classes && [::nsf::is baseclass $sc]} continue
                lappend reduced_sc $sc
            }
            if {$reduced_sc eq {}} continue
            foreach sc $reduced_sc {
                if {$sc in $things} {
                    append superclasses "[dotquote $e]->[dotquotel $sc];\n"
                }
            }
	}
        set children ""
        set mixins ""
        foreach e $things {
            if {![::nsf::is object $e] || ($omit_base_classes && [::nsf::is baseclass $e])} continue
            if {$with_children} {
                foreach c [$e info children] {
                    if {$c ni $things} continue
                    append children "[dotquote $c]->[dotquote $e];\n"
                }
            }
            set m [xo::getObjectProperty $e mixin]
            #puts "-- $e mixin $m"
            if {$m eq ""} continue
            foreach mixin $m {
                if {$mixin ni $things} {lappend mclasses $m}
                append mixins "[dotquote $e]->[dotquotel $mixin];\n"
            }
        }
        set tclasses ""
        set instmixins ""
        foreach e $classes {
            set m [xo::getObjectProperty $e instmixin]
            #puts "-- $e instmixin $m"
            if {$m eq ""} continue
            #foreach mixin $m {
            #  append tclasses [dotclass -documented_methods $documented_methods $mixin]
            #}

            foreach mixin $m {
                if {$mixin ni $things} {lappend mclasses $mixin}
                append instmixins "[dotquote $e]->[dotquotel $mixin];\n"
            }
        }

        foreach e $classes {
            append tclasses [dotclass -is_focus [expr {$e eq $current_object}] -documented_methods $documented_methods $e]
        }
        set tobjects {}
        foreach e $objects {
            append tobjects [dotobject $e]
        }
        set tmclasses {}
        foreach e $mclasses {
            append tmclasses [dotobject $e]
        }
        set ticlasses {}
        foreach e $iclasses {
            append ticlasses [dotobject $e]
        }
        
        #label = \".\\n.\\nObject relations of [self]\"
        #edge \[dir=back, constraint=0\] \"::Decorate_Action\" -> \"::Action\";
        set objects  [join [dotquotel $objects] {; }]
        #set classes  [join [dotquotel $classes] {; }]
        set imcolor hotpink4

        set font "fontname = \"Helvetica\",fontsize = 8,"
        #set font "fontname = \"Bitstream Vera Sans\",fontsize = 8,"
        # rankdir = BT; labeldistance = 20;
        return "digraph {
   dpi = $dpi;
   rankdir = BT;
   node \[$font shape=record\]; $tclasses
   edge \[arrowhead=empty\]; $superclasses
   node \[fontcolor=$imcolor, color=$imcolor, style=filled, fillcolor=bisque\]; $tmclasses
   node \[fontcolor=blue, color=blue, style=filled, fillcolor=darkslategray2\]; $ticlasses
   node \[color=Green,shape=ellipse,fontcolor=Blue, style=filled, fillcolor=darkseagreen1\]; $tobjects
   edge \[color=Blue,style=dotted,arrowhead=normal,label=\"instance of\",fontsize=10\]; $instances
   edge \[color=pink,arrowhead=diamond, style=dotted\]; $children
   edge \[label=instmixin,fontsize=10,color=$imcolor,fontcolor=$imcolor,arrowhead=none,arrowtail=vee,style=dashed,dir=back,constraint=0\]; $instmixins
   edge \[label=mixin,fontsize=10,color=$imcolor,fontcolor=$imcolor,arrowhead=none,arrowtail=vee,style=dashed,dir=back,constraint=0\]; $mixins

}"
    }

}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
