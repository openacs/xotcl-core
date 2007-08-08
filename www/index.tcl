ad_page_contract {
  Show classes defined in the connection threads

  @author Gustaf Neumann
  @cvs-id $Id$
} -query {
  {all_classes:optional 0}
} -properties {
  title:onevalue
  context:onevalue
  output:onevalue
}

set title "XOTcl Classes Defined in Connection Threads"
set context [list "XOTcl"]

set dimensional_slider [ad_dimensional {
  {
    all_classes "Show:" 0 {
      { 1 "All Classes" }
      { 0 "Application Classes only" }
    }
  }
}]


proc local_link cl {
  upvar all_classes all_classes
  if {$all_classes || ![string match "::xotcl::*" $cl]} {
    return "<a href='#$cl'>$cl</a>"
  } else {
    return $cl
  }
}

proc info_classes {cl key {dosort 0}} {
  upvar all_classes all_classes
  set infos ""
  set classes [$cl info $key]
  if {$dosort} {
    set classes [lsort $classes]
  }
  foreach s $classes {
    append infos [local_link $s] ", "
  }
  set infos [string trimright $infos ", "]
  if {$infos ne ""} {
    return "<li><em>$key</em> $infos</li>\n"
  } else {
    return ""
  }
}

set output "<ul>"
foreach cl [lsort [::xotcl::Class allinstances]] {
  if {!$all_classes && [string match "::xotcl::*" $cl]} \
      continue
  
  append output "<li><b><a name='$cl'>[::xotcl::api object_link {} $cl]</b> <ul>"

  append output [info_classes $cl superclass]
  append output [info_classes $cl subclass 1]
  append output [info_classes $cl mixin]
  append output [info_classes $cl instmixin]

  foreach key {procs instprocs} {
    set infos ""
    foreach i [lsort [$cl info $key]] {append infos [::xotcl::api method_link $cl $key $i] ", "}
    set infos [string trimright $infos ", "]
    if {$infos ne ""} {
      append output "<li><em>$key:</em> $infos</li>\n"
    }
    
  }

  set infos ""
  foreach o [lsort [$cl info instances]] {append infos [::xotcl::api object_link {} $o] ", "}
  set infos [string trimright $infos ", "]
  if {$infos ne ""} {
    append output "<li><em>instances:</em> $infos</li>\n"
  }


  append output </ul>
}
append output </ul>

