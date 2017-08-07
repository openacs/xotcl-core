ad_page_contract {
        Cache Viewer
} {    
  {cache:optional 0}
  {item:optional 0}
  {flush:optional 0}
  {filter ""}
  {flushall:optional 0}
} -properties {
    title:onevalue
    context:onevalue
}

set admin_p [acs_user::site_wide_admin_p]
if {!$admin_p} {
  ad_return_warning "Insufficient Permissions" \
      "Only side wide admins are allowed to view this page!"
  ad_script_abort
}

# Expires: now
ns_set update [ns_conn outputheaders] "Expires" "now"

if { $flush ne "0" } {
  ns_cache flush $cache $flush
  ad_returnredirect [export_vars -base [ns_conn url] {cache}]
  ad_script_abort
} 

if {$flushall == 1} {
  foreach i [ns_cache names $cache] {
    ns_cache flush $cache $i
  }
  ad_returnredirect [export_vars -base [ns_conn url] {cache}]
  ad_script_abort
}

set title "Show Caches"
set context [list [list "./cache" cache]]

set output ""


if { $cache == 0 } {
  set context ""
  
  TableWidget create t1 \
      -actions [subst {
        Action new -label Refresh -url [ad_conn url] -tooltip "Reload this page"
      }] \
      -columns {
        AnchorField name    -label "Name"
        Field       stats   -label "Stats"
        Field       size    -label "Size" -html { align right }
      } \
      -no_data "Currently no data available"

  foreach item [lsort [ns_cache_names]] {
    t1 add -name $item \
        -name.href "?cache=[ns_quotehtml $item]" \
        -stats [ns_cache_stats $item] \
        -size [lindex [ns_cache_size $item] 1]

  }
  set t1 [t1 asHTML]

} elseif { $item != 0 } {
  append output "<h3>Data for cache_item $item of cache $cache</h3>"
  append output "<p><xmp>[ns_cache get $cache $item]</xmp></p>"
} else {
  set item_list [ns_cache names $cache]
  set item_count [llength $item_list]
  set href [export_vars -base [ns_conn url] {cache {flushall 1}}]
  
  append output "<h3>Items in cache $cache ($item_count) with size [ns_cache_size $cache]</h3>\n"
  append output "<form>
    <input type='hidden' name='cache' value='$cache'>
    <a href='[ns_quotehtml $href]' class='button'>flush all</a>
    Filter: <input name='filter' value='$filter'> 
    </form>
  "


  #append output "<a href='[ns_quotehtml $href]'>flush all</a> items of [ns_quotehtml $cache]"

  set entries "<ul>"
  set count 0
  foreach name [lsort -dictionary $item_list] {
    if {[catch {set entry [ns_cache get $cache $name]}]} continue
    if {$filter ne ""} {if {![regexp $filter $entry]} continue}
    incr count
    set n ""
    regexp -- {-set name ([^\\]+)\\} $entry _ n
    set show_url  [export_vars -base [ns_conn url] [list cache [list item $name]]]
    set flush_url [export_vars -base [ns_conn url] [list cache [list flush $name]]]
    append entries "<li><a href=\"[ns_quotehtml $show_url]\">$name</a> $n ([string length $entry] bytes, " \
        "<a href=\"[ns_quotehtml $flush_url]\">flush</a>)</li>"
  }
  append entries "</ul>"
  if {$filter ne ""} {
    append output "$count matching entries:\n"
  }
  append output $entries
  append output "<a href='?'>All Caches</a>"
}

lappend context "Cache Statistics"


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
