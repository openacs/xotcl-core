ad_page_contract {
  Show an xotcl class or object
  
  @author Gustaf Neumann
  @cvs-id $Id$
} -query {
  {classes}
  {documented_only:boolean 1}
  {with_children:boolean 0}
  {dpi:integer 96}
  {format:word png}
}


set dot_code [::xo::dotcode -dpi $dpi \
                  -with_children $with_children -documented_methods $documented_only \
                  $classes]
set dot ""
catch {set dot [::util::which dot]}
# final ressort for cases, where ::util::which is not available
if {$dot eq "" && [file executable /usr/bin/dot]} {set dot /usr/bin/dot}
if {$dot eq ""} {ns_return 404 plain/text "dot not found"; ad_script_abort}
 
set tmpnam [ad_tmpnam]
set tmpfile $tmpnam.$format
set f [open $tmpnam.$format w]; puts $f $dot_code; close $f

#ns_log notice "png $tmpnam dot $tmpnam.dot"
set f [open "|$dot  -T$format -o $tmpfile" w]; puts $f $dot_code; close $f
ns_returnfile 200 [ns_guesstype $tmpfile] $tmpfile
file delete $tmpfile

#set f [open $tmpnam.dot w]; puts $f $dot_code; close $f
#file delete $tmpnam.dot


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
