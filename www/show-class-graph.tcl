ad_page_contract {
  Show an XOTcl class or object

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
set dot [::util::which dot]

if {$dot eq ""} {
  ns_return 404 plain/text "dot not found"
  ad_script_abort
}

set stem [ad_tmpnam]
set dotfile $stem.dot
set outfile $stem.$format

try {
  set f [open $dotfile w]; puts $f $dot_code; close $f
  exec $dot -T$format -o $outfile $dotfile
} on error {errorMsg} {
  ns_log warning "show-class-graph: dot returned $errorMsg"
  ad_return_error "dot error" $errorMsg
} on ok {result} {
  ns_returnfile 200 [ns_guesstype $outfile] $outfile
  file delete -- $outfile
} finally {
  file delete -- $dotfile
}

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
