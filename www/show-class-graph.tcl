ad_page_contract {
  Show an XOTcl class or object

  @author Gustaf Neumann
  @cvs-id $Id$
} -query {
  {classes:token,notnull}
  {documented_only:boolean,notnull 1}
  {with_children:boolean,notnull 0}
  {dpi:integer,notnull 96}
  {format:word,notnull png}
}


set dot_code [::xo::dotcode -dpi $dpi \
                  -with_children $with_children -documented_methods $documented_only \
                  $classes]
set dot [::util::which dot]

if {$dot eq ""} {
  ns_return 404 plain/text "dot not found"
  ad_script_abort
}


try {
  set F [ad_opentmpfile dotfile dot]
  puts $F $dot_code
  close $F

  exec $dot -T$format -O $dotfile
  set outfile $dotfile.$format
  
} on error {errorMsg} {
  catch {close $F}
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
