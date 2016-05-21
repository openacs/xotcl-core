ad_page_contract {
    ... purpose ...
    @author ...
    @creation-date ...
} {
    {proc_spec ""}
    {return_url:localurl "."}
}

if {![acs_user::site_wide_admin_p]} {
    ad_complain [_ acs-admin.lt_You_dont_have_permiss]
}

switch [llength $proc_spec] {
    1 {}
    3 {lassign $proc_spec obj methodType method; set scope ""}
    4 {lassign $proc_spec scope obj methodType method}
    default {
        ad_log warning "toggle_debug: Unexpected format <$proc_spec> consists of [llength $proc_spec] parts"
    }
}
if {[info exists method]} {
    if {$methodType eq "proc"} {
        set modifier "-per-object"
    } elseif {$methodType eq "instproc"} {
        set modifier ""
    } else {
        ns_log warning "unexpected method type <$methodType>"
        set modifier ""
    }
    set debug_p [{*}$scope ::nsf::method::property $obj {*}$modifier $method debug]
    set cmd ""
} else {
    #
    # In case the proc_spec is not fully qualified, prepend namespace qualifier
    #
    if {![string match ::* $proc_spec]} {set proc_spec ::$proc_spec}
    
    set definition [nsf::cmd::info definition $proc_spec]
    set method $proc_spec
    set modifier ""
    set scope ""
    set obj ::nx::Object
    set cmd ""

    #
    # If the proc is not a nsf::proc, build a nsf::proc on the fly
    #
    if {[lindex $definition 0] eq "::proc"} {
	set cmd [list ::nsf::proc -debug {*}[lrange $definition 1 end]]
	ns_log notice $cmd
	ns_eval [list ::nsf::proc -debug {*}[lrange $definition 1 end]]
	set debug_p 0
    } else {
	set debug_p [{*}$scope ::nsf::method::property $obj {*}$modifier $method debug]
    }
}
set cmd [list {*}$scope ::nsf::method::property $obj {*}$modifier $method debug [expr {!$debug_p}]]
ns_log notice "setting debug flag with cmd\n$cmd"
if {[catch {ns_eval {*}$cmd} errorMsg] } {
    ns_log notice "toggle-debug raised error: $errorMsg"
}
ns_log notice "calling return redirect to $return_url"

ad_returnredirect $return_url
