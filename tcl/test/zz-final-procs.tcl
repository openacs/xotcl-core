ad_library {
    Test xotcl-core features
}


aa_register_case -cats {
    api smoke
} -procs {
    "::xo::dc foreach"
    "::xo::dc multirow"
} zz_final_leftover {

    Makes sure hat not tmp objects survived

} {
    ::xo::aa_check_leftovers
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
