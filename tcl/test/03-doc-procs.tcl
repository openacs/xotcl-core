ad_library {

    Test xotcl-core documentation generation.

}


aa_register_case -cats {
  api smoke
} -procs {
    "::xo::api update_object_doc"
    "::xo::api update_nx_docs"
} xo_update_object_doc {

    Test rebuilding object documentation in relevant corner cases.

} {
    namespace eval ::__test_xo {
        nx::Class create AClass {
            :public method "blueprint get info" {} {
                #
                # A method with a cornercase name meant to potentially
                # collide with other "special words" in nsf.
                #
            }
        }
    }

    try {
        ::xo::api update_nx_docs
    } on ok {d} {
        set failed_p false
    } on error {errmsg} {
        set failed_p true
        aa_log "Regenerating the docs reports: $errmsg"
    }

    aa_false "Updating the docs works" $failed_p
}
