ad_library {
    Test xotcl-core features
}

aa_register_case -cats {
    api smoke
} -procs {
    "::xo::ConnectionContext instproc query_parameter"

    "::xo::ConnectionContext proc require"
    "::xo::Context instproc exists_query_parameter"

} test_xo_cc {
   Test basic ::xo::cc features
} {
    aa_run_with_teardown -test_code {
        ::xo::ConnectionContext require \
            -url /foo \
            -package_id 123 \
            -parameter "" \
            -user_id -1 \
            -actual_query "a=1&b=2&name=dagobert&__name=duck" \
           
        aa_true "exists xo::cc " {[info commands ::xo::cc] ne ""}
        ::xo::cc destroy_on_cleanup
        aa_log "xo::cc <pre>[::xo::cc serialize]</pre>"
        foreach {param expected} {a 1 b 1 foo 0 name 1 __name 1} {
            aa_equals "exists_query_parameter $param" [::xo::cc exists_query_parameter $param] $expected
        }
        foreach {pair expected} {
            {a ""} 1
            {b ""} 2
            {foo ""} ""
            {foo 123} 123
            {name "123"} "dagobert"
            {__name "123"} "duck"
        } {
            aa_equals "query_parameter $pair" [::xo::cc query_parameter {*}$pair] $expected
        }
        foreach {pair expected} {
            {a:integer ""} 1
            {foo:wordchar "abc"} "abc"
            {name:alpha "123"} "dagobert"
        } {
            aa_equals "query_parameter $pair" [::xo::cc query_parameter {*}$pair] $expected
        }
        if {[::acs::icanuse "nsf::parseargs -asdict"]} {
            #
            # The implementation based on "nsf::parseargs" without
            # "-asdict" clobbers local variables and might overwrite
            # "__name".
            #
            foreach {pair expected} {
                {__name:alpha "123"} "duck"
            } {
                aa_equals "query_parameter $pair" [::xo::cc query_parameter {*}$pair] $expected
            }
        }
        #
        # Avoid script_abort when value constraint fails
        #
        set ::aa_test_noabort 1
        
        foreach {pair expected} {
            {a:alpha ""} {expected alpha but got "1" for parameter "a"}
        } {
            set failed [catch {[::xo::cc query_parameter {*}$pair]} errorMsg]
            aa_equals "query_parameter $pair" $errorMsg $expected
        }
    }
}
