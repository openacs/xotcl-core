ad_library {
  Test of parts of the xotcl-core API
}


aa_register_case -cats {
  api smoke
} -procs {
    ::xo::update_query
    ::xo::update_query_variable
    "::xo::Context instproc process_query_parameter"
    "::xo::ConnectionContext instproc query_parameter"
} api__context {
  Test of handling query variables.
} {

    set cases {
        {a=1 a 2} "{a 2}"
        {a=1 b 2} "{b 2} {a 1}"
        {a=1&b=2&c=3 b 200} "{b 200} {a 1} {c 3}"
        {a=1&b=2&c=3 b ""} "{b {}} {a 1} {c 3}"
        {a=1 a "hello world"} "{a {hello world}}"
        {a=1 a "a&b"} "{a a&b}"
        {a=a%26b&b=2 a 100} "{a 100} {b 2}"
        {a=1&b=2&c=3 c wörld} "{c wörld} {a 1} {b 2}"
        {a=1&w%c3%b6rld=nice&c=1 wörld beautiful} "{wörld beautiful} {a 1} {c 1}"
        {a&b=1 a x} "{a x} {b 1}"
        {a&b=1 b ""} "{b {}} {a {}}"
    }

    foreach {input expected} $cases {
        lassign $input q k v
        aa_equals "update_query_variable $q $k $v" \
            [::xo::update_query_variable $q $k $v] \
            $expected
    }

    set cases {
        {a=1 a 2} "a=2"
        {a=1 b 2} "b=2&a=1"
        {a=1&b=2&c=3 b 200} "b=200&a=1&c=3"
        {a=1&b=2&c=3 b ""} "b=&a=1&c=3"
        {a=1 a "hello world"} "a=hello+world"
        {a=1&b=2 a "a&b"} "a=a%26b&b=2"
        {a=a%26b&b=2 a 100} "a=100&b=2"
        {a=1&b=2&c=3 c wörld} "c=w%c3%b6rld&a=1&b=2"
        {a=1&w%c3%b6rld=nice&c=1 wörld beautiful} "w%c3%b6rld=beautiful&a=1&c=1"
        {a&b=1 a x} "a=x&b=1"
        {a&b=1 b ""} "b=&a="
    }

    foreach {input expected} $cases {
        lassign $input q k v
        aa_equals "update_query $q $k $v" \
            [::xo::update_query $q $k $v] \
            $expected
    }

    xo::ConnectionContext create ::xo::tcc \
        -package_id [ad_conn package_id] \
        -parameter_declaration {{-m view} {-folder_id:integer 0}} \
        -user_id [ad_conn user_id] \
        -actual_query m=edit&a=1&b \
        -locale en_US \
        -url http://test.org
    ::xo::tcc destroy_on_cleanup

    ::xo::tcc process_query_parameter

    aa_log "connection context <pre>[ns_quotehtml [::xo::tcc serialize]]</pre>"
    set cases {
        m "edit"
        folder_id "0"
        a "1"
        b ""
    }
    foreach {p expected} $cases {
        aa_equals "get query variable $p from xo::tcc" [::xo::tcc query_parameter $p] $expected
    }
}
