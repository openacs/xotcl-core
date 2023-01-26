ad_library {
    Test xotcl-core features
}


aa_register_case -cats {
    api smoke
} -procs {
    "::xo::db::Class proc exists_in_db"
    "::xo::db::Class proc get_instance_from_db"
    "::xo::db::DB-postgresql instproc get_value"

    "::xo::db::select_driver"
    "::xo::db::Object instproc save"
    "::xo::db::Object instproc save_new"
    "::xo::db::Object instproc delete"
    "::xo::db::Class proc object_type_to_class"
    "::xo::db::DB-postgresql instproc 0or1row"
    "::xo::db::DB-postgresql instproc 1row"
    "::xo::db::DB instproc transaction"
    "::xo::db::Object instproc update"
    "::xo::db::CrClass proc get_object_type"
    "::throttle proc check"

} test_xo_db_object {
   Test basic ::xo::db::Object ORM features
} {
    aa_run_with_teardown -test_code {

        aa_section "Quick trivial CRUD of an object"
        aa_log "Create object"
        set orm_object [::xo::db::Object new]
        aa_log "Save new"
        set object_id [$orm_object save_new]
        aa_log "Fetch"
        set orm_object [::xo::db::Class get_instance_from_db -id $object_id]
        aa_log "Save"
        $orm_object save
        aa_log "Delete"
        $orm_object delete

        aa_section "Object creation"
        aa_log "Create object"
        set orm_object [::xo::db::Object new]
        $orm_object set title "test_title"
        set object_id [$orm_object save_new]

        aa_log "Destroy object"
        $orm_object destroy

        set orm_exists_p [::xo::db::Class exists_in_db -id $object_id]
        set db_exists_p [::xo::dc 0or1row lookup_object {
            select 1 from acs_objects where object_id = :object_id
        }]
        aa_true "Object was created" {$orm_exists_p && $db_exists_p}


        aa_section "Object fetching"
        aa_log "Fetching object from ORM"
        set orm_object [::xo::db::Class get_instance_from_db -id $object_id]
        aa_log "Fetching object from DB"
        ::xo::dc 1row get_object_from_db {
            select title as object_title,
                   creation_date,
                   creation_user,
                   creation_ip,
                   package_id,
                   context_id,
                   modifying_user,
                   modifying_ip,
                   last_modified
            from acs_objects
            where object_id = :object_id
        }
        set attributes {
            object_title
            creation_date
            creation_user
            creation_ip
            package_id
            context_id
            modifying_user
            modifying_ip
            last_modified
        }
        foreach att $attributes {
            aa_equals "Same $att" [set $att] [$orm_object set $att]
        }


        aa_section "Object manipulation"
        aa_log "Setting a different title"
        set new_title "a different title"
        $orm_object set object_title $new_title

        set old_context_id [$orm_object set context_id]
        # obtain a random different context_id
        set new_context_id [::xo::dc get_value get_context_id {
            select min(object_id) from acs_objects
            where object_id <> :object_id
              and (:old_context_id is null or object_id <> :old_context_id)
        }]
        aa_log "Setting a different context_id: $new_context_id"
        $orm_object set context_id $new_context_id

        aa_log "Saving the object"
        $orm_object save


        aa_log "Fetching object attributes from DB"
        ::xo::dc 1row get_object_from_db {
            select title as object_title,
                   creation_date,
                   creation_user,
                   creation_ip,
                   package_id,
                   context_id,
                   modifying_user,
                   modifying_ip,
                   last_modified
            from acs_objects
            where object_id = :object_id
        }


        aa_section "Check modifications BEFORE refetching"
        aa_equals "title was updated"      [$orm_object set object_title] $new_title
        aa_equals "context_id was updated" [$orm_object set context_id]   $new_context_id
        foreach att $attributes {
            if {![aa_equals "Attribute $att in the object matches database value" [set $att] [$orm_object set $att]]} {
                aa_log "DB: [set $att]| ORM: [$orm_object set $att]"
            }
        }


        aa_section "Check modifications AFTER refetching"
        aa_log "Fetching object again from ORM"
        set orm_object [::xo::db::Class get_instance_from_db -id $object_id]
        aa_equals "title was updated"      [$orm_object set object_title] $new_title
        aa_equals "context_id was updated" [$orm_object set context_id]   $new_context_id
        foreach att $attributes {
            if {![aa_equals "Attribute $att in the object matches database value" [set $att] [$orm_object set $att]]} {
                aa_log "DB: [set $att]| ORM: [$orm_object set $att]"
            }
        }


        aa_section "Object deletion"
        $orm_object delete
        set orm_exists_p [::xo::db::Class exists_in_db -id $object_id]
        set db_exists_p [::xo::dc 0or1row lookup_object {
            select 1 from acs_objects where object_id = :object_id
        }]
        aa_true "Object is not there anymore" {!$orm_exists_p && !$db_exists_p}
    }
}

aa_register_case -cats {
    api smoke
} -procs {
    "::xo::db::Class proc exists_in_db"
    "::xo::db::CrClass instproc get_instance_from_db"
    "::xo::db::CrClass proc get_instance_from_db"
    "::xo::db::DB-postgresql instproc get_value"

    "::cr_check_mime_type"
    "::cr_create_content_file"
    "::xo::db::Class proc object_type_to_class"
    "::xo::db::CrItem instproc delete"
    "::xo::db::CrItem instproc save"
    "::xo::db::CrItem instproc save_new"
    "::xo::db::DB instproc transaction"
    "::xo::db::DB-postgresql instproc 0or1row"
    "::xo::db::DB-postgresql instproc 1row"
    "::xo::db::DB-postgresql instproc dml"
    "::xo::db::DB-postgresql instproc insert-view-operation"
    "::xo::db::DB-postgresql instproc row_lock"
    "::xo::db::postgresql instproc nextval"
    "::xo::db::CrClass proc get_object_type"
    "::throttle proc check"
} test_cr_items {
   Test basic ::xo::db::CrItem ORM features
} {
    aa_run_with_teardown -test_code {

        aa_section "Quick trivial CRUD of an object"
        aa_log "Create object"
        set orm_object [::xo::db::CrItem new]
        aa_log "Save new"
        set object_id [$orm_object save_new]
        aa_log "Fetch"
        set orm_object [::xo::db::CrClass get_instance_from_db -item_id $object_id]
        aa_log "Save"
        $orm_object save
        aa_log "Delete"
        $orm_object delete

        aa_section "Object creation"
        aa_log "Create object"
        set orm_object [::xo::db::CrItem new]
        $orm_object set title "test_title"
        set object_id [$orm_object save_new]
        set revision_id [$orm_object set revision_id]

        aa_log "Destroy object"
        $orm_object destroy

        set orm_exists_p [::xo::db::Class exists_in_db -id $object_id]
        set db_exists_p [::xo::dc 0or1row lookup_object {
            select 1 from cr_items where item_id = :object_id
        }]
        aa_true "Object was created" {$orm_exists_p && $db_exists_p}


        aa_section "Object fetching"
        aa_log "Fetching object from ORM"
        set orm_object [::xo::db::CrClass get_instance_from_db -item_id $object_id]
        aa_log "Fetching object from DB"
        ::xo::dc 1row get_object_from_db {
            select creation_date,
                   creation_user,
                   creation_ip,
                   modifying_ip,
                   package_id,
                   context_id
            from acs_objects
            where object_id = :object_id
        }
        # In CrItem some modification info is in fact creation info of
        # the live revision
        ::xo::dc 1row get_revision_object {
            select creation_user as modifying_user,
                   creation_date as last_modified
            from acs_objects
            where object_id = :revision_id
        }
        set title [::xo::dc get_value get_title {
            select title from cr_revisions
            where revision_id = :revision_id
        }]
        set attributes {
            title
            creation_date
            creation_user
            creation_ip
            package_id
            context_id
            modifying_user
            modifying_ip
            last_modified
        }
        foreach att $attributes {
            if {![aa_equals "Attribute $att in the object matches database value" [set $att] [$orm_object set $att]]} {
                aa_log "DB: [set $att]| ORM: [$orm_object set $att]"
            }
        }


        aa_section "Object manipulation"
        aa_log "Setting a different title"
        set new_title "a different title"
        $orm_object set title $new_title

        set old_context_id [$orm_object set context_id]
        # obtain a random different context_id
        set new_context_id [::xo::dc get_value get_context_id {
            select min(object_id) from acs_objects
            where object_id <> :object_id
              and (:old_context_id is null or object_id <> :old_context_id)
        }]
        aa_log "Setting a different context_id: $new_context_id"
        $orm_object set context_id $new_context_id

        aa_log "Saving the object"
        set old_revision_id [$orm_object set revision_id]
        $orm_object save
        set revision_id [$orm_object set revision_id]

        aa_true "revision_id changed after saving" {$revision_id != $old_revision_id}
        aa_true "Old revision is still there" [::xo::dc 0or1row check_old_revision {
            select 1 from cr_revisions where revision_id = :old_revision_id
        }]
        aa_true "New revision is the live revision" {
            $revision_id == [::xo::dc get_value get_live_revision {
                select live_revision from cr_items
                where item_id = :object_id
            }]
        }

        aa_log "Fetching object again from DB"
        ::xo::dc 1row get_object_from_db {
            select creation_date,
                   creation_user,
                   creation_ip,
                   modifying_ip,
                   package_id,
                   context_id
            from acs_objects
            where object_id = :object_id
        }
        # In CrItem some modification info is in fact creation info of
        # the live revision
        ::xo::dc 1row get_revision_object {
            select creation_user as modifying_user,
                   creation_date as last_modified
            from acs_objects
            where object_id = :revision_id
        }
        set title [::xo::dc get_value get_title {
            select title from cr_revisions
            where revision_id = :revision_id
        }]


        aa_section "Check modifications BEFORE refetching"
        aa_equals "title was updated"      [$orm_object set title]      $new_title
        aa_equals "context_id was updated" [$orm_object set context_id] $new_context_id
        foreach att $attributes {
            if {![aa_equals "Attribute $att in the object matches database value" [set $att] [$orm_object set $att]]} {
                aa_log "DB: [set $att]| ORM: [$orm_object set $att]"
            }
        }


        aa_section "Check modifications AFTER refetching"
        aa_log "Fetching object again from ORM"
        set orm_object [::xo::db::CrItem get_instance_from_db -item_id $object_id]
        aa_equals "title was updated"      [$orm_object set title]      $new_title
        aa_equals "context_id was updated" [$orm_object set context_id] $new_context_id
        foreach att $attributes {
            if {![aa_equals "Attribute $att in the object matches database value" [set $att] [$orm_object set $att]]} {
                aa_log "DB: [set $att]| ORM: [$orm_object set $att]"
            }
        }

        aa_section "Object deletion"
        $orm_object delete
        set orm_exists_p [::xo::db::Class exists_in_db -id $object_id]
        set db_exists_p [::xo::dc 0or1row lookup_object {
            select 1 from acs_objects where object_id = :object_id
        }]
        aa_true "Object is not there anymore" {!$orm_exists_p && !$db_exists_p}
    }
}

aa_register_case -cats {
    api smoke
} -procs {
    "::xo::require_html_procs"
    "::xo::db::list_to_values"
    "::xo::db::tcl_date"
} test_misc_core {
    Test various small xotcl-core functionalities.
} {
    ::xo::require_html_procs
    aa_true "html::a exists" {[info commands ::html::a] ne ""}

    aa_equals "xo::db::list_to_values" [xo::db::list_to_values {1 2 3}] {(VALUES ('1'),('2'),('3'))}

    aa_equals "tcl_date from oracle" [::xo::db::tcl_date 2008-08-25 tz_var secfrac_var] 2008-08-25
    aa_equals "tcl_date from oracle TZ and secfrac" "$tz_var $secfrac_var" "00 0"

    aa_equals "tcl_date from PostgreSQL type ANSI format secfrac and TZ" \
        [::xo::db::tcl_date "2017-08-08 13:19:33.264032+02" tz_var secfrac_var] "2017-08-08 13:19:33"
    aa_equals "tcl_date from PostgreSQL TZ and secfrac" "$tz_var $secfrac_var" "+02 264032"

    aa_equals "tcl_date from PostgreSQL type ANSI format secfrac no TC" \
        [::xo::db::tcl_date "2017-08-08 13:19:33.264032" tz_var secfrac_var] "2017-08-08 13:19:33"
    aa_equals "tcl_date from PostgreSQL TZ and secfrac" "$tz_var $secfrac_var" "00 264032"

    aa_equals "tcl_date from PostgreSQL type ANSI format no TC" \
        [::xo::db::tcl_date "2017-08-08 13:19:33" tz_var secfrac_var] "2017-08-08 13:19:33"
    aa_equals "tcl_date from PostgreSQL TZ and secfrac" "$tz_var $secfrac_var" "00 0"
}

aa_register_case -cats {
    api smoke
} -procs {
    "::xo::dc get_value"
    "::xo::dc 1row"
    "::xo::dc foreach"
    "::xo::dc multirow"
} test_prepared_statements {
    Tests the ::xo::dc with respect to prepared statements.
} {
    #
    # get_value
    #

    aa_false "::xo::dc get_value with 0 parameters, unprepared statement - no error" [catch {
        set object_id [::xo::dc get_value one_object {select max(object_id) from acs_objects}]
    }]

    aa_false "::xo::dc get_value with 0 parameters, prepared statement - no error" [catch {
        set object_id [::xo::dc get_value -prepare "" one_object {
            select max(object_id) from acs_objects
        }]
    }]

    #
    # 1row
    #

    aa_false "::xo::dc 1row with 1 parameter, unprepared statement - no error" [catch {
        ::xo::dc 1row get_object {
            select object_id as object_id_found_1
            from acs_objects where object_id = :object_id
        }
    }]

    aa_equals "::xo::dc 1row with 1 parameter, unprepared statement - value was returned" \
        $object_id $object_id_found_1

    aa_false "::xo::dc 1row with 1 parameter, prepared statement" [catch {
        ::xo::dc 1row -prepare integer get_object {
            select object_id as object_id_found_2
            from acs_objects where object_id = :object_id
        }
    }]

    aa_equals "::xo::dc 1row with 1 parameter, prepared statement - value was returned" \
        $object_id $object_id_found_2

    aa_false "::xo::dc 1row with 2 parameters, unprepared statement - no error" [catch {
        ::xo::dc 1row get_object {
            select object_id as object_id_found_3
            from acs_objects where object_id = :object_id and object_id = :object_id
        }
    }]

    aa_equals "::xo::dc 1row with 2 parameters, unprepared statement - value was returned" \
        $object_id $object_id_found_3

    aa_false "::xo::dc 1row with 2 parameters, prepared statement" [catch {
        ::xo::dc 1row -prepare integer,integer get_object {
            select object_id as object_id_found_4
            from acs_objects where object_id = :object_id and object_id = :object_id
        }
    }]

    if {[info procs ns_pg_prepare] ne ""} {

        #
        # ns_pg_prepare is implemented via tcl fallback: this
        # NaviServer version will not support prepared statements
        # where the query contains strings with colon.
        #
        set aa_error_level $::aa_error_level
        set ::aa_error_level warning
        aa_log_result fail "This NaviServer version does not support prepared statements with strings containing colons."
        set ::aa_error_level $aa_error_level

    } else {

        aa_false "::xo::dc 1row with 1 parameter, prepared statement with SQL containing colon - no error" [catch {
            ::xo::dc 1row -prepare integer get_object {
                select object_id as object_id_found_6
                from acs_objects where object_id = :object_id and title <> '__I:Do:Not:Exist'
            }
        }]

        aa_equals "::xo::dc 1row with 1 parameter, prepared statement with SQL containing colon - value was returned" \
            $object_id $object_id_found_6

        aa_false "::xo::dc 1row with 1 parameter, prepared statement with SQL containing colon - no error" [catch {
            ::xo::dc 1row -prepare integer get_object {
                select object_id as object_id_found_7
                from acs_objects
                where object_id = :object_id
                and title <> ':__I:Do:Not:Exist'
                and title <> ' :__I::also:Do:Not:Exist'
            }
        }]

        aa_equals "::xo::dc 1row with 1 parameter, prepared statement with SQL containing colon - value was returned" \
            $object_id $object_id_found_7

    }

    #
    # foreach
    #

    aa_false "::xo::dc foreach with 1 parameter - no error" [catch {
        set l [list]
        ::xo::dc foreach get_object {
            select object_id as object_id_found_8
            from acs_objects
           where object_id = :object_id
        } {
            lappend l $object_id_found_8
        }
    }]

    aa_equals "::xo::dc foreach with 1 parameter - value was returned" \
        $l [list $object_id]

    aa_false "::xo::dc foreach with 1 parameter, prepared statement - no error" [catch {
        set l2 [list]
        ::xo::dc foreach -prepare integer get_object {
            select object_id as object_id_found_9
            from acs_objects
           where object_id = :object_id
        } {
            lappend l2 $object_id_found_9
        }
    }]

    aa_equals "::xo::dc foreach with 1 parameter, prepared statement - value was returned" \
        $l2 [list $object_id]

    #
    # multirow
    #

    aa_false "::xo::dc multirow with 1 parameter - no error" [catch {
        set l3 [list]
        ::xo::dc multirow test get_object {
            select object_id as object_id_found_10
            from acs_objects
           where object_id = :object_id
        } {
            lappend l3 $object_id_found_10
        }
    }]

    aa_equals "::xo::dc multirow with 1 parameter - value was returned" \
        $l3 [list $object_id]

    aa_false "::xo::dc multirow with 1 parameter, prepared statement - no error" [catch {
        ::xo::dc multirow -prepare integer test get_object {
            select object_id + 1 as object_id_found_10
            from acs_objects
           where object_id = :object_id
        } {
            lappend l3 $object_id_found_10
        }
    }]

    aa_equals "::xo::dc multirow with 1 parameter, prepared statement - value was returned" \
        $l3 [list $object_id [expr {$object_id + 1}]]

    aa_equals "::xo::dc multirow appended twice to the test multirow" \
        [::template::multirow size test] 2

}

aa_register_case -cats {
    api smoke
} -procs {
    "::xo::dc multirow"
} test_multirow {
    Tests the ::xo::dc multirow api
} {
    aa_section "Test that ::xo::dc multirow behaves as db_multirow with respect to Bug 3441"
    #
    # Create a multirow with 0 entries and append a row "manually"
    # For details, see # https://openacs.org/bugtracker/openacs/bug?bug_number=3441
    #
    ::xo::dc multirow __xotcl_person_mr1 noxql {
        SELECT person_id, first_names, last_name
        FROM persons WHERE false
    }

    aa_equals "have empty multirow" [template::multirow size __xotcl_person_mr1] 0
    template::multirow append __xotcl_person_mr1 1234 “Ed” “Grooberman”
    aa_equals "have one tuple in multirow" [template::multirow size __xotcl_person_mr1] 1

    aa_equals "columns empty" \
        [template::multirow columns __xotcl_person_mr1] \
        "person_id first_names last_name"

    set user_id [ad_conn user_id]
    ::xo::dc multirow person_mr2 noxql {
        SELECT person_id, first_names, last_name
        FROM persons where person_id = :user_id
    }
    aa_equals "columns nonempty" \
        [template::multirow columns person_mr2] \
        "person_id first_names last_name"

    aa_section "Create a new multirow via ::xo::dc, then append via the ::template api"

    # We set d outside the multirow body to show that the variable
    # will be reinitialized at every loop.
    set d a

    ::xo::dc multirow -local t -extend {d e} __test_multirow q {
        select *
        from (values (1, 2, 3), (4, 5, 6), (7, 8, 9), (10, 11, 12), (666, 666, 666)) as t (a, b, c)
    } {
        # Test issuing continue in the loop
        if {$a == 7} {
            continue
        }

        # Test issuing break in the loop
        if {$c == 12} {
            break
        }

        # Test changing a "native" column
        incr a

        # Test changing an extended column (var existed outside)
        append d a
    }

    aa_equals "columns nonempty" \
        [template::multirow -local columns __test_multirow] \
        {a b c d e}

    aa_equals "size is 2" [template::multirow -local size __test_multirow] 2

    set template {
        <ul>
        <multiple name="__test_multirow">
        <li>
        |@__test_multirow.a@|
        @__test_multirow.b@|
        @__test_multirow.c@|
        @__test_multirow.d@|
        @__test_multirow.e@
        </li>
        </multiple>
        </ul>
    }

    set code [template::adp_compile -string $template]

    set expected {
        <ul>
        <li>|2|2|3|a|</li>
        <li>|5|5|6|a|</li>
        </ul>
    }

    aa_equals "Template returns expected result" \
        [join [template::adp_eval code] ""] [join $expected ""]

    template::multirow -local append __test_multirow I am appended to multirow

    set expected {
        <ul>
        <li>|2|2|3|a|</li>
        <li>|5|5|6|a|</li>
        <li>|I|am|appended|to|multirow</li>
        </ul>
    }

    aa_equals "Template returns expected result after appending to the multirow" \
        [join [template::adp_eval code] ""] [join $expected ""]


    aa_section "Create a multirow via the ::template api, then append via the ::xo::dc interface"
    template::multirow -local create __test_multirow_2 a b c
    template::multirow -local append __test_multirow_2 1 2 3

    ::xo::dc multirow -local t __test_multirow_2 q {
        select *
        from (values (4, 5, 6), (7, 8, 9)) as t (a, b, c)
    }

    aa_equals "size is 3" [template::multirow -local size __test_multirow_2] 3

    aa_section "Append again via ::xo::dc"

    ::xo::dc multirow -extend {b c} -local t __test_multirow_2 q {
        select *
        from (values (10), (13)) as t (a)
    } {
        set b [expr {$a + 1}]
        set c [expr {$b + 1}]
    }

    aa_equals "size is 5" [template::multirow -local size __test_multirow_2] 5

    set template {
        <ul>
        <multiple name="__test_multirow_2">
        <li>
        |@__test_multirow_2.a@|
        @__test_multirow_2.b@|
        @__test_multirow_2.c@|
        </li>
        </multiple>
        </ul>
    }

    set code [template::adp_compile -string $template]

    set expected {
        <ul>
        <li>|1|2|3|</li>
        <li>|4|5|6|</li>
        <li>|7|8|9|</li>
        <li>|10|11|12|</li>
        <li>|13|14|15|</li>
        </ul>
    }

    aa_equals "Template returns expected result after appending to the multirow" \
        [join [template::adp_eval code] ""] [join $expected ""]
}

aa_register_case -cats {
    api smoke
} -procs {
    "::xo::dc foreach"
    "::xo::dc multirow"
} test_db_out_of_pools {

    Makes sure the ::xo::dc api does not incur in the "out of pools"
    bug when nested looping idioms are executed.

} {
    set one_too_many_pools [expr {[llength [db_available_pools ""]] + 1}]

    aa_false "Nesting $one_too_many_pools '::xo::dc foreach' does not return an error" [catch {
        set code {}
        for {set i 0} {$i < $one_too_many_pools} {incr i} {
            set code "::xo::dc foreach q {select 1 from dual} {$code}"
        }
        eval $code
    }]

    aa_false "Nesting $one_too_many_pools '::xo::dc multirow' does not return an error" [catch {
        set code {}
        for {set i 0} {$i < $one_too_many_pools} {incr i} {
            set code "::xo::dc multirow test_$i q {select 1 from dual} {$code}"
        }
        eval $code
    }]

}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
