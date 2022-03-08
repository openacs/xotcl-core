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


# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
