ad_library {
    Test xotcl-core features
}


aa_register_case -cats {api smoke} test_xo_db_object {
   Test basic ::xo::db::Object ORM features
} {
    aa_run_with_teardown -test_code {
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
        aa_equals "Object was created" \
            [string is true -strict $orm_exists_p] [string is true -strict $db_exists_p]

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
        foreach att {
            object_title
            creation_date
            creation_user
            creation_ip
            package_id
            context_id
            modifying_user
            modifying_ip
            last_modified
        } {
            aa_equals "Same $att" [set $att] [$orm_object set $att]
        }

        aa_log "Setting a different title"
        set old_title [$orm_object set object_title]
        $orm_object set object_title "a different title"

        set old_context_id [$orm_object set context_id]
        # obtain a random different context_id
        set new_context_id [::xo::dc get_value get_context_id {
            select min(object_id) from acs_objects
            where object_id <> :object_id
        }]
        aa_log "Setting a different context_id: $new_context_id"
        $orm_object set context_id $new_context_id

        aa_log "Saving the object"
        $orm_object save

        aa_log "Fetching object again from DB"
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

        # Before re-fetching the object from db, some properties will
        # not be updated in the object. We expose this behavior, which
        # is somehow suboptimal.
        foreach att {
            modifying_user
            last_modified
        } {
            aa_true "Attribute $att didn't change before re-fetching" {[set $att] ne [$orm_object set $att]}
        }

        aa_log "Fetching object again from ORM"
        set orm_object [::xo::db::Class get_instance_from_db -id $object_id]

        # Title would not be updated. This is suboptimal and we expose it
        aa_equals "Title did not change" [$orm_object set object_title] $old_title

        aa_equals "context_id changed" [$orm_object set context_id] $new_context_id

        foreach att {
            object_title
            creation_date
            creation_user
            creation_ip
            package_id
            context_id
            modifying_user
            modifying_ip
            last_modified
        } {
            aa_equals "Same $att after modifying the title" [set $att] [$orm_object set $att]
        }

        $orm_object delete
        set orm_exists_p [::xo::db::Class exists_in_db -id $object_id]
        set db_exists_p [::xo::dc 0or1row lookup_object {
            select 1 from acs_objects where object_id = :object_id
        }]
        aa_equals "Object is not there anymore" \
            [string is true -strict $orm_exists_p] [string is true -strict $db_exists_p]
    }
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
