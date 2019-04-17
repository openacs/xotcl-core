ad_library {
    Test xotcl-core features
}


aa_register_case -cats {api smoke} test_xo_db_object {
   Test basic ::xo::db::Object ORM features
} {
    aa_run_with_teardown -test_code {


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
              and object_id <> :old_context_id
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

aa_register_case -cats {api smoke} test_cr_items {
   Test basic ::xo::db::CrItem ORM features
} {
    aa_run_with_teardown -test_code {

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
                   package_id,
                   context_id
            from acs_objects
            where object_id = :object_id
        }
        # In CrItem modification info is in fact creation info of the
        # live revision
        ::xo::dc 1row get_revision_object {
            select creation_user as modifying_user,
                   creation_date as last_modified,
                   creation_ip   as modifying_ip
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
              and object_id <> :old_context_id
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
                   package_id,
                   context_id
            from acs_objects
            where object_id = :object_id
        }
        # In CrItem modification info is in fact creation info of the
        # live revision
        ::xo::dc 1row get_revision_object {
            select creation_user as modifying_user,
                   creation_date as last_modified,
                   creation_ip   as modifying_ip
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


# Local variables:
#    mode: tcl
#    tcl-indent-level: 4
#    indent-tabs-mode: nil
# End:
