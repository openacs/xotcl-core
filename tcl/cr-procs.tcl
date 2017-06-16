::xo::library doc {
  XOTcl for the Content Repository 

  @author Gustaf Neumann
  @creation-date 2007-08-13
  @cvs-id $Id$
}

namespace eval ::xo::db {

  ::xotcl::Class create ::xo::db::CrClass \
      -superclass ::xo::db::Class \
      -parameter {
        {supertype content_revision}
        form
        edit_form
        {mime_type text/plain}
        {storage_type "text"}
        {folder_id -100}
        {non_cached_instance_var_patterns {__*}}
      } -ad_doc {
        <p>The meta class CrClass serves for a class of applications that mostly 
        store information in the content repository and that use a few 
        attributes adjoining this information. The class handles the open 
        acs object_type creation and the automatic creation of the 
        necessary tables based on instances of this meta-class.</p>
        
        <p>The definition of new types is handled in the constructor of 
        CrType through the method 
        <a href='#instproc-create_object_type'>create_object_type</a>, 
        the removal of the
        object type is handled through the method 
        <a href='#instproc-drop_object_type'>drop_object_type</a>
        (requires that 
         all instances of this type are deleted).</p>

        <p>Each content item can be retrieved either through the 
        general method 
        <a href='/api-doc/proc-view?proc=::xo::db::CrClass+proc+get_instance_from_db'>
        CrClass get_instance_from_db</a> or through the "get_instance_from_db" method of 
        every subclass of CrItem.

        <p>This Class is a meta-class providing methods for Classes 
        managing CrItems.</p>
      }

  #
  # Methods for the meta class
  #

  CrClass ad_proc get_object_type {
    -item_id:integer,required
    {-revision_id:integer 0}
  } {
    Return the object type for an item_id or revision_id.

    @return object_type typically an XOTcl class
  } {
    #
    # Use a request-spanning cache. When the type whould changes, we
    # would require xo::broadcast or server restart.
    #
    set key ::xo::object_type($item_id,$revision_id)
    if {[info exists $key]} {
      return [set $key]
    }
    set $key [ns_cache eval xotcl_object_type_cache \
                  [expr {$item_id ? $item_id : $revision_id}] {
                    if {$item_id} {
                      ::xo::dc 1row get_class_from_item_id \
                          "select content_type as object_type from cr_items where item_id=:item_id"
                    } else {
                      ::xo::dc 1row get_class_from_revision_id \
                          "select object_type from acs_objects where object_id=:revision_id"
                    }
                    return $object_type
                  }]
  }

  CrClass ad_proc get_instance_from_db {
    {-item_id:integer 0}
    {-revision_id:integer 0}
    {-initialize:boolean true}
  } {
    Instantiate the live revision or the specified revision of an 
    CrItem. The XOTcl object is destroyed automatically on cleanup 
    (end of a connection request).

    @return fully qualified object containing the attributes of the CrItem
  } { 
    set object_type [my get_object_type -item_id $item_id -revision_id $revision_id]
    set class [::xo::db::Class object_type_to_class $object_type]
    return [$class get_instance_from_db -item_id $item_id -revision_id $revision_id -initialize $initialize]
  }

  CrClass ad_proc get_parent_id {
    -item_id:required
  } {
    Get the parent_id of a content item either from an already instantiated
    object or from the database without instantiating it. If item_id is not 
    a valid item_id, we throw an error.

    @return parent_id
  } { 
    # TODO: the following line is deactivated, until we get rid of the "folder object" in xowiki
    #if {[my isobject ::$item_id]} {return [::$item_id parent_id]}
    ::xo::dc 1row get_parent "select parent_id from cr_items where item_id = :item_id"
    return $parent_id
  }

  CrClass ad_proc get_name {
    -item_id:required
  } {
    Get the name of a content item either from an already instantiated object 
    or from the database without instantiating it. If item_id is not a valid 
    item_id, we throw an error.

    @return parent_id
  } { 
    # TODO: the following line is deactivated, until we get rid of the "folder object" in xowiki
    #if {[my isobject ::$item_id]} {return [::$item_id parent_id]}
    ::xo::dc 1row get_name "select name from cr_items where item_id = :item_id"
    return $name
  }

  CrClass ad_proc get_child_item_ids {
    -item_id:required
  } {
    Return a list of content items having the provided item_id as 
    direct or indirect parent. The method returns recursively all 
    item_ids.

    @return list of item_ids
  } {
    set items [list]
    foreach item_id [::xo::dc list get_child_items \
                         "select item_id from cr_items where parent_id = :item_id"] {
      lappend items $item_id {*}[my [self proc] -item_id $item_id]
    }
    return $items
  }

  CrClass ad_proc lookup {
    -name:required
    {-parent_id -100}
    {-content_type "::%"}
  } {
    Check, whether an content item with the given name exists.
    If the item exists, return its item_id, otherwise 0.

    @return item_id
  } {
    return [::xo::dc get_value entry_exists_select {
      select item_id from cr_items 
      where name = :name and parent_id = :parent_id
      and content_type like :content_type
    } 0]
  }
  

  CrClass ad_proc delete {
    -item_id 
  } {
    Delete a CrItem in the database
  } {
    set object_type [my get_object_type -item_id $item_id]
    $object_type delete -item_id $item_id
  }

  CrClass instproc unknown { obj args } {
    # When this happens, this is most likely an error. Ease debugging
    # by writing the call stack to the error log.
    ::xo::show_stack
    my log "::xo::db::CrClass: unknown called with $obj $args"
  }

  #
  # Deal with locking requirements
  # 
  if {[db_driverkey ""] eq "postgresql"} {
    #
    # PostgreSQL
    #
    set pg_version [::xo::dc get_value get_version {
      select substring(version() from 'PostgreSQL #"[0-9]+.[0-9]+#"%' for '#')   }]
    ns_log notice "--Postgres Version $pg_version"      
    if {$pg_version < 8.2} {
      ns_log notice "--Postgres Version $pg_version older than 8.2, use locks"
      #
      # We define a locking function, really locking the tables...
      #
      CrClass instproc lock {tablename mode} {
        ::xo::dc dml fix_content_length "update cr_revisions "
        ::xo::dc lock_objects "LOCK TABLE $tablename IN $mode MODE"
      }
    } else {
      # No locking needed for newer versions of PostgreSQL
      CrClass instproc lock {tablename mode} {;}
    }
  } else {
    #
    # Oracle
    #
    # No locking needed for known versions of Oracle
    CrClass instproc lock {tablename mode} {;}
  }

  #
  # Generic part (independent of Postgres/Oracle)
  #

  CrClass instproc type_selection_clause {{-base_table cr_revisions} {-with_subtypes:boolean false}} {
    my instvar object_type
    if {$with_subtypes} {
      if {$base_table eq "cr_revisions"} {
        # do type selection manually
        return "acs_objects.object_type in ([my object_types_query])"
      }
      # the base-table defines contains the subtypes
      return ""
    } else {
      if {$base_table eq "cr_revisions"} {
        return "acs_objects.object_type = '$object_type'"
      } else {
        return "bt.object_type = '$object_type'"
      }
    }
  }
  

  #
  # database version (Oracle/PG) independent code
  #


  CrClass set common_query_atts {
    object_type  
    creation_user creation_date
    publish_status last_modified 
  }
  if {[apm_version_names_compare [ad_acs_version] 5.2] > -1} {
    CrClass lappend common_query_atts package_id
  }

  CrClass instproc edit_atts {} {
    # TODO remove, when name and text are slots (only for generic)
    my array names db_slot
  }

  CrClass ad_instproc folder_type_unregister_all {
    {-include_subtypes t}
  } {
    Unregister the object type from all folders on the system

    @param include_subtypes Boolean value (t/f) to flag whether the 
    operation should be applied on subtypes as well
  } {
    my instvar object_type
    xo::dc foreach all_folders { 
      select folder_id from cr_folder_type_map 
      where content_type = :object_type
    } {
      ::xo::db::sql::content_folder unregister_content_type \
          -folder_id $folder_id \
          -content_type $object_type \
          -include_subtypes $include_subtypes
    }
  }

  CrClass ad_instproc folder_type {
    {-include_subtypes t}
    -folder_id
    operation
  } {
    register the current object type for folder_id. If folder_id 
    is not specified, use the instvar of the class instead.

    @param include_subtypes Boolean value (t/f) to flag whether the 
    operation should be applied on subtypes as well
  } {
    if {$operation ne "register" && $operation ne "unregister"} {
      error "[self] operation for folder_type must be 'register' or 'unregister'"
    }
    my instvar object_type
    if {![info exists folder_id]} {
      my instvar folder_id
    }
    ::xo::db::sql::content_folder ${operation}_content_type \
        -folder_id $folder_id \
        -content_type $object_type \
        -include_subtypes $include_subtypes
  }

  CrClass ad_instproc create_object_type {} {
    Create an oacs object_type and a table for keeping the
    additional attributes.
  } {
    my instvar object_type supertype pretty_name pretty_plural \
        table_name id_column name_method

    my check_table_atts

    set supertype [my info superclass]
    switch -- $supertype {
      ::xotcl::Object -
      ::xo::db::CrItem {set supertype content_revision}
    }
    if {![info exists pretty_plural]} {set pretty_plural $pretty_name}

    ::xo::dc transaction {
      ::xo::db::sql::content_type create_type \
          -content_type $object_type \
          -supertype $supertype \
          -pretty_name $pretty_name \
          -pretty_plural $pretty_plural \
          -table_name $table_name \
          -id_column $id_column \
          -name_method $name_method
      
      my folder_type register
    }
  }



  CrClass ad_instproc drop_object_type {} {
    Delete the object type and remove the table for the attributes.
    This method should be called when all instances are deleted. It
    undoes everying what create_object_type has produced.
  } {
    my instvar object_type table_name
    ::xo::dc transaction {
      my folder_type unregister
      ::xo::db::sql::content_type drop_type \
          -content_type $object_type \
          -drop_children_p t \
          -drop_table_p t
    }
  }

  CrClass ad_proc require_folder_object {
    -folder_id
    -package_id 
  } {
    Dummy stub; let specializations define it
  } {
  }

  CrClass instproc getFormClass {-data:required} {
    if {[$data exists item_id] && [$data set item_id] != 0 && [my exists edit_form]} {
      return [my edit_form]
    } else {
      return [my form]
    }
  }

  CrClass instproc remember_long_text_slots {} {
    #
    # keep long_text_slots in a separate array (for Oracle)
    #
    my array unset long_text_slots
    foreach {slot_name slot} [my array get db_slot] {
      if {[$slot sqltype] eq "long_text"} {
        my set long_text_slots($slot_name) $slot
      }
    }
    #my log "--long_text_slots = [my array names long_text_slots]"
  }

  #
  # ::xo::db::Class creates automatically save and insert methods.
  # For the content repository classes (created with CrClass) we use
  # for the time being the automatically created views for querying
  # and saving (save and save_new).  Therefore, we overwrite for
  # CrClass the generator methods.
  #
  CrClass instproc mk_save_method {} {;}
  CrClass instproc mk_insert_method {} {;}

  CrClass instproc init {} {
    my instvar object_type db_slot
    # first, do whatever ::xo::db::Class does for initialization ...
    next
    # We want to be able to define for different CrClasses different
    # default mime-types. Therefore, we define attribute slots per 
    # application class with the given default for mime_type. 
    if {[self] ne "::xo::db::CrItem"} {
      my slots {
        ::xotcl::Attribute create mime_type -default [my mime_type]
      }
      my db_slots
    }
    # ... then we do the CrClass specific initialization.
    #if {[my info superclass] ne "::xo::db::CrItem"} {
    #  my set superclass [[my info superclass] set object_type]
    #}

    # CrClasses store all attributes of the class hierarchy in
    # db_slot. This is due to the usage of the
    # automatically created views. Note, that classes created with
    # ::xo::db::Class keep only the class specific db slots.
    #
    foreach {slot_name slot} [[my info superclass] array get db_slot] {
      # don't overwrite slots, unless the object_title (named title)
      if {![info exists db_slot($slot_name)] ||
          $slot eq "::xo::db::Object::slot::object_title"} {
        set db_slot($slot_name) $slot
      }
    }
    my remember_long_text_slots
    
    if {![::xo::db::Class object_type_exists_in_db -object_type $object_type]} {
      my create_object_type
    }
  }
  

  CrClass ad_instproc fetch_object {
    -item_id:required
    {-revision_id 0}
    -object:required
    {-initialize:boolean true}
  } {
    Load a content item into the specified object. If revision_id is
    provided, the specified revision is returned, otherwise the live
    revision of the item_id. If the object does not exist, we create it.

    @return cr item object
  } {
    #my log "-- generic fetch_object [self args]"
    if {![::xotcl::Object isobject $object]} {
      # if the object does not yet exist, we have to create it
      my create $object
    }
    set raw_atts [::xo::db::CrClass set common_query_atts]
    #my log "-- raw_atts = '$raw_atts'"

    set atts [list]
    foreach v $raw_atts {
      switch -glob -- $v {
        publish_status {set fq i.$v}
        creation_date  {set fq o.$v}
        creation_user  {set fq o.$v}
        package_id     {set fq o.$v}
        default        {set fq n.$v}
      }
      lappend atts $fq
    }
    foreach {slot_name slot} [my array get db_slot] {
      switch -- $slot {
        ::xo::db::CrItem::slot::text {
          # We need the rule, since insert the handling of the sql
          # attribute "text" is somewhat magic. On insert, one can use the
          # automatic view with column_name "text, on queries, one has to use
          # "data". Therefore, we cannot use simply -column_name for the slot.
          lappend atts "n.data AS text"
        }

        ::xowiki::Page::slot::text {
          #
          # this is just a hotfix for now
          #
          #ns_log notice [$slot serialize]
          lappend atts "n.data as text"
        }
        
        ::xo::db::CrItem::slot::name {
          lappend atts i.[$slot column_name]
        }
        default {
          lappend atts n.[$slot column_name]
        }
      }
    }
    if {$revision_id} {
      $object set revision_id $revision_id

      db_with_handle db {
        set sql [::xo::dc prepare -handle $db -argtypes integer "\
               select [join $atts ,], i.parent_id \
               from   [my set table_name]i n, cr_items i,acs_objects o \
               where  n.revision_id = :revision_id \
               and    i.item_id = n.item_id \
               and    o.object_id = n.revision_id"]
        
        set selection [db_exec 1row $db dbqd..cr-procs-fetch_object-from-revision_id $sql]
      }
      $object mset [ns_set array $selection]

    } else {
      # We fetch the creation_user and the modifying_user by returning the 
      # creation_user of the automatic view as modifying_user. In case of
      # troubles, comment next line out.
      lappend atts "n.creation_user as modifying_user"
      
      $object set item_id $item_id
     
      $object db_1row [my qn fetch_from_view_item_id] "\
       select [join $atts ,], i.parent_id \
       from   [my set table_name]i n, cr_items i, acs_objects o \
       where  i.item_id = :item_id \
       and    n.[my id_column] = coalesce(i.live_revision, i.latest_revision) \
       and    o.object_id = i.item_id"
    }
    # the method db_1row treats all newly created variables as instance variables,
    # so we can see vars like __db_sql, __db_lst that we do not want to keep
    foreach v [$object info vars __db_*] {$object unset $v}

    if {[apm_version_names_compare [ad_acs_version] 5.2] <= -1} {
      $object set package_id [::xo::dc get_value get_pid \
                                  "select package_id from cr_folders where folder_id = [$object set parent_id]"]
    }

    #my log "--AFTER FETCH\n[$object serialize]"
    if {$initialize} {$object initialize_loaded_object}
    return $object
  }


  CrClass ad_instproc get_instance_from_db {
    {-item_id 0}
    {-revision_id 0}
    {-initialize:boolean true}
  } { 
    Retrieve either the live revision or a specified revision
    of a content item with all attributes into a newly created object.
    The retrieved attributes are strored in the instance variables in
    class representing the object_type. The XOTcl object is
    destroyed automatically on cleanup (end of a connection request)

    @param item_id id of the item to be retrieved.
    @param revision_id revision-id of the item to be retrieved.
    @return fully qualified object
  } {
    set object ::[expr {$revision_id ? $revision_id : $item_id}]
    if {![my isobject $object]} {
      my fetch_object -object $object \
          -item_id $item_id -revision_id $revision_id \
          -initialize $initialize
      $object destroy_on_cleanup
    }
    return $object
  }

  CrClass ad_instproc new_persistent_object {-package_id -creation_user -creation_ip args} {
    Create a new content item of the actual class,
    configure it with the given arguments and 
    insert it into the database.  The XOTcl object is
    destroyed automatically on cleanup (end of a connection request).

    @return fully qualified object
  } {
    my get_context package_id creation_user creation_ip
    #my log "ID [self] create $args"
    if {[catch {set p [my create ::0 {*}$args]} errorMsg]} {
      ad_log error $errorMsg
    }
    #my log "ID [::0 serialize]"
    set item_id [::0 save_new \
                     -package_id $package_id \
                     -creation_user $creation_user \
                     -creation_ip $creation_ip]
    ::0 move ::$item_id
    ::$item_id destroy_on_cleanup
    return ::$item_id
  }

  CrClass ad_instproc delete {
    -item_id:required
  } { 
    Delete a content item from the content repository.
    @param item_id id of the item to be deleted
  } {
    ::xo::db::sql::content_item del -item_id $item_id
  }


  CrClass ad_instproc instance_select_query {
    {-select_attributes ""}
    {-orderby ""}
    {-where_clause ""}
    {-from_clause ""}
    {-with_subtypes:boolean true}
    {-with_children:boolean false}
    {-publish_status}
    {-count:boolean false}
    {-folder_id}
    {-parent_id}
    {-page_size 20}
    {-page_number ""}
    {-base_table "cr_revisions"}
  } {
    returns the SQL-query to select the CrItems of the specified object_type
    @param select_attributes attributes for the sql query to be retrieved, in addition
    to item_id, name, publish_status, object_type, and package_id 
    which are always returned
    @param orderby for ordering the solution set
    @param where_clause clause for restricting the answer set
    @param with_subtypes return subtypes as well
    @param with_children return immediate child objects of all objects as well
    @param count return the query for counting the solutions
    @param folder_id parent_id
    @param publish_status one of 'live', 'ready', or 'production'
    @param base_table typically automatic view, must contain title and revision_id
    @return sql query
  } {
    if {![info exists folder_id]} {my instvar folder_id}
    if {![info exists parent_id]} {set parent_id $folder_id}

    if {$base_table eq "cr_revisions"} {
      set attributes [list ci.item_id ci.name ci.publish_status acs_objects.object_type acs_objects.package_id] 
    } else {
      set attributes [list bt.item_id ci.name ci.publish_status bt.object_type "bt.object_package_id as package_id"] 
    }
    foreach a $select_attributes {
      if {$a eq "title"} {set a bt.title}
      lappend attributes $a
    }
    set type_selection_clause [my type_selection_clause -base_table $base_table -with_subtypes $with_subtypes]
    #my log "type_selection_clause -with_subtypes $with_subtypes returns $type_selection_clause"
    if {$count} {
      set attribute_selection "count(*)"
      set orderby ""      ;# no need to order when we count
      set page_number  ""      ;# no pagination when count is used
    } else {
      set attribute_selection [join $attributes ,]
    }
    
    set cond [list]
    if {$type_selection_clause ne ""} {lappend cond $type_selection_clause}
    if {$where_clause ne ""}          {lappend cond $where_clause}
    if {[info exists publish_status]} {lappend cond "ci.publish_status = $publish_status"}
    if {$base_table eq "cr_revisions"} {
      lappend cond "acs_objects.object_id = bt.revision_id"
      set acs_objects_table "acs_objects, "
    } else {
      lappend cond "ci.item_id = bt.item_id"
      set acs_objects_table ""
    }
    lappend cond "coalesce(ci.live_revision,ci.latest_revision) = bt.revision_id"
    if {$parent_id ne ""} {
      if {$with_children} {
        append from_clause ", (select $parent_id as item_id from dual union \
            select item_id from cr_items where parent_id = $parent_id) children"
        lappend cond "ci.parent_id = children.item_id"
      } else {
        lappend cond "ci.parent_id = $parent_id"
      }
    }

    if {$page_number ne ""} {
      set limit $page_size
      set offset [expr {$page_size*($page_number-1)}]
    } else {
      set limit ""
      set offset ""
    }

    set sql [::xo::dc select \
                 -vars $attribute_selection \
                 -from "$acs_objects_table cr_items ci, $base_table bt $from_clause" \
                 -where [join $cond " and "] \
                 -orderby $orderby \
                 -limit $limit -offset $offset]
    #my log "--sql=$sql"
    return $sql
  }

  CrClass ad_instproc get_instances_from_db {
    {-select_attributes ""}
    {-from_clause ""}
    {-where_clause ""}
    {-orderby ""}
    {-with_subtypes:boolean true}
    {-folder_id}
    {-page_size 20}
    {-page_number ""}
    {-base_table "cr_revisions"}
    {-initialize true}
  } {
    Returns a set (ordered composite) of the answer tuples of 
    an 'instance_select_query' with the same attributes.
    The tuples are instances of the class, on which the 
    method was called.
  } {
    set s [my instantiate_objects -sql \
               [my instance_select_query \
                    -select_attributes $select_attributes \
                    -from_clause $from_clause \
                    -where_clause $where_clause \
                    -orderby $orderby \
                    -with_subtypes $with_subtypes \
                    -folder_id $folder_id \
                    -page_size $page_size \
                    -page_number $page_number \
                    -base_table $base_table \
                   ] \
              -initialize $initialize]
    return $s
  }


  ##################################

  ::xo::db::CrClass create ::xo::db::CrItem \
      -superclass ::xo::db::Object \
      -table_name cr_revisions -id_column revision_id \
      -object_type content_revision \
      -slots {
        #
        # The following attributes are from cr_revisions
        #
        ::xo::db::CrAttribute create item_id \
            -datatype integer \
            -pretty_name "Item ID" -pretty_plural "Item IDs" \
            -references "cr_items on delete cascade"
        ::xo::db::CrAttribute create title \
            -sqltype varchar(1000) \
            -pretty_name "Title" -pretty_plural "Titles"
        ::xo::db::CrAttribute create description \
            -sqltype text \
            -pretty_name "Description" -pretty_plural "Descriptions"
        ::xo::db::CrAttribute create publish_date \
            -datatype date
        ::xo::db::CrAttribute create mime_type \
            -sqltype varchar(200) \
            -pretty_name "Mime Type" -pretty_plural "Mime Types" \
            -default text/plain -references cr_mime_types
        ::xo::db::CrAttribute create nls_language \
            -sqltype varchar(50) \
            -pretty_name "Language" -pretty_plural "Languages" \
            -default en_US
        # lob, content, content_length
        #
        # "magic attribute "text"
        ::xo::db::CrAttribute create text \
            -pretty_name "Text" \
            -create_table_attribute false \
            -create_acs_attribute false
        # missing: attribute from cr_items
        ::xo::db::CrAttribute create name \
            -pretty_name "Name" \
            -create_table_attribute false \
            -create_acs_attribute false
      } \
      -parameter {
        package_id 
        {parent_id -100}
        {publish_status ready}
      }

  CrItem::slot::revision_id default 0

  CrItem instproc initialize_loaded_object {} {
    # empty body, to be refined
  }

  if {[db_driverkey ""] eq "postgresql"} {
    #
    # PostgreSQL
    # 

    #
    # INSERT statements differ between PostgreSQL and Oracle
    # due to the handling of CLOBS.
    #
    CrClass instproc insert_statement {atts vars} {
      return "insert into [my set table_name]i ([join $atts ,]) \
                values (:[join $vars ,:])"
    }

    CrItem instproc fix_content {revision_id content} {
      [my info class] instvar storage_type 
      #my msg "--long_text_slots: [[my info class] array get long_text_slots]"
      #foreach {slot_name slot} [[my info class] array get long_text_slots] {
      #  set cls [$slot domain]
      #  set content [my set $slot_name]
      #  my msg "$slot_name [$cls table_name] [$cls id_column] length=[string length $content]"
      #}
      if {$storage_type eq "file"} {
        ::xo::dc dml fix_content_length "update cr_revisions \
                set content_length = [file size [my set import_file]] \
                where revision_id = :revision_id"
      }
    }

    CrItem instproc update_content {revision_id content} {
      #
      # This method can be use to update the content field (only this) of 
      # an content item without creating a new revision. This works
      # currently only for storage_type == "text".
      #
      [my info class] instvar storage_type 
      if {$storage_type eq "file"} {
        my log "--update_content not implemented for type file"
      } else {
        ::xo::dc dml update_content "update cr_revisions set content = :content \
        where revision_id = :revision_id"
      }
    }

    CrItem instproc update_attribute_from_slot {-revision_id slot value} {
      if {![info exists revision_id]} {my instvar revision_id}
      set domain [$slot domain]
      set sql "update [$domain table_name] \
                set [$slot column_name] = :value \
        where [$domain id_column] = $revision_id"
      ::xo::dc dml update_attribute_from_slot $sql
    }
  } else {
    #
    # Oracle
    #

    CrClass instproc insert_statement {atts vars} {
      #
      # The Oracle implementation of OpenACS cannot update
      # here *LOBs safely updarted through the automatic generated
      # view. So we postpone these updates and perform these
      # as separate statements.
      #
      set values [list]
      set attributes [list]
      #my msg "--long_text_slots: [my array get long_text_slots]"

      foreach a $atts v $vars {
        #
        # "text" and long_text_slots are handled in Oracle 
        # via separate update statement.
        #
        if {$a eq "text" || [my exists long_text_slots($a)]} continue
        lappend attributes $a
        lappend values $v
      }
      return "insert into [my set table_name]i ([join $attributes ,]) \
                values (:[join $values ,:])"
    }

    CrItem instproc fix_content {{-only_text false} revision_id content} {
      [my info class] instvar storage_type
      if {$storage_type eq "file"} {
        ::xo::dc dml fix_content_length "update cr_revisions \
                set content_length = [file size [my set import_file]] \
                where revision_id = :revision_id"
      } elseif {$storage_type eq "text"} {
        ::xo::dc dml fix_content "update cr_revisions \
               set    content = empty_blob(), content_length = [string length $content] \
               where  revision_id = :revision_id \
               returning content into :1" -blobs [list $content]
      }
      if {!$only_text} {
        foreach {slot_name slot} [[my info class] array get long_text_slots] {
          my update_attribute_from_slot -revision_id $revision_id $slot [my set $slot_name]
        }
      }
    }

    CrItem instproc update_content {revision_id content} {
      #
      # This method can be used to update the content field (only this) of 
      # an content item without creating a new revision. This works
      # currently only for storage_type == "text".
      #
      [my info class] instvar storage_type
      if {$storage_type eq "file"} {
        my log "--update_content not implemented for type file"
      } else {
        my fix_content -only_text true $revision_id $content
      }
    }

    CrItem instproc update_attribute_from_slot {-revision_id slot value} {
      if {![info exists revision_id]} {my instvar revision_id}
      set domain [$slot domain]
      set att [$slot column_name]
      if {[$slot sqltype] eq "long_text"} {
        ::xo::dc dml att-$att "update [$domain table_name] \
               set    $att = empty_clob() \
               where  [$domain id_column] = :revision_id \
               returning $att into :1" -clobs [list $value]
      } else {
        set sql "update [$domain table_name] \
                set $att = :value \
        where [$domain id_column] = $revision_id"
        ::xo::dc dml $att $sql
      }
    }
  }
  
  CrItem instproc update_revision {{-quoted false} revision_id attribute value} {
    #
    # This method can be use to update arbitrary fields of 
    # an revision.
    #
    if {$quoted} {set val $value} {set val :value}
    ::xo::dc dml update_content "update cr_revisions set $attribute = $val \
        where revision_id = :revision_id"
  }
  
  CrItem instproc current_user_id {} {
    if {[my isobject ::xo::cc]} {return [::xo::cc user_id]}
    if {[ad_conn isconnected]}  {return [ad_conn user_id]}
    return ""
  }

  CrItem ad_instproc save {
    -modifying_user 
    {-live_p:boolean true} 
    {-use_given_publish_date:boolean false}
  } {
    Updates an item in the content repository. We insert a new revision instead of 
    changing the current revision.
    @param modifying_user
    @param live_p make this revision the live revision
  } {
    #my instvar creation_user
    set __atts [list creation_user]
    set __vars $__atts

    # The modifying_user is not maintained by the CR (bug?)
    # xotcl-core handles this by having the modifying user as
    # creation_user of the revision.
    #
    # Caveat: the creation_user fetched is different if we fetch via
    # item_id (the creation_user is the creator of the item) or if we
    # fetch via revision_id (the creation_user is the creator of the
    # revision)

    set creation_user [expr {[info exists modifying_user] ?
                             $modifying_user :
                             [my current_user_id]}]
    #set old_revision_id [my set revision_id]

    foreach {__slot_name __slot} [[my info class] array get db_slot] {
      if {
          $__slot eq "::xo::db::Object::slot::object_title" ||
          $__slot eq "::xo::db::CrItem::slot::name" ||
          $__slot eq "::xo::db::CrItem::slot::publish_date"
        } continue
      my instvar $__slot_name
      lappend __atts [$__slot column_name]
      lappend __vars $__slot_name
    }

    if {$use_given_publish_date} {
      if {"publish_date" ni $__atts} {
        my instvar publish_date
        lappend __atts publish_date
        lappend __vars publish_date
      }
      set publish_date_flag [list -publish_date $publish_date]
    } else {
      set publish_date_flag ""
    }
    
    ::xo::dc transaction {
      [my info class] instvar storage_type
      set revision_id [xo::dc nextval acs_object_id_seq]
      if {$storage_type eq "file"} {
        my instvar import_file mime_type name
        # Get the mime_type from the file, eventually creating a new
        # one if it's unrecognized.
        set mime_type [cr_check_mime_type \
                           -mime_type $mime_type \
                           -filename  $name \
                           -file      $import_file]
        set text [cr_create_content_file $item_id $revision_id $import_file]
      }
      ::xo::dc [::xo::dc insert-view-operation] revision_add \
          [[my info class] insert_statement $__atts $__vars]

      my fix_content $revision_id $text

      if {$live_p} {
        #
        # Update the life revision with the publish status and
        # optionally the publish_date
        #
        ::xo::db::sql::content_item set_live_revision \
            -revision_id $revision_id \
            -publish_status [my set publish_status] \
            -is_latest true \
            {*}$publish_date_flag
        my set revision_id $revision_id
        my update_item_index
      } else {
        # if we do not make the revision live, use the old revision_id,
        # and let CrCache save it ...... TODO: is this still needed? comment out for testing
        #set revision_id $old_revision_id
      }
      my set modifying_user $creation_user
      my set last_modified [::xo::dc get_value get_last_modified \
                                {select last_modified from acs_objects where object_id = :revision_id}]
    }
    return $item_id
  }

  CrItem ad_instproc set_live_revision {
    -revision_id:required
    {-publish_status "ready"}
    {-is_latest:boolean false}
  } {
    @param revision_id
    @param publish_status one of 'live', 'ready' or 'production'
  } {
    ::xo::db::sql::content_item set_live_revision \
        -revision_id $revision_id \
        -publish_status $publish_status \
        -is_latest $is_latest
    ::xo::clusterwide ns_cache flush xotcl_object_cache ::[my item_id]
    ::xo::clusterwide ns_cache flush xotcl_object_cache ::$revision_id
  }

  CrItem ad_instproc update_item_index {} {
    Dummy stub to allow subclasses to produce an more efficient
    index for items based on live revisions.
  } {
    next
  }
  
  CrItem ad_instproc save_new {
    -package_id 
    -creation_user 
    -creation_ip 
    {-live_p:boolean true}
    {-use_given_publish_date:boolean false}
  } {
    Insert a new item to the content repository
    @param package_id
    @param creation_user user_id if the creating user
    @param live_p make this revision the live revision
  } {
    
    set __class [my info class]
    my instvar parent_id item_id import_file name
    
    if {![info exists package_id] && [my exists package_id]} {
      set package_id [my package_id]
    }
    [self class] get_context package_id creation_user creation_ip
    my set creation_user $creation_user
    set __atts  [list creation_user]
    set __vars $__atts

    #my log "db_slots for $__class: [$__class array get db_slot]"
    foreach {__slot_name __slot} [$__class array get db_slot] {
      #my log "--slot = $__slot"
      if {
          $__slot eq "::xo::db::Object::slot::object_title" ||
          $__slot eq "::xo::db::CrItem::slot::name" ||
          $__slot eq "::xo::db::CrItem::slot::publish_date"
        } continue
      my instvar $__slot_name
      if {![info exists $__slot_name]} {set $__slot_name ""}
      lappend __atts [$__slot column_name]
      lappend __vars $__slot_name
    }
    
    if {$use_given_publish_date} {
      if {"publish_date" ni $__atts} {
        my instvar publish_date
        lappend __atts publish_date
        lappend __vars publish_date
      }
      set publish_date_flag [list -publish_date $publish_date]
    } else {
      set publish_date_flag ""
    }

    ::xo::dc transaction {
      $__class instvar storage_type object_type
      [self class] lock acs_objects "SHARE ROW EXCLUSIVE"
      set revision_id [xo::dc nextval acs_object_id_seq]
      my set revision_id $revision_id

      if {![my exists name] || $name eq ""} {
        # we have an autonamed item, use a unique value for the name
        set name [expr {[my exists __autoname_prefix] ? 
                        "[my set __autoname_prefix]$revision_id" : $revision_id}]
      }
      if {$title eq ""} {
        set title [expr {[my exists __title_prefix] ? 
                         "[my set __title_prefix] ($name)" : $name}]
      }

      if {$storage_type eq "file"} {
        # Get the mime_type from the file, eventually creating a new
        # one if it's unrecognized.
        set mime_type [cr_check_mime_type \
                           -mime_type $mime_type \
                           -filename  $name \
                           -file      $import_file]
      }
      
      set item_id [::xo::db::sql::content_item new \
                       -name            $name \
                       -parent_id       $parent_id \
                       -creation_user   $creation_user \
                       -creation_ip     $creation_ip \
                       -item_subtype    "content_item" \
                       -content_type    $object_type \
                       -description     $description \
                       -mime_type       $mime_type \
                       -nls_language    $nls_language \
                       -is_live         f \
                       -storage_type    $storage_type \
                       -package_id      $package_id \
                       -with_child_rels f]
      
      if {$storage_type eq "file"} {
        set text [cr_create_content_file $item_id $revision_id $import_file]
      }

      ::xo::dc [::xo::dc insert-view-operation] revision_add \
          [[my info class] insert_statement $__atts $__vars]
      my fix_content $revision_id $text

      if {$live_p} {
        #
        # Update the life revision with the publish status and
        # optionally the publish_date
        #
        ::xo::db::sql::content_item set_live_revision \
            -revision_id $revision_id \
            -publish_status [my set publish_status] \
            -is_latest true \
            {*}$publish_date_flag
        my update_item_index
      }
    }

    my db_1row [my qn get_dates] {
      select creation_date, last_modified 
      from acs_objects where object_id = :revision_id
    }
    my set object_id $item_id
    return $item_id
  }

  CrItem ad_instproc delete {} {
    Delete the item from the content repositiory with the item_id taken from the 
    instance variable.
  } {
    # delegate deletion to the class
    [my info class] delete -item_id [my set item_id]
  }

  CrItem ad_instproc rename {-old_name:required -new_name:required} {
    Rename a content item 
  } {
    my instvar item_id
    ::xo::dc dml update_rename \
        "update cr_items set name = :new_name where item_id = :item_id"
    my set name $new_name
    my update_item_index
  }

  #
  # The method "changed_redirect_url" is a helper method for old-style
  # wiki pages, still using ad_form. Form.edit_data calls this method
  # after a rename operation to optionally redirect the browser after
  # the edit operation to the new url, unless an explicit return_url
  # was specified.
  #
  CrItem instproc changed_redirect_url {} {
    return ""
  }

  CrItem instproc www-revisions {} {

    set isAdmin [acs_user::site_wide_admin_p]

    ::TableWidget create t1 -volatile \
        -columns {
          Field version_number -label "" -html {align right}
          ImageAnchorField edit -label "" -src /resources/acs-subsite/Zoom16.gif \
              -title "View Item" -alt  "view" \
              -width 16 -height 16 -border 0
          AnchorField diff -label ""
          AnchorField author -label [_ file-storage.Author]
          Field content_size -label [_ file-storage.Size] -html {align right}
          Field last_modified_ansi -label [_ file-storage.Last_Modified]
          Field description -label [_ file-storage.Version_Notes] 
          if {[acs_user::site_wide_admin_p]} {AnchorField show -label ""}
          ImageAnchorField live_revision -label [_ xotcl-core.live_revision] \
              -src /resources/acs-subsite/radio.gif \
              -width 16 -height 16 -border 0 -html {align center}
          ImageField_DeleteIcon version_delete -label "" -html {align center}
        }

    set user_id [my current_user_id]
    set page_id [my set item_id]
    set live_revision_id [::xo::db::sql::content_item get_live_revision -item_id $page_id]
    my instvar package_id
    set base [$package_id url]
    set sql [::xo::dc select \
                 -map_function_names true \
                 -vars "ci.name, r.revision_id as version_id,\
                        person__name(o.creation_user) as author, \
                        o.creation_user as author_id, \
                        to_char(o.last_modified,'YYYY-MM-DD HH24:MI:SS') as last_modified_ansi,\
                        r.description,\
                        acs_permission__permission_p(r.revision_id,:user_id,'admin') as admin_p,\
                        acs_permission__permission_p(r.revision_id,:user_id,'delete') as delete_p,\
                        r.content_length,\
                        content_revision__get_number(r.revision_id) as version_number " \
                 -from  "cr_items ci, cr_revisions r, acs_objects o" \
                 -where "ci.item_id = :page_id and r.item_id = ci.item_id and o.object_id = r.revision_id 
                         and acs_permission__permission_p(r.revision_id, :user_id, 'read')" \
                 -orderby "r.revision_id desc"]
    
    ::xo::dc foreach revisions_select $sql {
      if {$content_length < 1024} {
        if {$content_length eq ""} {set content_length 0}
        set content_size_pretty "[lc_numeric $content_length] [_ file-storage.bytes]"
      } else {
        set content_size_pretty "[lc_numeric [format %.2f [expr {$content_length/1024.0}]]] [_ file-storage.kb]"
      }
      
      set last_modified_ansi [lc_time_system_to_conn $last_modified_ansi]
      
      if {$version_id != $live_revision_id} {
        set live_revision "Make this Revision Current"
        set live_revision_icon /resources/acs-subsite/radio.gif
      } else {
        set live_revision "Current Live Revision"
        set live_revision_icon /resources/acs-subsite/radiochecked.gif
      }
      
      set live_revision_link [export_vars -base $base \
                                  {{m make-live-revision} {revision_id $version_id}}]

      t1 add \
          -version_number $version_number: \
          -edit.href [export_vars -base $base {{revision_id $version_id}}] \
          -author $author \
          -content_size $content_size_pretty \
          -last_modified_ansi [lc_time_fmt $last_modified_ansi "%x %X"] \
          -description $description \
          -live_revision.src $live_revision_icon \
          -live_revision.title $live_revision \
          -live_revision.href $live_revision_link \
          -version_delete.href [export_vars -base $base \
                                    {{m delete-revision} {revision_id $version_id}}] \
          -version_delete.title [_ file-storage.Delete_Version]
      
      [t1 last_child] set payload(revision_id) $version_id

      if {$isAdmin} {
        set show_revision_link [export_vars -base $base \
                                    {{m show-object} {revision_id $version_id}}]
        [t1 last_child] set show show
        [t1 last_child] set show.href $show_revision_link
      }

    }
    
    # providing diff links to the prevision versions. This can't be done in
    # the first loop, since we have not yet the revision id of entry in the next line.
    set lines [t1 children]
    for {set i 0} {$i < [llength $lines]-1} {incr i} {
      set e [lindex $lines $i]
      set n [lindex $lines $i+1]
      set revision_id [$e set payload(revision_id)]
      set compare_revision_id [$n set payload(revision_id)]
      $e set diff.href [export_vars -base $base {{m diff} compare_revision_id revision_id}]
      $e set diff "diff"
    }
    set e [lindex $lines end]
    if {$e ne ""} {
      $e set diff.href ""
      $e set diff ""
    }

    return [t1 asHTML]
  }


  #
  # Object specific privilege to be used with policies
  #

  CrItem ad_instproc privilege=creator {
    {-login true} user_id package_id method
  } {

    Define an object specific privilege to be used in the policies.
    Grant access to a content item for the creator (creation_user)
    of the item, and for the package admin.

  } {
    set allowed 0
    #my log "--checking privilege [self args]"
    if {[my exists creation_user]} {
      if {[my set creation_user] == $user_id} {
        set allowed 1
      } else {
        # allow the package admin always access
        set allowed [::xo::cc permission \
                         -object_id $package_id \
                         -party_id $user_id \
                         -privilege admin]
      }
    }
    return $allowed
  }

  ::xo::db::CrClass create ::xo::db::image -superclass ::xo::db::CrItem \
      -pretty_name "Image" \
      -table_name "images" -id_column "image_id" \
      -object_type image \
      -slots {
        ::xo::db::CrAttribute create width  -datatype integer
        ::xo::db::CrAttribute create height -datatype integer
      }

  #
  # CrFolder
  #
  ::xo::db::CrClass create ::xo::db::CrFolder \
      -superclass ::xo::db::CrItem  \
      -pretty_name "Folder" -pretty_plural "Folders" \
      -table_name "cr_folders" -id_column "folder_id" \
      -object_type content_folder \
      -form CrFolderForm \
      -edit_form CrFolderForm \
      -slots {
        ::xo::db::CrAttribute create folder_id -datatype integer -pretty_name "Folder ID" \
            -references "cr_items on delete cascade"
        ::xo::db::CrAttribute create label -datatype text -pretty_name "Label"
        ::xo::db::CrAttribute create description \
            -datatype text -pretty_name "Description" -spec "textarea,cols=80,rows=2"
        # the package_id in folders is deprecated, the one in acs_objects should be used
      } \
      \
      -ad_doc {
        This is a generic class that represents a "cr_folder"
        XoWiki specific methods are currently directly mixed
        into all instances of this class.

        @see ::xowiki::Folder
      }

  # TODO: the following block should not be necessary We should get
  # rid of the old "folder object" in xowiki and use parameter pages
  # instead. The primary usage of the xowiki folder object is for
  #
  #  a) specifying richt-text properties for an instance
  #  b) provide a title for the instance
  # 
  # We should provide either a minimal parameter page for this
  # purposes, or - more conservative - provide simply package
  # parameters for this. The only thing we are losing are "computed
  # parameters", what most probably no-one uses. The delegation based
  # parameters are most probably good replacement to manage such
  # parameters site-wide.

  ::xo::db::CrFolder ad_proc instance_select_query {
    {-select_attributes ""}
    {-orderby ""}
    {-where_clause ""}
    {-from_clause ""}
    {-with_subtypes:boolean true}
    {-with_children:boolean true}
    {-publish_status}
    {-count:boolean false}
    {-folder_id}
    {-parent_id}
    {-page_size 20}
    {-page_number ""}
    {-base_table "cr_folders"}
  } {
    returns the SQL-query to select the CrItems of the specified object_type
    @param select_attributes attributes for the sql query to be retrieved, in addition
    to item_id, name, publish_status, object_type which are always returned
    @param orderby for ordering the solution set
    @param where_clause clause for restricting the answer set
    @param with_subtypes return subtypes as well
    @param with_children return immediate child objects of all objects as well
    @param count return the query for counting the solutions
    @param folder_id parent_id
    @param publish_status one of 'live', 'ready', or 'production'
    @param base_table typically automatic view, must contain title and revision_id
    @return sql query
  } {
    if {![info exists folder_id]} {my instvar folder_id}
    if {![info exists parent_id]} {set parent_id $folder_id}

    if {$base_table eq "cr_folders"} {
      set attributes [list ci.item_id ci.name ci.publish_status acs_objects.object_type] 
    } else {
      set attributes [list bt.item_id ci.name ci.publish_status bt.object_type] 
    }
    foreach a $select_attributes {
      # if {$a eq "title"} {set a bt.title}
      lappend attributes $a
    }
    # FIXME: This is dirty: We "fake" the base table for this function, so we can reuse the code
    set type_selection_clause [my type_selection_clause -base_table cr_revisions -with_subtypes false]
    #my log "type_selection_clause -with_subtypes $with_subtypes returns $type_selection_clause"
    if {$count} {
      set attribute_selection "count(*)"
      set orderby ""      ;# no need to order when we count
      set page_number  ""      ;# no pagination when count is used
    } else {
      set attribute_selection [join $attributes ,]
    }

    set cond [list]
    if {$type_selection_clause ne ""} {lappend cond $type_selection_clause}
    if {$where_clause ne ""}          {lappend cond $where_clause}
    if {[info exists publish_status]} {lappend cond "ci.publish_status = :publish_status"}
    if {$base_table eq "cr_folders"} {
      lappend cond "acs_objects.object_id = cf.folder_id and ci.item_id = cf.folder_id"
      set acs_objects_table "acs_objects, cr_items ci, "
    } else {
      lappend cond "ci.item_id = bt.item_id"
      set acs_objects_table ""
    }
    if {$parent_id ne ""} {
      set parent_clause "ci.parent_id = :parent_id"
      if {$with_children} {
        lappend cond "ci.item_id in (
                select children.item_id from cr_items parent, cr_items children
                where children.tree_sortkey between parent.tree_sortkey and tree_right(parent.tree_sortkey)
                and parent.item_id = $parent_id and parent.tree_sortkey <> children.tree_sortkey)"
      } else {
        lappend cond $parent_clause
      }
    }

    if {$page_number ne ""} {
      set limit $page_size
      set offset [expr {$page_size*($page_number-1)}]
    } else {
      set limit ""
      set offset ""
    }

    set sql [::xo::dc select \
                 -vars $attribute_selection \
                 -from "$acs_objects_table cr_folders cf $from_clause" \
                 -where [join $cond " and "] \
                 -orderby $orderby \
                 -limit $limit -offset $offset]
    return $sql
  }

  ::xo::db::CrFolder ad_proc get_instance_from_db {
    {-item_id 0}
    {-revision_id 0}
    {-initialize:boolean true}
  } {
    The "standard" get_instance_from_db methods return objects following the
    naming convention "::<acs_object_id>", e.g. ::1234

    Usually, the id of the item that is fetched from the database is used. However,
    XoWiki's "folder objects" (i.e. an ::xowiki::Object instance that can be used
                               to configure the respective instance) are created using the acs_object_id of the
    root folder of the xowiki instance, which is actually the id of another acs_object.

    Because of this, we cannot simply create the instances of CrFolder using the
    "standard naming convention". Instead we create them as ::cr_folder<acs_object_id>
  } {
    set object ::cr_folder$item_id
    if {![my isobject $object]} {
      my fetch_object -object $object -item_id $item_id -initialize $initialize
      $object destroy_on_cleanup
    }
    return $object
  }

  ::xo::db::CrFolder ad_proc register_content_types {
    {-folder_id:required}
    {-content_types ""}
  } {
    Register the specified content types for the folder.
    If a content_type ends with a *, include its subtypes
  } {
    foreach content_type $content_types {
      set with_subtypes [expr {[regexp {^(.*)[*]$} $content_type _ content_type] ? "t" : "f"}]
      ::xo::db::sql::content_folder register_content_type \
          -folder_id $folder_id \
          -content_type $content_type \
          -include_subtypes $with_subtypes
    }
  }

  ::xo::db::CrFolder ad_proc fetch_object {
    -item_id:required
    {-revision_id 0}
    -object:required
    {-initialize:boolean true}
  } {
    We overwrite the default fetch_object method here.
    We join acs_objects, cr_items and cr_folders and fetch
    all attributes. The revision_id is completely ignored.
    @see CrClass fetch_object
  } {
    if {![::xotcl::Object isobject $object]} {
      my create $object
    }
    
    $object db_1row [my qn fetch_folder] "
        SELECT * FROM cr_folders
        JOIN cr_items on cr_folders.folder_id = cr_items.item_id
        JOIN acs_objects on cr_folders.folder_id = acs_objects.object_id
        WHERE folder_id = $item_id"

    if {$initialize} {$object initialize_loaded_object}
    return $object
  }

  ::xo::db::CrFolder ad_instproc save_new {-creation_user} {
  } {
    my instvar parent_id package_id folder_id
    [my info class] get_context package_id creation_user creation_ip
    set folder_id [::xo::db::sql::content_folder new \
                       -name [my name] -label [my label] \
                       -description [my description] \
                       -parent_id $parent_id \
                       -package_id $package_id \
                       -creation_user $creation_user \
                       -creation_ip $creation_ip]
    #parent_s has_child_folders attribute could have become outdated
    if { [my isobject ::$parent_id] } {
      ::$parent_id set has_child_folders t
    }
    # well, obtaining the allowed content_types this way is not very
    # straightforward, but since we currently create these folders via
    # ad_forms, and we have no form variable, this should be at least
    # robust.
    if {[[self class] exists allowed_content_types]} {
      ::xo::db::CrFolder register_content_types \
          -folder_id $folder_id \
          -content_types [[self class] set allowed_content_types]
    }
    ::xo::clusterwide ns_cache flush xotcl_object_cache ::$parent_id
    # who is setting sub_folder_list?
    #db_flush_cache -cache_key_pattern sub_folder_list_*
    return $folder_id
  }

  ::xo::db::CrFolder ad_instproc save {args} { }  {
    my instvar folder_id
    content::folder::update \
        -folder_id $folder_id \
        -attributes [list \
                         [list name [my set name]] \
                         [list label [my set label]] \
                         [list description [my set description]]\
                        ]
    my get_context package_id user_id ip
    ::xo::dc 1row _ "select acs_object__update_last_modified(:folder_id,$user,'$ip')"
  }

  ::xo::db::CrFolder instproc is_package_root_folder {} {
    my instvar package_id folder_id
    return [expr {$folder_id eq [::$package_id folder_id]} ? true : false]
  }
  
  ::xo::db::CrFolder instproc delete {} {
    my instvar package_id name parent_id folder_id
    if {[my is_package_root_folder]} {
      ad_return_error "Removal denied" "Dont delete the package root folder, delete the package"
      return
    }
    ::xo::db::sql::content_folder del -folder_id $folder_id -cascade_p t
  }
  

  #
  # Caching interface
  #
  # CrClass is a mixin class for caching the CrItems in ns_cache.
  #
  
  ::xotcl::Class create CrCache 
  CrCache instproc fetch_object {
    -item_id:required
    {-revision_id 0}
    -object:required
    {-initialize:boolean true}
  } {
    set serialized_object [ns_cache eval xotcl_object_cache $object {
      #my log "--CACHE true fetch [self args], call shadowed method [self next]" 
      set loaded_from_db 1
      # Call the showdowed method with initializing turned off. We
      # want to store object before the after-load initialize in the
      # cache to save storage.
      set o [next -item_id $item_id -revision_id $revision_id -object $object -initialize 0]
      return [::Serializer deepSerialize $o]
    }]
    #my log "--CACHE: [self args], created [info exists created] o [info exists o]"
    if {[info exists loaded_from_db]} {
      # The basic fetch_object method creates the object, we have
      # just to run the after load init (if wanted)
      if {$initialize} {$object initialize_loaded_object}
    } else {
      # The variable serialized_object contains the serialization of
      # the object from the cache; check if the object exists already
      # or create it.
      if {[my isobject $object]} {
        # There would have been no need to call this method. We could
        # raise an error here.  
        # my log "--!! $object exists already"
      } else {
        # Create the object from the serialization and initialize it
        eval $serialized_object
        if {$initialize} {$object initialize_loaded_object}
      }
    }
    return $object
  }

  CrCache instproc delete {-item_id} {
    next
    ::xo::clusterwide ns_cache flush xotcl_object_cache ::$item_id
    # we should probably flush as well cached revisions
  }

  ::xotcl::Class create CrCache::Class
  CrCache::Class instproc lookup {
    -name:required
    {-parent_id -100}
  } {
    # We need here the strange logic to avoid caching of lookup fails.
    # In order to cache fails as well, we would have to flush the fail
    # on new added items and renames.
    while {1} {
      set item_id [ns_cache eval xotcl_object_type_cache $parent_id-$name {
        set item_id [next]
        if {$item_id == 0} break ;# don't cache
        return $item_id
      }]
      
      break
    }
    #my msg "lookup $parent_id-$name -> item_id=$item_id"
    return $item_id
  }

  ::xotcl::Class create CrCache::Item
  CrCache::Item set name_pattern {^::[0-9]+$}
  
  CrCache::Item instproc remove_non_persistent_vars {} {
    #
    # Do not save __db__artefacts in the cache.
    #
    foreach x [info vars :__db_*] {
      unset :$x
    }
    #
    # Remove vars and arrays matching the class-specific specified
    # non_cached_instance_var_patterns and treat these as variables,
    # which are not stored in the cache, but which are kept in the
    # instance variables. These variables are removed before caching
    # and restored afterwards.
    #
    set arrays {}
    set scalars {}
    set non_cached_vars {}
    foreach pattern [[my info class] non_cached_instance_var_patterns] {
      lappend non_cached_vars {*}[info vars :$pattern]
    }
    
    #puts stderr "pattern [[my info class] non_cached_instance_var_patterns], non_cached_vars=$non_cached_vars"
    foreach x $non_cached_vars {
      if {[array exists :$x]} {
        lappend arrays $x [array get :$x]
        array unset :$x
      } {
        lappend scalars $x [set :$x]
        unset :$x
      }
    }
    return [list $arrays $scalars]
  }
  
  CrCache::Item instproc set_non_persistent_vars {vars} {
    lassign $vars arrays scalars
    foreach {var value} $arrays {my array set $var $value}
    :mset $scalars
  }
  CrCache::Item instproc flush_from_cache_and_refresh {} {
    # cache only names with IDs
    set obj [self]
    set canonical_name ::[$obj item_id]
    ::xo::clusterwide ns_cache flush xotcl_object_cache $obj
    if {$obj eq $canonical_name} {
      #my log "--CACHE saving $obj in cache"
      #
      # The object name is eq to the item_id; we assume, this is a
      # fully loaded object, containing all relevant instance
      # variables. We can restore it. after the flash
      # 
      # We do not want to cache per object mixins for the
      # time being (some classes might be volatile). So save
      # mixin-list, cache and resore them later for the current
      # session.
      set mixins [$obj info mixin]
      $obj mixin [list]
      set npv [$obj remove_non_persistent_vars]
      ns_cache set xotcl_object_cache $obj [$obj serialize]
      $obj set_non_persistent_vars $npv
      $obj mixin $mixins
    } else {
      # in any case, flush the canonical name
      ::xo::clusterwide ns_cache flush xotcl_object_cache $canonical_name
    }
    # To be on he safe side, delete the revison as well from the
    # cache, if possible.
    if {[$obj exists revision_id]} {
      set revision_name ::[$obj revision_id]
      if {$obj ne $revision_name} {
        ::xo::clusterwide ns_cache flush xotcl_object_cache $revision_name
      }
    }
  }
  CrCache::Item instproc update_attribute_from_slot args {
    set r [next]
    my flush_from_cache_and_refresh
    return $r
  }
  CrCache::Item instproc save args {
    # we perform next before the cache update, since when update fails, we do not
    # want to populate wrong content in the cache
    set r [next]
    my flush_from_cache_and_refresh
    return $r
  }
  CrCache::Item instproc save_new args {
    set item_id [next]
    # the following approach will now work nicely, we would have to rename the object
    # caching this does not seem important here, the next fetch will cache it anyhow
    #ns_cache set xotcl_object_cache $item_id [::Serializer deepSerialize [self]]
    return $item_id
  }
  CrCache::Item instproc delete args {
    ::xo::clusterwide ns_cache flush xotcl_object_cache [self]
    #my msg "delete flush xotcl_object_type_cache [my parent_id]-[my name]"
    ::xo::clusterwide ns_cache flush xotcl_object_type_cache [my parent_id]-[my name]
    next
  }
  CrCache::Item instproc rename {-old_name:required -new_name:required} {
    #my msg "rename flush xotcl_object_type_cache [my parent_id]-$old_name"
    ::xo::clusterwide ns_cache flush xotcl_object_type_cache [my parent_id]-$old_name
    next
  }
  
  CrClass instmixin CrCache
  CrClass mixin CrCache::Class
  CrItem instmixin CrCache::Item
}  

#::xo::library source_dependent 


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
