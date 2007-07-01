ad_library {
  XOTcl API simple Content repository apps, supports categories.

  @author Gustaf Neumann
  @creation-date 2005-08-13
  @cvs-id $Id$
}

namespace eval ::Generic {

  Class CrClass -superclass Class -parameter {
    pretty_name
    pretty_plural
    {supertype content_revision}
    table_name
    id_column
    {cr_attributes {}}
    {sql_attribute_names {}}
    form
    edit_form
    {name_method ""}
    {description " "}
    {mime_type text/plain}
    {nls_language ""}
    {text " "}
    {storage_type "text"}
    {folder_id -100}
    {object_type [self]}
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
    <a href='proc-view?proc=%3a%3aGeneric%3a%3aCrItem+proc+instantiate'>
    CrItem instantiate</a> or through the "instantiate" method of 
    every subclass of CrItem.

    <p>This Class is a meta-class providing methods for Classes 
    manageing CrItems.</p>
  }

  proc package_id_from_package_key { key } {
    return [db_string get_package_id_from_key \
                {select package_id from apm_packages where package_key = :key}]
  }

  CrClass instproc unknown { obj args } {
    my log "unknown called with $obj $args"
  }

  #
  # The following methods are used oracle, postgres specific code (locking,
  # for the type hierarchies, ...
  #
  CrClass instproc lock {tablename mode} {
    # no locking by default
  }
  if {[db_driverkey ""] eq "postgresql"} {
    #
    # Postgres
    #
    CrClass instproc object_types_query {
      {-subtypes_first:boolean false}
    } {
      my instvar object_type_key
      set order_clause [expr {$subtypes_first ? "order by tree_sortkey desc":""}]
      return "select object_type from acs_object_types where 
        tree_sortkey between '$object_type_key' and tree_right('$object_type_key')
        $order_clause"
    }
    CrClass instproc init_type_hierarchy {} {
      my instvar object_type
      my set object_type_key [db_list [my qn get_tree_sortkey] {
        select tree_sortkey from acs_object_types 
        where object_type = :object_type
      }]
    }
    CrClass instproc type_selection {-with_subtypes:boolean} {
      my instvar object_type_key object_type
      if {$with_subtypes} {
        #return "acs_object_types.tree_sortkey between '$object_type_key' and tree_right('$object_type_key')"
        #return "ci.content_type in ('[join [my object_types] ',']')"
        return "ci.content_type in ([my object_types_query])"
      } else {
        return "ci.content_type = '$object_type'"
        #return "acs_object_types.tree_sortkey = '$object_type_key'"
      }
    }
    set pg_version [db_string qn.null.get_version {
      select substring(version() from 'PostgreSQL #"[0-9]+.[0-9+]#".%' for '#')   }]
    ns_log notice "--Postgres Version $pg_version"      
    if {$pg_version < 8.2} {
      ns_log notice "--Postgres Version $pg_version older than 8.2, use locks"      
      CrClass instproc lock {tablename mode} {
        db_dml [my qn lock_objects] "LOCK TABLE $tablename IN $mode MODE"
      }
    }
  } else {
    #
    # Oracle
    #
    CrClass instproc object_types_query {
      {-subtypes_first:boolean false}
    } {
      my instvar object_type
      set order_clause [expr {$subtypes_first ? "order by LEVEL desc":""}]
      return "select object_type from acs_object_types 
        start with object_type = '$object_type' 
        connect by prior object_type = supertype $order_clause"
    }
    CrClass instproc init_type_hierarchy {} {
      my set object_type_key {}
    }
    CrClass instproc type_selection {-with_subtypes:boolean} {
      my instvar object_type
      if {$with_subtypes} {
        return "acs_objects.object_type in ([my object_types_query])"
      } else {
        return "acs_objects.object_type = '$object_type'"
      }
    }
  }
  
  CrClass set common_query_atts {
    object_type item_id revision_id 
    creation_user creation_date creation_user 
    publish_status last_modified 
  }
  if {[apm_version_names_compare [ad_acs_version] 5.2] > -1} {
     CrClass lappend common_query_atts package_id
  }

  CrClass set common_insert_atts {name title description mime_type nls_language text}

   CrClass instproc edit_atts {} {
    concat [[self class] set common_insert_atts] [my sql_attribute_names]
  }

  CrClass instproc object_type_exists {} {
    my instvar object_type
    expr {$object_type eq [db_list [my qn select_type] {
      select object_type from acs_object_types where 
      object_type = :object_type
    }]}
  }

  CrClass ad_instproc folder_type_unregister_all {
    {-include_subtypes t}
  } {
    Unregister the object type from all folders on the system

    @param include_subtypes Boolean value (t/f) to flag whether the 
    operation should be applied on subtypes as well
  } {
    my instvar object_type
    db_foreach all_folders { 
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

  CrClass instproc create_attributes {} {
    if {[my cr_attributes] ne ""} {
      my instvar object_type
      set slot [self]::slot
      if {[info command $slot] eq ""} {
        ::xotcl::Object create $slot
      }
      set o [::xo::OrderedComposite new -contains [my cr_attributes]]
      $o destroy_on_cleanup

      foreach att [$o children] {
	$att instvar attribute_name datatype pretty_name sqltype default
        # provide a default pretty name for the attribute based on message keys
        if {![info exists pretty_name]} {
          set pretty_name "#xowiki.[namespace tail [self]]-$attribute_name#"
        }

        set column_spec [::xo::db::sql map_datatype $sqltype] 
        #my log "--SQL $attribute_name datatype=$datatype, sqltype=$sqltype, column_spec=$column_spec"
        if {[info exists default]} {append column_spec " default '$default'" }
        append column_spec " " \
            [::xo::db::sql datatype_constraint $sqltype [my table_name] $attribute_name]

	if {![attribute::exists_p $object_type $attribute_name]} {
	  ::xo::db::sql::content_type create_attribute \
	      -content_type $object_type \
	      -attribute_name $attribute_name \
	      -datatype $datatype \
	      -pretty_name $pretty_name \
	      -column_spec [string trim $column_spec]
	}
	#if {![info exists default]} {
	#  set default ""
	#}
	#lappend parameters [list $attribute_name $default]
	#unset default
      }
      #my log "--parameter [self] parameter [list $parameters]"
      #my parameter $parameters

      # TODO the following will not be needed, when we enforce xotcl 1.5.0+
      set parameters [list]
      foreach att [$o children] {
	$att instvar attribute_name datatype pretty_name sqltype default help_text spec validator
        set slot_obj [self]::slot::$attribute_name
        #my log "--cr ::xo::Attribute create $slot_obj"
        ::xo::Attribute create $slot_obj
	if {![info exists default]} {
	  set default ""
	}
	if {[info exists help_text]} {$slot_obj help_text $help_text}
	if {[info exists validator]} {$slot_obj validator $validator}
	if {[info exists spec]} {$slot_obj spec $spec}
        $slot_obj datatype $datatype
        $slot_obj pretty_name $pretty_name
	$slot_obj default $default 
        $slot_obj sqltype $sqltype
	lappend parameters [list $attribute_name $default]
	unset default
      }
      if {$::xotcl::version < 1.5} {
        my parameter $parameters
      }
    }
  }

  CrClass ad_instproc create_object_type {} {
    Create an oacs object_type and a table for keeping the
    additional attributes.
  } {
    my instvar object_type supertype pretty_name pretty_plural \
        table_name id_column name_method

    set supertype [my info superclass]
    switch -- $supertype {
      ::xotcl::Object -
      ::Generic::CrItem {set supertype content_revision}
    }

    db_transaction {
      ::xo::db::sql::content_type create_type \
          -content_type $object_type \
          -supertype $supertype \
          -pretty_name $pretty_name \
          -pretty_plural $pretty_plural \
          -table_name $table_name \
          -id_column $id_column \
          -name_method $name_method
      
      my create_attributes
      my folder_type register
    }
  }



  CrClass ad_instproc drop_object_type {} {
    Delete the object type and remove the table for the attributes.
    This method should be called when all instances are deleted. It
    undoes everying what create_object_type has produced.
  } {
    my instvar object_type table_name
    db_transaction {
      my folder_type unregister
      ::xo::db::sql::content_type drop_type \
          -content_type $object_type \
          -drop_children_p t \
          -drop_table_p t
    }
  }

  CrClass ad_instproc require_folder {
    {-parent_id -100} 
    {-content_types content_revision}
    -package_id 
    -name
  } {
    Get folder_id for a community id or the actual package.
    If everything fails, return -100

    @return folder_id
  } {
    my instvar object_type table_name
    if {[info exists package_id]} {
      set cid $package_id
    } else {
      if {[my isobject ::xo::cc]} {
        set package_id [::xo::cc package_id]
        set url [::xo::cc url]
      } elseif {[ad_conn isconnected]} {
        set package_id [ad_conn package_id]
        set url [ad_conn url]
      }

      if {[info exists package_id]} {
        set cid ""
        if {[info command dotlrn_community::get_community_id_from_url] ne ""} {
          set cid [dotlrn_community::get_community_id_from_url -url $url]
        }
        if {$cid eq ""} {
          set cid $package_id
        }
      } else {
        error "Could not determine package id or community id"
      }
    }
    set folder_id [ns_cache eval xotcl_object_type_cache root_folder-$cid {
      set folder_name "$name: $cid"
      
      if {[info command content::item::get_id_by_name] eq ""} {
        set folder_id ""
        db_0or1row [my qn get_id_by_name] "select item_id as folder_id from cr_items \
         where name = :folder_name and parent_id = :parent_id"
      } else {
        set folder_id [content::item::get_id_by_name \
                           -name $folder_name -parent_id $parent_id]
      }
      if {$folder_id eq ""} {
        set folder_id [content::folder::new \
                           -name $folder_name \
                           -parent_id $parent_id \
                           -package_id $package_id -context_id $cid]
      }
      # register all specified content types
      foreach content_type $content_types {
	# if a content_type ends with a *, include subtypes
	set with_subtypes [expr {[regexp {^(.*)[*]$} $content_type _ content_type] ? "t" : "f"}]
	::xo::db::sql::content_folder register_content_type \
	    -folder_id $folder_id \
	    -content_type $content_type \
	    -include_subtypes $with_subtypes
      }
      return $folder_id
    }]
    
    return $folder_id
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


  CrClass instproc init {} {
    my instvar object_type sql_attribute_names
    if {[my info superclass] ne "::Generic::CrItem"} {
      my set superclass [[my info superclass] set object_type]
    }
    my init_type_hierarchy
    set sql_attribute_names [list]
    set o [::xo::OrderedComposite new -contains [my cr_attributes]]
    $o destroy_on_cleanup
    foreach att [$o children] { 
      lappend sql_attribute_names [$att attribute_name]
    }
    set sc [my info superclass]
    if {[$sc exists sql_attribute_names]} {
      # my log "-- inherited attribute_names <[$sc set sql_attribute_names]>"
      foreach n [$sc set sql_attribute_names] {lappend sql_attribute_names $n}
    }
    #my log "-- attribute_names <$sql_attribute_names> [$o info children]"

    if {![my object_type_exists]} {
      my create_object_type
    } else {
      db_transaction {
	my create_attributes
      }
    }
 
    next
  }
  
  CrClass ad_instproc lookup {
    -name:required
    -parent_id:required
  } {
    Check, whether an content item with the given title exists.
    If not, return 0.

    @return item_id
  } {
    if {[db_0or1row [my qn entry_exists_select] "\
       select item_id from cr_items where name = :name and parent_id = :parent_id"]} {
      return $item_id
    }
    return 0
  }


  CrClass ad_instproc fetch_object {
    -item_id:required
    {-revision_id 0}
    -object:required
  } {
    Load a content item into the specified object. If revision_id is
    provided, the specified revision is returned, otherwise the live
    revision of the item_id. If the object does not exist, we create it.

    @return cr item object
  } {
    #my log "-- [self args]"
    if {![::xotcl::Object isobject $object]} {
      # if the object does not yet exist, we have to create it
      my create $object
    }
    set raw_atts [concat [[self class] set common_query_atts] [my edit_atts]]
    set atts [list]
    foreach v $raw_atts {
      switch -- $v {
        name           {set fq i.$v}
        publish_status {set fq i.$v}
        creation_date  {set fq o.$v}
        package_id     {set fq o.$v}
        text           {set fq "n.data as text"}
        default        {set fq n.$v}
      }
      lappend atts $fq
    }
    if {$revision_id} {
      $object db_1row [my qn fetch_from_view_revision_id] "\
       select [join $atts ,], i.parent_id \
       from   [my set table_name]i n, cr_items i,acs_objects o \
       where  n.revision_id = $revision_id \
       and    i.item_id = n.item_id \
       and    o.object_id = $revision_id"
    } else {
      $object db_1row [my qn fetch_from_view_item_id] "\
       select [join $atts ,], i.parent_id \
       from   [my set table_name]i n, cr_items i, acs_objects o \
       where  i.item_id = $item_id \
       and    n.[my id_column] = coalesce(i.live_revision, i.latest_revision) \
       and    o.object_id = i.item_id"
    }

    if {[apm_version_names_compare [ad_acs_version] 5.2] <= -1} {
      $object set package_id [db_string [my qn get_pid] \
                   "select package_id from cr_folders where folder_id = [$object set parent_id]"]
    }

    #my log "--AFTER FETCH\n[$object serialize]"
    $object initialize_loaded_object
    return $object
  }


  CrClass ad_instproc instantiate {
    -item_id
    {-revision_id 0}
  } { 
    Retrieve either the live revision or a specified revision
    of a content item with all attributes into a newly created object.
    The retrieved attributes are strored in the instance variables in
    class representing the object_type.

    @param item_id id of the item to be retrieved.
    @param revision_id revision-id of the item to be retrieved.
  } {
    set object ::[expr {$revision_id ? $revision_id : $item_id}]
    if {![my isobject $object]} {
      my fetch_object -object $object \
          -item_id $item_id -revision_id $revision_id
    }
    return $object
  }

  CrClass ad_instproc delete {
    -item_id:required
  } { 
    Delete a content item from the content repository.
    @param item_id id of the item to be deleted
  } {
    ::xo::db::sql::content_item del -item_id $item_id
  }

  CrClass instproc object_types {
    {-subtypes_first:boolean false}
   } {
     return [db_list [my qn get_object_types] \
		 [my object_types_query -subtypes_first $subtypes_first]]
  }

  CrClass ad_instproc instance_select_query {
    {-select_attributes ""}
    {-orderby ""}
    {-where_clause ""}
    {-from_clause ""}
    {-with_subtypes:boolean true}
    {-publish_status}
    {-count:boolean false}
    {-folder_id}
    {-page_size 20}
    {-page_number ""}
  } {
    returns the SQL-query to select the CrItems of the specified object_type
    @select_attributes attributes for the sql query to be retrieved, in addion
      to ci.item_id acs_objects.object_type, which are always returned
    @param orderby for ordering the solution set
    @param where_clause clause for restricting the answer set
    @param with_subtypes return subtypes as well
    @param count return the query for counting the solutions
    @param folder_id parent_id
    @param publish_status one of 'live', 'ready' or 'production'
    @return sql query
  } {
    if {![info exists folder_id]} {my instvar folder_id}

    set attributes [list ci.item_id ci.name ci.publish_status acs_objects.object_type] 
    foreach a $select_attributes {
      if {$a eq "title"} {set a cr.title}
      lappend attributes $a
    }
    set type_selection [my type_selection -with_subtypes $with_subtypes]
    #my log "type_selection -with_subtypes $with_subtypes returns $type_selection"
    if {$count} {
      set attribute_selection "count(*)"
      set orderby ""      ;# no need to order when we count
      set page_number  ""      ;# no pagination when count is used
    } else {
      set attribute_selection [join $attributes ,]
    }
    
    set cond [list]
    if {$type_selection ne ""} {lappend cond $type_selection}
    if {$where_clause   ne ""} {lappend cond $where_clause}
    if {[info exists publish_status]} {lappend cond "ci.publish_status eq '$publish_status'"}
    lappend cond "coalesce(ci.live_revision,ci.latest_revision) = cr.revision_id 
        and parent_id = $folder_id and acs_objects.object_id = cr.revision_id"

    if {$page_number ne ""} {
      set limit $page_size
      set offset [expr {$page_size*($page_number-1)}]
    } else {
      set limit ""
      set offset ""
    }

    set sql [::xo::db::sql select \
                -vars $attribute_selection \
                -from "acs_objects, cr_items ci, cr_revisions cr $from_clause" \
                -where [join $cond " and "] \
                -orderby $orderby \
                -limit $limit -offset $offset]
    #my log "--sql=$sql"
    return $sql
  }

  CrClass ad_instproc instantiate_all {
    {-select_attributes ""}
    {-orderby ""}
    {-where_clause ""}
    {-with_subtypes:boolean true}
    {-folder_id}
    {-page_size 20}
    {-page_number ""}
  } {
    Return all instances of an content type class matching the
    specified clauses.
  } {
    set __result [::xo::OrderedComposite new]
    uplevel #1 [list $__result volatile]
    #$__result proc destroy {} {my log "-- "; next}

    set __attributes [list] 
    foreach a [concat [list ci.item_id acs_objects.object_type] \
                   $select_attributes] {
      lappend __attributes [lindex [split $a .] end]
    }

    db_foreach instance_select \
        [my instance_select_query \
             -folder_id $folder_id \
             -select_attributes $select_attributes \
             -with_subtypes $with_subtypes \
             -where_clause $where_clause \
             -orderby $orderby \
             -page_size $page_size -page_number $page_number] {
               set __o [$object_type create ${__result}::$item_id]
               $__result add $__o
               #my log "-- $__result add $__o, $object_type $item_id"
               foreach __a $__attributes {$__o set $__a [set $__a]}
             }
    return $__result
  }

  CrClass ad_instproc instantiate_objects {
    {-dbn ""}
    {-sql ""}
    {-full_statement_name ""}
  } {
    Return a set of instances of objects. It creates plain objects
    of type ::xotcl::Object just containing the variables that
    the sql query returns.

    The container and contained objects are automatically 
    destroyed on cleanup of the connection thread
  } {
    set __result [::xo::OrderedComposite new -destroy_on_cleanup]
    #$__result proc destroy {} {my log "-- "; next}

    db_with_handle -dbn $dbn db {
      set selection [db_exec select $db $full_statement_name $sql]
      while {1} {
        set continue [ns_db getrow $db $selection]
        if {!$continue} break
        set o [::xotcl::Object new]
        foreach {att val} [ns_set array $selection] {$o set $att $val}

        if {[$o exists object_type]} {
          # set the object type if it looks like from xotcl
          if {[string match "::*" [set ot [$o set object_type]] ]} {
            $o class $ot
          }
        }
        #my log "--DB more = $continue [$o serialize]" 
        $__result add $o
      }
    }
    return $__result
  }

  Class create Attribute -parameter {
    attribute_name datatype pretty_name {sqltype "text"}
    default help_text spec validator
  }

  Class create CrItem -parameter {
    package_id 
    {title ""} 
    {mime_type text/plain}
    {nls_language en_US}
    {publish_status ready}
  }

  CrItem instproc initialize_loaded_object {} {
    # dummy action, to be refined
  }

  CrItem ad_proc instantiate {
    -item_id
    {-revision_id 0}
  } {
    Instantiate the live revision or the specified revision of an 
    CrItem. 
    @return object containing the attributes of the CrItem
  } { 
    set object_type [ns_cache eval xotcl_object_type_cache \
                         [expr {$item_id ? $item_id : $revision_id}] {
      if {$item_id} {
        db_1row [my qn get_class] "select content_type as object_type from cr_items where item_id=$item_id"
      } else {
        db_1row [my qn get_class] "select object_type from acs_objects where object_id=$revision_id"
      }
      return $object_type
    }]
    #if {![string match "::*" $object_type]} {set object_type ::$object_type}
    return [$object_type instantiate -item_id $item_id -revision_id $revision_id]
  }
  

  CrItem ad_proc delete {
    -item_id 
  } {
    Delete a CrItem in the database
  } {
    db_1row [my qn get_class_and_folder] \
        "select content_type as object_type from cr_items where item_id = $item_id"
    $object_type delete -item_id $item_id
  }

  CrItem ad_proc lookup {
    -name:required
    -parent_id:required
  } {
    Lookup CR item from  title and folder (parent_id)
    @return item_id or 0 if not successful
  } {
    if {[db_0or1row [my qn entry_exists_select] "\
        select item_id from cr_items where name = :name and parent_id = :parent_id" ]} {
      #my log "-- found $item_id for $name in folder '$parent_id'"
      return $item_id
    }
    #my log "-- nothing found for $name in folder '$parent_id'"
    return 0
  }

  if {[db_driverkey ""] eq "postgresql"} {

    # provide the appropriate db_* call for the view update. Earlier
    # versions up to 5.3.0d1 used db_dml, newer versions (around july
    # 2006) have to use db_0or1row, when the patch for deadlocks and
    # duplicate items is applied...
    
    apm_version_get -package_key acs-content-repository -array info
    array get info
    CrItem set insert_view_operation \
        [expr {[apm_version_names_compare $info(version_name) 5.3.0d1] < 1 ? "db_dml" : "db_0or1row"}]
    array unset info
  } else { ;# Oracle
    CrItem set insert_view_operation db_dml
  }
  
  # uncomment the following line, if you want to force db_0or1row for
  # update operations (e.g. when using the provided patch for the
  # content repository in a 5.2 installation)

  #CrItem set insert_view_operation db_0or1row

  CrItem instproc update_content_length {storage_type revision_id} {
    if {$storage_type eq "file"} {
      db_dml [my qn update_content_length] "update cr_revisions \
                set content_length = [file size [my set import_file]] \
                where revision_id = $revision_id"
    }
  }
  CrItem instproc update_content {revision_id content} {
    [my info class] instvar storage_type 
    if {$storage_type eq "file"} {
      my log "--update_content not implemented for type file"
    } else {
      db_dml [my qn update_content] "update cr_revisions \
                set content = :content where revision_id = $revision_id"
    }
  }

  CrItem instproc current_user_id {} {
    if {[my isobject ::xo::cc]} {return [::xo::cc user_id]}
    if {[ad_conn isconnected]}  {return [ad_conn user_id]}
    return ""
  }

  CrItem ad_instproc save {-creation_user_id {-live_p:boolean true}} {
    Updates an item in the content repository. We insert a new revision instead of 
    changing the current revision.
    @param creation_user_id
    @param live_p make this revision the live revision
  } {
    set __atts [concat \
                    [list item_id revision_id creation_user] \
                    [[my info class] edit_atts]]
    # "name" is not part of the *i rule, ignore it for now
    # TODO: are all atts really useful here? also in save_new
    set __p [lsearch $__atts name]
    if {$__p > -1} {set __atts [lreplace $__atts $__p $__p]}

    eval my instvar $__atts 
    set creation_user [expr {[info exists creation_user_id] ?
                             $get_creation_user_id :
                             [my current_user_id]}]
    set old_revision_id [my set revision_id]
    [self class] instvar insert_view_operation
    db_transaction {
      [my info class] instvar storage_type
      set revision_id [db_nextval acs_object_id_seq]
      if {$storage_type eq "file"} {
        my instvar import_file
        set text [cr_create_content_file $item_id $revision_id $import_file]
      }
      $insert_view_operation [my qn revision_add] \
          "insert into [[my info class] set table_name]i ([join $__atts ,]) \
                values (:[join $__atts ,:])"
      my update_content_length $storage_type $revision_id
      if {$live_p} {
        ::xo::db::sql::content_item set_live_revision \
            -revision_id $revision_id \
            -publish_status [my set publish_status]
      } else {
        # if we do not make the revision live, use the old revision_id,
        # and let CrCache save it
        set revision_id $old_revision_id
      }
    }
    return $item_id
  }

  if {[apm_version_names_compare [ad_acs_version] 5.2] > -1} {
    ns_log notice "--OpenACS Version 5.2 or newer [ad_acs_version]"
#     CrItem set content_item__new_args {
#       name parent_id creation_user {item_subtype "content_item"} {content_type $object_type} 
#       description mime_type nls_language {is_live f} storage_type package_id 
#     }
    CrItem set content_item__new_args {
      -name $name -parent_id $parent_id -creation_user $creation_user \
        -item_subtype "content_item" -content_type $object_type \
        -description $description -mime_type $mime_type -nls_language $nls_language \
        -is_live f -storage_type $storage_type -package_id $package_id
    }
  } else {
    ns_log notice "--OpenACS Version 5.1 or older [ad_acs_version]"
#     CrItem set content_item__new_args {
#       name parent_id creation_user {item_subtype "content_item"} {content_type $object_type} 
#       description mime_type nls_language {is_live f} storage_type
#     }
    CrItem set content_item__new_args {
      -name $name -parent_id $parent_id -creation_user $creation_user \
        -item_subtype "content_item" -content_type $object_type \
        -description $description -mime_type $mime_type -nls_language $nls_language \
        -is_live f -storage_type $storage_type
    }
  }

  CrItem ad_instproc set_live_revision {-revision_id:required {-publish_status "ready"}} {
    @param revision_id
    @param publish_status one of 'live', 'ready' or 'production'
  } {
    ::xo::db::sql::content_item set_live_revision \
        -revision_id $revision_id \
        -publish_status $publish_status
  }

  CrItem ad_instproc save_new {-package_id -creation_user_id {-live_p:boolean true}} {
    Insert a new item to the content repository
    @param package_id
    @param creation_user_id
    @param live_p make this revision the live revision
  } {
    set __class [my info class]
    my instvar parent_id item_id import_file

    set __atts  [list item_id revision_id creation_user]
    foreach __var [$__class edit_atts] {
      my instvar $__var
      lappend __atts $__var
      if {![info exists $__var]} {set $__var ""}
      #my log "--V importing var $__var"
    }

    set creation_user [expr {[info exists creation_user_id] ?
                             $get_creation_user_id :
                             [my current_user_id]}]

    # "name" is not part of the *i rule, ignore it for now
    set __p [lsearch $__atts name]
    if {$__p > -1} {set __atts [lreplace $__atts $__p $__p]}

    if {![info exists package_id]} {
      set package_id [expr {[my exists package_id] ? [my set package_id] : 0}]
    }
    [self class] instvar insert_view_operation

    db_transaction {
      $__class instvar storage_type object_type
      #$__class folder_type -folder_id $parent_id register
      [self class] lock acs_objects "SHARE ROW EXCLUSIVE"
      set revision_id [db_nextval acs_object_id_seq]

      if {$name eq ""} {
	# we have an autonamed item, use a unique value for the name
	set name [expr {[my exists __autoname_prefix] ? 
                        "[my set __autoname_prefix]$revision_id" : $revision_id}]
        if {$title eq ""} {
          set title [expr {[my exists __title_prefix] ? 
                          "[my set __title_prefix] ($name)" : $name}]
        }
      }
      set item_id [eval ::xo::db::sql::content_item new [[self class] set content_item__new_args]]
      if {$storage_type eq "file"} {
        set text [cr_create_content_file $item_id $revision_id $import_file]
      }
      #my log "--V atts=([join $__atts ,])\nvalues=(:[join $__atts ,:])"
      $insert_view_operation  [my qn revision_add] \
          "insert into [$__class set table_name]i ([join $__atts ,]) \
                values (:[join $__atts ,:])"
      my update_content_length $storage_type $revision_id
      if {$live_p} {
        ::xo::db::sql::content_item set_live_revision \
            -revision_id $revision_id \
            -publish_status [my set publish_status] 
      }
    }
    my set revision_id $revision_id
    my db_1row  [my qn get_dates] {
      select creation_date, last_modified 
      from acs_objects where object_id = :revision_id
    }
    return $item_id
  }

  CrItem ad_instproc delete {} {
    Delete the item from the content repositiory with the item_id taken from the 
    instance variable.
  } {
    # delegate deletion to the class
    [my info class] delete [my set item_id]
  }

  ::Generic::CrItem instproc revisions {} {

    TableWidget t1 -volatile \
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
          ImageAnchorField live_revision -label [_ xotcl-core.live_revision] \
              -src /resources/acs-subsite/radio.gif \
              -width 16 -height 16 -border 0 -html {align center}
          ImageField_DeleteIcon version_delete -label "" -html {align center}
        }

    set user_id [my current_user_id]
    set page_id [my set item_id]
    set live_revision_id [content::item::get_live_revision -item_id $page_id]
    my instvar package_id
    set base [$package_id url]
    set sql [::xo::db::sql select \
		 -map_function_names true \
		 -vars "ci.name, n.revision_id as version_id,\
                        person__name(n.creation_user) as author, \
                        n.creation_user as author_id, \
                        to_char(n.last_modified,'YYYY-MM-DD HH24:MI:SS') as last_modified_ansi,\
                        n.description,\
                        acs_permission__permission_p(n.revision_id,:user_id,'admin') as admin_p,\
                        acs_permission__permission_p(n.revision_id,:user_id,'delete') as delete_p,\
                        r.content_length,\
                        content_revision__get_number(n.revision_id) as version_number " \
		 -from "cr_revisionsi n, cr_items ci, cr_revisions r" \
		 -where "ci.item_id = n.item_id and ci.item_id = :page_id
             and r.revision_id = n.revision_id 
             and exists (select 1 from acs_object_party_privilege_map m
                         where m.object_id = n.revision_id
                          and m.party_id = :user_id
                          and m.privilege = 'read')" \
		 -orderby "n.revision_id desc"]
    
    db_foreach revisions_select $sql {
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
    }
    
    # providing diff links to the prevision versions. This can't be done in
    # the first loop, since we have not yet the revision id of entry in the next line.
    set lines [t1 children]
    for {set i 0} {$i < [llength $lines]-1} {incr i} {
      set e [lindex $lines $i]
      set n [lindex $lines [expr {$i+1}]]
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
      if {$user_id == 0 && $login} {
        auth::require_login
      } elseif {[my set creation_user] == $user_id} {
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

  #
  # Form template class
  #
  
  Class CrCache 
  CrCache instproc fetch_object {
    -item_id:required
    {-revision_id 0}
    -object:required
  } {
    set code [ns_cache eval xotcl_object_cache $object {
      set created 1
      #my log "--CACHE new new [self]"
      set o [next]
      return [::Serializer deepSerialize $o]
    }]
    #my log "--CACHE: [self args], created [info exists created] o [info exists o]"
    if {![info exists created]} {
      if {[my isobject $object]} {
        my log "--!! $object exists already"
      } else {
        set o [eval $code]
      }
    }
    return $object
  }
  CrCache instproc delete {-item_id} {
    next
    ns_cache flush xotcl_object_cache ::$item_id
    # we should probably flush as well cached revisions
  }

  Class CrCache::Item
  CrCache::Item instproc save args {
    set r [next]
    #my log "--CACHE saving [self] in cache"
    ns_cache set xotcl_object_cache [self] \
        [::Serializer deepSerialize [self]]
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
    ns_cache flush xotcl_object_cache [self]
    next
  }
  
  CrClass instmixin CrCache
  CrItem instmixin CrCache::Item
  


  #
  # Form template class
  #
### FIXME: form should get a package id as parameter
  Class Form -parameter {
    fields 
    data
    {folder_id -100}
    {name {[namespace tail [self]]}}
    add_page_title
    edit_page_title
    {validate ""}
    {html ""}
    {with_categories false}
    {submit_link "."}
    {action "[::xo::cc url]"}
  } -ad_doc {
    Class for the simplified generation of forms. This class was designed 
    together with the content repository class 
    <a href='/xotcl/show-object?object=::Generic::CrClass'>::Generic::CrClass</a>.

    <ul>
    <li><b>fields:</b> form elements as described in 
       <a href='/api-doc/proc-view?proc=ad_form'>ad_form</a>.
    <li><b>data:</b> data object (e.g. instance if CrItem) 
    <li><b>folder_id:</b> associated folder id
    <li><b>name:</b> of this form, used for naming the template, 
       defaults to the object name
    <li><b>add_page_title:</b> page title when adding content items
    <li><b>edit_page_title:</b> page title when editing content items
    <li><b>with_categories:</b> display form with categories (default false)
    <li><b>submit_link:</b> link for page after submit
    </ul>
  }
  
  Form instproc init {} {
    set level [template::adp_level]
    my forward var uplevel #$level set 

    my instvar data folder_id
    set class     [$data info class]
    set folder_id [$data set parent_id]

    if {![my exists add_page_title]} {
      my set add_page_title [_ xotcl-core.create_new_type \
                                 [list type [$class pretty_name]]]
    }
    if {![my exists edit_page_title]} {
      my set edit_page_title [_ xotcl-core.edit_type \
                                  [list type [$class pretty_name]]]
    }

    # check, if the specified fields are available from the data source
    # and ignore the unavailable entries
    set checked_fields [list]
    set available_atts [$class edit_atts]
    #my log "-- available atts <$available_atts>"
    lappend available_atts [$class id_column] item_id

    if {![my exists fields]} {my mkFields}
    #my log --fields=[my fields]
  }
  
  Form instproc form_vars {} {
    set vars [list]
    foreach varspec [my fields] {
      lappend vars [lindex [split [lindex $varspec 0] :] 0]
    }
    return $vars
  }
  Form instproc new_data {} {
    my instvar data
    #my log "--- new_data ---"
    foreach __var [my form_vars] {
      $data set $__var [my var $__var]
    }
    $data initialize_loaded_object
    $data save_new
    return [$data set item_id]
  }
  Form instproc edit_data {} {
    #my log "--- edit_data --- setting form vars=[my form_vars]"
    my instvar data
    foreach __var [my form_vars] {
      $data set $__var [my var $__var]
    }
    $data initialize_loaded_object
    db_transaction {
      $data save
      set old_name [::xo::cc form_parameter __object_name ""]
      set new_name [$data set name]
      if {$old_name ne $new_name} {
        db_dml [my qn update_rename] "update cr_items set name = :new_name \
                where item_id = [$data set item_id]"
      }
    }
    return [$data set item_id]
  }

  Form instproc request {privilege} {
    my instvar edit_form_page_title context data
    set package_id [$data package_id]

    if {[my isobject ::$package_id] && ![::$package_id exists policy]} {
      # not needed, if governed by a policy
      auth::require_login
      permission::require_permission \
          -object_id $package_id \
          -privilege $privilege
    }
      
    set edit_form_page_title [expr {$privilege eq "create" ? 
                   [my add_page_title] : [my edit_page_title]}]
    set context [list $edit_form_page_title]
  }

  Form instproc new_request {} {
    #my log "--- new_request ---"
    my request create
    my instvar data
    #my log "--VAR [my var item_id]"
    foreach var [[$data info class] edit_atts] {
      if {[$data exists $var]} {
        my var $var [list [$data set $var]]
      }
    }
  }
  Form instproc edit_request {item_id} {
    my instvar data
    #my log "--- edit_request ---"
    my request write
    foreach var [[$data info class] edit_atts] {
      if {[$data exists $var]} {
        my var $var [list [$data set $var]]
      }
    }
  }

  Form instproc on_submit {item_id} {
    # The content of this proc is strictly speaking not necessary.
    # However, on redirects after a submit to the same page, it
    # ensures the setting of edit_form_page_title and context
    my request write
  }

  Form instproc on_validation_error {} {
    my instvar edit_form_page_title context
    #my log "-- "
    set edit_form_page_title [my edit_page_title]
    set context [list $edit_form_page_title]
  }
  Form instproc after_submit {item_id} {
    my instvar data
    set link [my submit_link]
    if {$link eq "view"} {
      set link [export_vars -base $link {item_id}]
    }
    ns_log notice "-- redirect to $link // [string match *\?* $link]"
    ad_returnredirect $link
    ad_script_abort
  }
 
  Form ad_instproc generate {
    {-template "formTemplate"}
    {-export}
  } {
    the method generate is used to actually generate the form template
    from the specifications and to set up page_title and context 
    when appropriate.
    @template is the name of the tcl variable to contain the filled in template
    @export list of attribue value pairs to be exported to the form (nested list)
  } {
    # set form name for adp file
    my set $template [my name]
    my instvar data folder_id
    set object_type [[$data info class] object_type]
    set object_name [expr {[$data exists name] ? [$data set name] : ""}]
    #my log "-- $data, cl=[$data info class] [[$data info class] object_type]"
    
    my log "--e [my name] final fields [my fields]"
    set exports [list [list object_type $object_type] \
                     [list folder_id $folder_id] \
                     [list __object_name $object_name]] 
    if {[info exists export]} {foreach pair $export {lappend exports $pair}}
    ad_form -name [my name] -form [my fields] \
        -export $exports -action [my action] -html [my html]

    set new_data            "set item_id \[[self] new_data\]"
    set edit_data           "set item_id \[[self] edit_data\]"
    set new_request         "[self] new_request"
    set edit_request        "[self] edit_request \$item_id"
    set after_submit        "[self] after_submit \$item_id"
    set on_validation_error "[self] on_validation_error"
    set on_submit           "[self] on_submit \$item_id"

    if {[my with_categories]} {
      set coid [expr {[$data exists item_id] ? [$data set item_id] : ""}]
      category::ad_form::add_widgets -form_name [my name] \
          -container_object_id [$data package_id] \
          -categorized_object_id $coid

      append new_data {
        category::map_object -remove_old -object_id $item_id $category_ids
        #ns_log notice "-- new data category::map_object -remove_old -object_id $item_id $category_ids"
        #db_dml [my qn insert_asc_named_object] \
        #    "insert into acs_named_objects (object_id,object_name,package_id) \
        #     values (:item_id, :name, :package_id)"
      }
      append edit_data {
        #db_dml [my qn update_asc_named_object] \
        #    "update acs_named_objects set object_name = :name, \
        #        package_id = :package_id where object_id = :item_id"
        #ns_log notice "-- edit data category::map_object -remove_old -object_id $item_id $category_ids"
        category::map_object -remove_old -object_id $item_id $category_ids
      }
      append on_submit {
        set category_ids [category::ad_form::get_categories \
                              -container_object_id $package_id]
      }
    }
    #ns_log notice "-- ad_form new_data=<$new_data> edit_data=<$edit_data> edit_request=<$edit_request>"

    # action blocks must be added last
    ad_form -extend -name [my name] \
        -validate [my validate] \
        -new_data $new_data -edit_data $edit_data -on_submit $on_submit \
        -new_request $new_request -edit_request $edit_request \
        -on_validation_error $on_validation_error -after_submit $after_submit
  }

  #
  # List template class
  #

  Class List -parameter {
    fields 
    object_type
    object_types
    {with_subtypes true}
    {name {[namespace tail [self]]}}
    {edit_link edit}
    {view_link view}
    {delete_link delete}
    {folder_id -100}
  } -ad_doc {
    Class for the simplified generation of lists. This class was designed 
    together with the content repository class 
    <a href='/xotcl/show-object?object=::Generic::CrClass'>::Generic::CrClass</a>.
    This class can be parameterized with
    <ul>
    <li><b>fields:</b> form elements as described in 
       <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>.
    <li><b>object_types:</b> instances of
       <a href='/xotcl/show-object?object=::Generic::CrClass'>::Generic::CrClass</a>,
       used for computing the actions of the list.
    <li><b>object_type:</b> name of the most generic object type for 
       filling this list; used for retrieveing the items through an 
       sql query, which is obtained from the object_type through 
       <a href='/api-doc/proc-view?proc=::Generic::CrClass%20instproc%20instance_select_query'>instance_select_query</a>.
       The provided value must be an instance of
       <a href='/xotcl/show-object?object=::Generic::CrClass'>::Generic::CrClass</a>.
    <li><b>with_subtypes:</b> compute subtypes (default true),
    <li><b>name:</b> of this form, used for naming the template, 
       defaults to the object name
    <li><b>edit_link:</b> link to edit content item (default: edit)
    <li><b>delete_link:</b> link to delete content item (default: delete)
    <li><b>view_link:</b> link to view content item (default: view)
    </ul>
  }

  List ad_instproc actions {} {
    actions is a method to compute the actions of the list
    depending on the object types. It can be easily overwritten 
    by e.g. a subclass or an object specific method
  } {
    my instvar object_types
    set actions [list]
    foreach object_type $object_types {
      lappend actions \
          "Add [$object_type pretty_name]" \
          [export_vars -base [my edit_link] {object_type folder_id}] \
          "Add a new item of kind [$object_type pretty_name]"
    }
    return $actions
  }

  List ad_instproc elements {} {
    elements is a method to compute the elements of each line in the list
    depending on the specified fields. It can be easily overwritten 
    by e.g. a subclass or an object specific method
  } {
    set elements [list]
    foreach {e spec} [my fields] {
      switch -exact $e {
        EDIT {
          lappend elements edit {
            link_url_col edit_url
            display_template {
              <img src='/resources/acs-subsite/Edit16.gif' \
                  title='Edit Item' alt='edit' \
                  width="16" height="16" border="0">
            }
            sub_class narrow
          }
        }
        DELETE {
          lappend elements delete {
            link_url_col delete_url 
            display_template {
              <img src='/resources/acs-subsite/Delete16.gif' \
                  title='Delete Item' alt='delete' \
                  width="16" height="16" border="0">
            }
            sub_class narrow
          }
        }
        VIEW {
          lappend elements view {
            link_url_col view_url
            display_template {
              <img src='/resources/acs-subsite/Zoom16.gif' \
                  title='View Item' alt='view' \
                  width="16" height="16" border="0">
            }
            sub_class narrow
          }
        }
        default {
          lappend elements $e $spec
        }
      }
    }
    return $elements
  }


  List ad_instproc generate {
    {-orderby ""}
    -template
  } {
    the method generate is used to actually generate the list template
    from the specifications and to fill in the actual values from a generic
    query
    @param order_by specifies the attribute the order of the listing
    @template is the name of the tcl variable to contain the filled in template
  } {
    my instvar object_type with_subtypes
    
    if {![info exists template]} {
      set template [my name]
    }
    uplevel set template $template
    
    set select_attributes [list]
    foreach {e spec} [my fields] {
      if {[lsearch -exact {item_id object_type EDIT DELETE VIEW} $e] == -1} {
        lappend select_attributes $e
      }
    }

    template::list::create \
        -name $template \
        -actions [my actions] \
        -elements [my elements]

    db_multirow \
        -extend {
          edit_url
          delete_url
          view_url
        } $template instance_select [$object_type instance_select_query \
              -folder_id [my folder_id] \
              -select_attributes $select_attributes \
              -with_subtypes $with_subtypes \
              -orderby $orderby] {
        set view_url [export_vars -base [my view_link] {item_id}]
        set edit_url [export_vars -base [my edit_link] {item_id}]
        set delete_url [export_vars -base [my delete_link] {item_id}]
      }
  }

  namespace export CrItem
}
namespace import -force ::Generic::*



