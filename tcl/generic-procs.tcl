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
    set id [apm_version_id_from_package_key $key]
    set mount_url [site_node::get_children -all -package_key $key -node_id $id]
    array set site_node [site_node::get -url $mount_url]
    return $site_node(package_id)
  }

  CrClass instproc unknown { obj args } {
    my log "unknown called with $obj $args"
  }

  CrClass set common_query_atts {
    item_id creation_user creation_date last_modified object_type
    creation_user last_modified
  }
  #if {[apm_version_names_compare [ad_acs_version] 5.2] > -1} {
  #   CrClass lappend common_query_atts object_package_id
  #}

  CrClass set common_insert_atts {title description mime_type nls_language text}

  CrClass instproc object_types {
    {-subtypes_first:boolean false}
  } {
    my instvar object_type_key
    set order_clause [expr {$subtypes_first ? "order by tree_sortkey desc":""}]
    return [db_list get_object_types "
      select object_type from acs_object_types where 
      tree_sortkey between :object_type_key and tree_right(:object_type_key)
      $order_clause
    "]
  }

  CrClass instproc edit_atts {} {
    concat [[self class] set common_insert_atts] [my sql_attribute_names]
  }

  CrClass instproc object_type_exists {} {
    my instvar object_type
    expr {$object_type eq [db_list select_type {
      select object_type from acs_object_types where 
      object_type = :object_type
    }]}
  }

  CrClass ad_instproc folder_type {
    -folder_id
    operation
  } {
    register the current object type for folder_id. If folder_id 
    is not specified, use the instvar of the class instead.
  } {
    if {$operation ne "register" && $operation ne "unregister"} {
      error "[self] operation for folder_type must be '\
	register' or 'unregister'"
    }
    my instvar object_type
    if {![info exists folder_id]} {
      my instvar folder_id
    }
    db_1row register_type "select content_folder__${operation}_content_type(\
	$folder_id,:object_type,'t')"
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
      db_1row create_type {
	select content_type__create_type(
           :object_type,:supertype,:pretty_name, :pretty_plural,
  	   :table_name, :id_column, :name_method
        )
      }
      if {[my cr_attributes] ne ""} {
	set o [::xo::OrderedComposite new -volatile -contains [my cr_attributes]]
	foreach att [$o children] {
	  $att instvar attribute_name datatype pretty_name
	  db_1row create_att {
	    select content_type__create_attribute(
                :object_type,:attribute_name,:datatype,
                :pretty_name,null,null,null,'text'
            )
	  }
	}
      }
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
      db_1row drop_type {
	select content_type__drop_type(:object_type,'t','t')
      }
    }
  }

  CrClass ad_instproc require_folder {
    {-parent_id -100} 
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
    } elseif {[ad_conn isconnected]} {
      set package_id [ad_conn package_id]
      set cid ""
      if {[info command dotlrn_community::get_community_id_from_url] ne ""} {
	set cid [dotlrn_community::get_community_id_from_url -url [ad_conn url]]
      }
      if {$cid eq ""} {
	set cid $package_id
      }
    } else {
      set cid -100
    }
    set fullname "$name: $cid"

    if {[info command content::item::get_id_by_name] eq ""} {
      set folder_id ""
      db_0or1row get_id_by_name "select item_id as folder_id from cr_items \
	 where name = :fullname and parent_id = :parent_id"
    } else {
      set folder_id [content::item::get_id_by_name \
			 -name $fullname -parent_id $parent_id]
    }
    if {$folder_id eq ""} {
      set folder_id [content::folder::new -name $fullname -parent_id $parent_id \
			 -package_id $package_id]
    }
    if {[apm_version_names_compare [ad_acs_version] 5.2] > -1} { 
      #### for search, we need the package_id
      set pid [db_string get_package_id "select package_id from acs_objects where object_id = $folder_id"]
      if {$pid eq ""} {
	db_dml update_package_id \
	    "update acs_objects set package_id = :package_id where object_id = $folder_id"
      }
    }
    my require_folder_object -folder_id $folder_id -package_id $package_id
    return $folder_id
  }

  CrClass ad_proc require_folder_object {
    -folder_id
    -package_id 
  } {
    Dummy stub; let specializations define it
  } {
  }

  CrClass instproc getFormClass {} {
    set nsform [ns_getform]
    set item_id [ns_set get $nsform item_id] ;# item_id should be be hardcoded
    set confirmed_p [ns_set get $nsform __confirmed_p]
    set new_p [ns_set get $nsform __new_p]
    my log "-- item_id '$item_id', confirmed_p '$confirmed_p', new_p '$new_p'"
    if {$item_id ne "" && $new_p ne "1" && [my exists edit_form]} {
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
    set sql_attribute_names [list]
    set o [xo::OrderedComposite new -volatile -contains [my cr_attributes]]
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
    }
    my set object_type_key [db_list get_tree_sortkey {
      select tree_sortkey from acs_object_types 
      where object_type = :object_type
    }]
    next
  }
  
  CrClass ad_instproc lookup {
    -title:required
    -parent_id:required
  } {
    Check, whether an content item with the given title exists.
    If not, return 0.

    @return item_id
  } {
    my instvar table_name

    if {[db_0or1row entry_exists_select "
       select n.item_id from cr_items ci, ${table_name}i n
       where  n.title = :title and    
       n.[my id_column] = ci.live_revision and ci.parent_id = :parent_id"]} {
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
    revision of the item_id.

    @return cr item object
  } {
    #my log "-- [self args]"
    my instvar table_name 
    $object instvar parent_id
    set raw_atts [concat [[self class] set common_query_atts] [my edit_atts]]
    set atts [list data]
    foreach v $raw_atts {
      catch {$object instvar $v}
      lappend atts n.$v
    }
    if {$revision_id} {
      db_1row note_select "
       select [join $atts ,], i.parent_id from [my set table_name]i n, cr_items i
       where  n.revision_id = :revision_id and i.item_id = n.item_id"
    } else {
      db_1row note_select "
       select [join $atts ,], i.parent_id from cr_items i, [my set table_name]i n
       where  i.item_id = :item_id 
       and    n.[my id_column] = i.live_revision"
    }
    $object set text $data
    $object set item_id $item_id
    $object initialize_loaded_object
    return $object
  }


  CrClass ad_instproc instantiate {
    -item_id
    {-revision_id 0}
  } { 
    Retrieve either the live revision or a specified revision
    of a content item with all attributes. 
    The retrieved attributes are strored in the instance variables in
    class representing the object_type.

    @param item_id id of the item to be retrieved.
    @param revision_id revision-id of the item to be retrieved.
  } {
    set o [my create ::[expr {$revision_id ? $revision_id : $item_id}]]
    my fetch_object -object $o -item_id $item_id -revision_id $revision_id
  }

  CrClass ad_instproc delete {
    -item_id:required
  } { 
    Delete a content item from the content repository.
    @param item_id id of the item to be deleted
  } {
    db_exec_plsql note_delete {
      select content_item__delete(:item_id)
    }
  }

  CrClass ad_instproc instance_select_query {
    {-select_attributes ""}
    {-order_clause ""}
    {-where_clause ""}
    {-with_subtypes:boolean true}
    {-count:boolean false}
    {-folder_id}
    {-page_size 20}
    {-page_number ""}
  } {
    returns the SQL-query to select the CrItems of the specified object_type
    @select_attributes attributes for the sql query to be retrieved, in addion
      to ci.item_id acs_objects.object_type, which are always returned
    @param order_clause clause for ordering the solution set
    @param where_clause clause for restricting the answer set
    @param with_subtypes return subtypes as well
    @param count return the query for counting the solutions
    @param folder_id parent_id
    @return sql query
  } {
    my instvar object_type_key
    if {![info exists folder_id]} {my instvar folder_id}

    set attributes [list ci.item_id acs_objects.object_type] 
    foreach a $select_attributes {
      if {$a eq "title"} {set a cr.title}
      lappend attributes $a
    }
    set type_selection [expr {$with_subtypes ? 
	      "acs_object_types.tree_sortkey between \
               '$object_type_key' and tree_right('$object_type_key')" :
	      "acs_object_types.tree_sortkey = '$object_type_key'"}]
    if {$count} {
      set attribute_selection "count(*)"
      set order_clause ""      ;# no need to order when we count
      set page_number  ""      ;# no pagination when count is used
    } else {
      set attribute_selection [join $attributes ,]
    }

    if {$where_clause ne ""} {
      set where_clause "and $where_clause"
    }
    if {$page_number ne ""} {
      set pagination "offset [expr {$page_size*($page_number-1)}] limit $page_size"
    } else {
      set pagination ""
    }
    return "select $attribute_selection
    from acs_object_types, acs_objects, cr_items ci, cr_revisions cr 
        where $type_selection
        and acs_object_types.object_type = ci.content_type
        and ci.live_revision = cr.revision_id 
        and parent_id = $folder_id and acs_objects.object_id = cr.revision_id \
	$where_clause $order_clause $pagination"
  }

  CrClass ad_instproc instantiate_all {
    {-select_attributes ""}
    {-order_clause ""}
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
	     -order_clause $order_clause \
	     -page_size $page_size -page_number $page_number] {
	       set __o [$object_type create ${__result}::$item_id]
	       $__result add $__o
	       #my log "-- $__result add $__o, $object_type $item_id"
	       foreach __a $__attributes {$__o set $__a [set $__a]}
 	     }
    return $__result
  }


  Class create Attribute -parameter {attribute_name datatype pretty_name}
  # create new objects as child of the callers namespace
  #Attribute proc new args {
  #  eval next -childof [uplevel namespace current] $args
  #}

  Class create CrItem 
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

    if {$item_id} {
      db_1row get_class "select content_type as object_type from cr_items where item_id=$item_id"
    } else {
      db_1row get_class "select object_type from acs_objects where object_id=$revision_id"
    }
    
    if {![string match "::*" $object_type]} {set object_type ::$object_type}
    set o [$object_type create ::[expr {$revision_id ? $revision_id : $item_id}]]
    $object_type fetch_object \
	-item_id $item_id -revision_id $revision_id -object $o
    #my log "-- fetched $o of type $object_type"
    return $o
  }
  

  CrItem ad_proc delete {
    -item_id 
  } {
    Delete a CrItem in the database
  } {
    db_1row get_class_and_folder \
        "select content_type as object_type from cr_items where item_id = $item_id"
    $object_type delete -item_id $item_id
  }

  CrItem ad_proc lookup {
    -title:required
    -parent_id:required
  } {
    Lookup CR item from  title and folder (parent_id)
    @return item_id or 0 if not successful
  } {
    if {[db_0or1row entry_exists_select "
	select i.item_id from cr_revisions r, cr_items i 
	where revision_id = i.live_revision and r.title = :title 
	and i.parent_id = :parent_id" ]} {
      #my log "-- found $item_id for $title in folder '$parent_id'"
      return $item_id
    }
    #my log "-- nothing found for $title in folder '$parent_id'"
    return 0
  }

  CrItem ad_instproc save {} {
    Updates an item in the content repository and makes
    it the live revision. We insert a new revision instead of 
    changing the current revision.
  } {
    set __atts [concat \
		    [list item_id revision_id creation_user] \
		    [[my info class] edit_atts]]
    eval my instvar $__atts 
    set creation_user [expr {[ad_conn isconnected] ? [ad_conn user_id] : ""}]

    db_transaction {
      set revision_id [db_nextval acs_object_id_seq]

      db_dml revision_add "
	insert into [[my info class] set table_name]i ([join $__atts ,]) 
	values (:[join $__atts ,:])"
      
      db_exec_plsql make_live {
	select content_item__set_live_revision(:revision_id)
      }
    }
    return $item_id
  }

  CrItem ad_instproc save_new {-package_id} {
    Insert a new item to the content repository and make
    it the live revision. 
  } {
    set __class [my info class]
    my instvar parent_id item_id

    set __atts  [list item_id revision_id creation_user]
    foreach __var [$__class edit_atts] {
      my instvar $__var
      lappend __atts $__var
      if {![info exists $__var]} {set $__var ""}
    }
    set creation_user [expr {[ad_conn isconnected] ? [ad_conn user_id] : ""}]

    db_transaction {
      $__class instvar storage_type object_type
      $__class folder_type -folder_id $parent_id register
       set item_id [db_exec_plsql note_insert "
	select content_item__new(:title,$parent_id,null,null,null,:creation_user,null,null,
				 'content_item',:object_type,:title,
				 :description,:mime_type,
				 :nls_language,:text,:storage_type)"]
      
      set revision_id [db_nextval acs_object_id_seq]
      db_dml revision_add "
	insert into [$__class set table_name]i ([join $__atts ,]) 
	values (:[join $__atts ,:])"
      
      db_exec_plsql make_live {
	select content_item__set_live_revision(:revision_id)
      }
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


  #
  # Form template class
  #
  
  Class Form -parameter {
    fields 
    data
    {folder_id -100}
    {name {[namespace tail [self]]}}
    add_page_title
    edit_page_title
    {validate ""}
    {with_categories false}
    {submit_link "."}
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
      my set add_page_title "New [$class pretty_name]"
    }
    if {![my exists edit_page_title]} {
      my set edit_page_title "Edit [$class pretty_name]"
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
    my log "--- new_data ---"
    foreach __var [my form_vars] {
      $data set $__var [my var $__var]
    }
    $data initialize_loaded_object
    $data save_new
    return [$data set item_id]
  }
  Form instproc edit_data {} {
    my log "--- edit_data ---"
    my instvar data
    foreach __var [my form_vars] {
      $data set $__var [my var $__var]
    }
    $data initialize_loaded_object
    $data save
    return [$data set item_id]
  }
  Form instproc request {privelege} {
    my instvar edit_form_page_title context
    auth::require_login
    permission::require_permission -object_id [ad_conn package_id] -privilege $privelege
    set edit_form_page_title [my add_page_title]
    set context [list $edit_form_page_title]
  }
  Form instproc new_request {} {
    my log "--- new_request ---"
    my request create
  }
  Form instproc edit_request {item_id} {
    my instvar data
    my log "--- edit_request ---"
    my request write
    foreach var [[$data info class] edit_atts] {
      my var $var [list [$data set $var]]
    }
  }

  Form instproc on_validation_error {} {
    my instvar edit_form_page_title context
    my log "-- "
    set edit_form_page_title [my edit_page_title]
    set context [list $edit_form_page_title]
  }
  Form instproc after_submit {item_id} {
    my instvar data
    set link [my submit_link]
    if {$link ne "." && ![string match {*[?]*} $link]} {
      set link [export_vars -base $link {item_id}]
    }
    ns_log notice "-- redirect to $link // [string match *\?* $link]"
    ad_returnredirect $link
    ad_script_abort
  }
 
  Form ad_instproc generate {
    {-template "formTemplate"}
  } {
    the method generate is used to actually generate the form template
    from the specifications and to set up page_title and context 
    when appropriate.
    @template is the name of the tcl variable to contain the filled in template
  } {
    # set form name for adp file
    my set $template [my name]
    my instvar data folder_id
    set object_type [[$data info class] object_type]
    #my log "-- $data, cl=[$data info class] [[$data info class] object_type]"

    #my log "--e final fields [my fields]"
    ad_form -name [my name] -form [my fields] \
	-export [list [list object_type $object_type] [list folder_id $folder_id]] 
    
    set new_data            "set item_id \[[self] new_data\]"
    set edit_data           "set item_id \[[self] edit_data\]"
    set new_request         "[self] new_request"
    set edit_request        "[self] edit_request \$item_id"
    set after_submit        "[self] after_submit \$item_id"
    set on_validation_error "[self] on_validation_error"
    set on_submit {}

    if {[my with_categories]} {
      set coid [expr {[$data exists item_id] ? [$data set item_id] : ""}]
      category::ad_form::add_widgets -form_name [my name] \
	  -container_object_id [ad_conn package_id] \
	  -categorized_object_id $coid

      append new_data {
	category::map_object -remove_old -object_id $item_id $category_ids
	#ns_log notice "-- new data category::map_object -remove_old -object_id $item_id $category_ids"
	db_dml insert_asc_named_object \
	    "insert into acs_named_objects (object_id,object_name,package_id) \
             values (:item_id, :title, :package_id)"
      }
      append edit_data {
	db_dml update_asc_named_object \
	    "update acs_named_objects set object_name = :title, \
		package_id = :package_id where object_id = :item_id"
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
    -order_by
    -template
  } {
    the method generate is used to actually generate the list template
    from the specifications and to fill in the actual values from a generic
    query
    @param order_by specifies the attribute the order of the listing
    @template is the name of the tcl variable to contain the filled in template
  } {
    my instvar object_type with_subtypes
    
    set order_clause [expr {[info exists order_by] ? "order by $order_by":""}]
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
  	      -order_clause $order_clause] {
        set view_url [export_vars -base [my view_link] {item_id}]
	set edit_url [export_vars -base [my edit_link] {item_id}]
	set delete_url [export_vars -base [my delete_link] {item_id}]
      }
  }

  namespace export CrItem
}
namespace import -force ::Generic::*

