ad_library {
  XOTcl API simple Content repository apps, supports categories.

  @author Gustaf Neumann
  @creation-date 2005-08-13
  @cvs-id $Id$
}

namespace eval ::Generic {

  # We do not want to re-source all of the user-data-models,
  # when small things in the definition of the CrClass change. Normally,
  # sourcing of this file causes CrClass do be destroyed with 
  # the consequence, that instances of CrClass loose their 
  # class-releationship. 

  Class CrClass -superclass Class -parameter {
    pretty_name
    pretty_plural
    {supertype content_revision}
    table_name
    id_column
    sql_attributes
    {name_method ""}
    {description ""}
    {mime_type text/plain}
    {nls_language ""}
    {text ""}
    {storage_type "text"}
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

    <p>Each content item is retrieved though the method 
    <a href='#instproc-get'>get</a>,
    added through the method 
    <a href='#instproc-add'>add</a>,
    edited (updated) throught the 
    method 
    <a href='#instproc-edit'>edit</a>,
    and deleted though the the method 
    <a href='#instproc-delete'>delete</a>. </p>

    <p>This Class provides generic methods for these purposes. For more 
    complex applications, these methods will be most probably overwritten
    by defining subclasses with (some of) these methods or by object 
    specific methods.</p>
  }

  CrClass instproc unknown { obj args } {
    my log "unknown called with $obj $args"
  }

  CrClass set query_atts {
    item_id creation_user creation_date last_modified object_type
  }
  CrClass set insert_atts {title description mime_type nls_language text}

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
    concat [[self class] set insert_atts] [my atts]
  }
  CrClass instproc atts {} {
    set atts [list [my id_column]]
    if {[my exists sql_attributes]} {
      foreach att [my sql_attributes] {
	lappend atts [lindex $att 0]
      }
    }
    return $atts
  }
  

  CrClass instproc object_type_exists {} {
    my instvar object_type
    expr {$object_type eq [db_list select_type {
      select object_type from acs_object_types where 
      object_type = :object_type
    }]}
  }
  
  CrClass ad_instproc create_object_type {} {
    Create an oacs object_type and a table for keeping the
    additional attributes.
  } {
    my instvar object_type supertype pretty_name pretty_plural \
	table_name id_column name_method

    my log "[self proc] $object_type"
    set st [my info superclass]
    if {$st ne "::xotcl::Object"} {
      set supertype [string trimleft $st :]
    }
    db_transaction {
      if {[my exists sql_attributes]} {
	set sql_atts [list]
	lappend sql_atts "$id_column integer primary key \
		references cr_revisions(revision_id)"
	foreach {att spec} [my sql_attributes] {
	  lappend sql_atts "$att $spec"
	}
	
	db_dml table_add "create table $table_name (\n[join $sql_atts ,\n])"
	my log "adding table explicitely"
      }
      db_1row create_type {
	select content_type__create_type(:object_type,:supertype,
					 :pretty_name, :pretty_plural,
					 :table_name, :id_column, :name_method)
      }
      db_1row register_type {
	select content_folder__register_content_type(-100,:object_type,'t')
      }
    }
  }

  CrClass ad_instproc drop_object_type {} {
    Delete the object type and remove the table for the attributes.
    This method should be called when all instances are deleted. It
    undoes everying what create_object_type has produced.
  } {
    my instvar object_type table_name
    db_transaction {
      db_1row unregister_type {
	select content_folder__unregister_content_type(-100,:object_type,'t')
      }
      db_1row drop_type {
	select content_type__drop_type(:object_type,'t','t')
      }
    }
  }

  CrClass instproc init {} {
    my instvar object_type
    set object_type [string trimleft [self] :]
    if {[my info superclass] ne "::xotcl::Object"} {
      my set superclass [[my info superclass] set object_type]
    }
    if {![my object_type_exists]} {
      my create_object_type
    }
    my set object_type_key [db_list get_tree_sortkey {
      select tree_sortkey from acs_object_types 
      where object_type = :object_type
    }]
    next
  }
  
  CrClass ad_instproc get {
    -item_id:required
  } { 
    Retrieve the live revision of a content item with all attributes. 
    The retrieved attributes are strored in the instance variables in
    class representing the object_type.

    @param item_id id of the item to be retreived.
  } {
    my instvar title table_name
    set raw_atts [concat [[self class] set query_atts] [my edit_atts]]
    set atts [list data]
    foreach v $raw_atts {
      catch {my instvar $v}
      lappend atts n.$v
    }
    
    db_1row note_select "
       select [join $atts ,] from cr_items ci, ${table_name}i n
       where  ci.item_id = :item_id 
       and    n.[my id_column] = ci.live_revision
    "
    my set text $data
    my set item_id $item_id
  }
  
  CrClass ad_instproc add {
    form
  } { 
    Insert a new item to the content repository and makes 
    it the live revision. This method obtains the values of 
    the new content item from the specified form.

    @param form form-object (instance of <a href='/xotcl/show-object?object=::Generic::Form'>::Generic::Form</a>) from where the values are obtained
    @return item_id of the new note.
  } {
    my instvar object_type table_name storage_type

    set atts [list item_id revision_id]
    foreach v [[self class] set insert_atts] {
      my instvar $v
      lappend atts $v
    }

    set form_vars [list]
    foreach var [$form form_vars] {lappend form_vars $var [uplevel set $var]}
    foreach var [$form form_vars] {set $var [uplevel set $var]}

    db_transaction {
      set item_id [db_exec_plsql note_insert {
	select content_item__new(:title,-100,null,null,null,null,null,null,
				 'content_item',:object_type,:title,
				 :description,:mime_type,
				 :nls_language,:text,:storage_type)
      }]
      
      set revision_id [db_nextval acs_object_id_seq]

      db_dml revision_add "
	insert into ${table_name}i ([join $atts ,]) 
	values (:[join $atts ,:])"
 
      my update_main_table -revision_id $revision_id -form_vars $form_vars

      db_exec_plsql make_live {
	select content_item__set_live_revision(:revision_id)
      }
    }
    return $item_id
  }
  
  CrClass instproc update_main_table {
    -revision_id
    -form_vars
  } {
    my instvar table_name
    if {[llength [my atts]]>1} {
      set vars [list]
      foreach a [lrange [my atts] 1 end] {lappend vars $a}
      catch {my instvar $vars}
      foreach {att val} $form_vars {set $att $val}
      if {[llength $vars]>1} {
	db_dml main_table_update "
	   update $table_name set ([join $vars ,]) = (:[join $vars ,:])
	   where [my id_column] = :revision_id"
      } else {
	db_dml main_table_update "
	   update $table_name set $vars = :$vars
	   where [my id_column] = :revision_id"
      }
    }
  }

  CrClass ad_instproc edit {
    form
  } { 
    Updates an item in the content repository and makes
    it the live revision. We insert a new revision instead of 
    changing the current revision.

    @param form form-object (instance of <a href='/xotcl/show-object?object=::Generic::Form'>::Generic::Form</a>) from where the values are obtained
  } {
    my instvar table_name item_id
    
    set atts [concat [list item_id revision_id] [[self class] set insert_atts]]
    catch {eval my instvar $atts}

    set form_vars [list]
    foreach var [$form form_vars] {lappend form_vars $var [uplevel set $var]}
    foreach var [$form form_vars] {set $var [uplevel set $var]}

    db_transaction {
      set revision_id [db_nextval acs_object_id_seq]
      
      db_dml revision_add "
	insert into ${table_name}i ([join $atts ,]) 
	values (:[join $atts ,:])"
      
      db_exec_plsql make_live {
	select content_item__set_live_revision(:revision_id)
      }
      my update_main_table -revision_id $revision_id -form_vars $form_vars
    }
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
    {-with_subtypes:boolean true}
    {-count:boolean false}
  } {
    returns the SQL-query to select the CrItems of the specified object_type
    @select_attributes attributes for the sql query to be retrieved, in addion
      to ci.item_id acs_objects.object_type
    @param order_clause clause for ordering the solution set
    @return sql query
  } {
    my instvar object_type_key
    set attributes [list ci.item_id acs_objects.object_type] 
    foreach a $select_attributes {
      if {$a eq "title"} {set a cr.title}
      lappend attributes $a
    }
    set type_selection [expr {$with_subtypes ? 
	      "acs_object_types.tree_sortkey between \
               '$object_type_key' and tree_right('$object_type_key')" :
	      "acs_object_types.tree_sortkey = '$object_type_key'"}]
    set attribute_selection [expr {$count ? "count(*)" : [join $attributes ,]}]
    return "select $attribute_selection
    from acs_object_types, acs_objects, cr_items ci, cr_revisions cr 
        where $type_selection
        and acs_object_types.object_type = ci.content_type
        and ci.live_revision = cr.revision_id and
        acs_objects.object_id = cr.revision_id $order_clause"
  }

  #
  # Form template class
  #
  
  Class Form -parameter {
    fields 
    object_type
    {name {[namespace tail [self]]}}
    add_page_title
    edit_page_title
    {with_categories false}
  } -ad_doc {
    Class for the simplified generation of forms. This class was designed 
    together with the content repository class 
    <a href='/xotcl/show-object?object=::Generic::CrClass'>::Generic::CrClass</a>.
    This class can be parameterized with
    <ul>
    <li><b>fields:</b> form elements as described in 
       <a href='/api-doc/proc-view?proc=ad_form'>ad_form</a>.
    <li><b>object_type:</b> instance of
       <a href='/xotcl/show-object?object=::Generic::CrClass'>::Generic::CrClass</a>,
       used as a data source for this form
    <li><b>name:</b> of this form, used for naming the template, 
       defaults to the object name
    <li><b>add_page_title:</b> page title when adding content items
    <li><b>edit_page_title:</b> page title when editing content items
    </ul>
  }
  
  Form instproc init {} {
    if {![my exists add_page_title]} {
      my set add_page_title "Add [[my object_type] pretty_name]"
    }
    if {![my exists edit_page_title]} {
      my set edit_page_title "Edit [[my object_type] pretty_name]"
    }
    # check, if the specified fields are available from the data source
    # and ignore the unavailable entries
    set checked_fields [list]
    set available_atts [[my object_type] edit_atts]
    lappend available_atts [[my object_type] id_column] item_id
    foreach varspec [my fields] {
      set var [lindex [split [lindex $varspec 0] :] 0]
      if {[lsearch -exact $available_atts $var] == -1} continue
      lappend checked_fields $varspec
    }
    my fields $checked_fields
  }
  
  Form instproc form_vars {} {
    set vars [list]
    foreach varspec [my fields] {
      lappend vars [lindex [split [lindex $varspec 0] :] 0]
    }
    return $vars
  }
  Form instproc get_vars {object_type} {
    foreach var [my form_vars] {
      uplevel [list set $var [$object_type set $var]]
    }
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
    uplevel set $template [my name]
    
    ad_form -name [my name] -form [my fields] \
	-export [list [list object_type [my object_type]]] 

    set new_data [subst -novariables {[my object_type] add [self]}]
    set edit_data [subst -novariables {[my object_type] edit [self]}]
    set on_submit {}

    if {[my with_categories]} {
      upvar item_id item_id
      category::ad_form::add_widgets -form_name [my name] \
	  -container_object_id [ad_conn package_id] \
	  -categorized_object_id [value_if_exists item_id]
      append new_data {
	category::map_object -remove_old -object_id $item_id $category_ids
	db_dml insert_asc_named_object \
	    "insert into acs_named_objects (object_id,object_name,package_id) \
             values (:item_id, :title, :package_id)"
      }
      append edit_data {
	db_dml update_asc_named_object \
	    "update acs_named_objects set object_name = :title, \
		package_id = :package_id where object_id = :item_id"
	category::map_object -remove_old -object_id $item_id $category_ids
      }
      append on_submit {
	set category_ids [category::ad_form::get_categories \
			      -container_object_id $package_id]
      }
    }

    # action blocks must be added last
    ad_form -extend -name [my name] \
	-new_data $new_data -edit_data $edit_data -on_submit $on_submit \
	-new_request  [subst -novariables {
	  auth::require_login
	  permission::require_permission \
	      -object_id [ad_conn package_id] \
	      -privilege create
	  set page_title "[my add_page_title]"
	  set context \[list $page_title\]
	}] -edit_request [subst -novariables {
	  auth::require_login
	  permission::require_write_permission -object_id $item_id
	  [my object_type] get -item_id $item_id 
	  my get_vars [my object_type]
	  set page_title "[my edit_page_title]"
	  set context \[list $page_title\]
	}] -on_validation_error [subst -novariables {
	  set page_title "[my edit_page_title]"
	  set context \[list $page_title\]
	}] -after_submit {
	  ad_returnredirect "."
	  ad_script_abort
	}
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
    {delete_link delete}
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
	  [export_vars -base [my edit_link] {object_type}] \
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
      if {[lsearch -exact {item_id object_type EDIT DELETE} $e] == -1} {
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
	} $template instance_select [$object_type instance_select_query \
  	      -select_attributes $select_attributes \
              -with_subtypes $with_subtypes \
  	      -order_clause $order_clause] {
	set edit_url [export_vars -base [my edit_link] {item_id object_type}]
	set delete_url [export_vars -base [my delete_link] {item_id object_type}]
      }
  }
}
