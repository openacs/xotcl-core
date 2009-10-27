ad_library {
  A simple OO interface for ad_form for content repository items.

  @author Gustaf Neumann
  @creation-date 2005-08-13
  @cvs-id $Id$
}

namespace eval ::Generic {
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
    <a href='/xotcl/show-object?object=::xo::db::CrClass'>::xo::db::CrClass</a>.

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
    set package_id [$data package_id]
    set folder_id [expr {[$data exists parent_id] ? [$data parent_id] : [$package_id folder_id]}]
    set class     [$data info class]

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
    set available_atts [$class array names db_slot]
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
        #my msg "rename from $old_name to $new_name"
        $data rename -old_name $old_name -new_name $new_name
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
    set edit_form_page_title [if {$privilege eq "create"} \
		 {my add_page_title} {my edit_page_title}]

    set context [list $edit_form_page_title]
  }

  Form instproc set_form_data {} {
    my instvar data
    foreach var [[$data info class] array names db_slot] {
      if {[$data exists $var]} {
        my var $var [list [$data set $var]]
      }
    }
  }

  Form instproc new_request {} {
    #my log "--- new_request ---"
    my request create
    my set_form_data
  }
  Form instproc edit_request {item_id} {
    #my log "--- edit_request ---"
    my request write
    my set_form_data
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
    #ns_log notice "-- redirect to $link // [string match *\?* $link]"
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
    if {[catch {set object_name [$data set name]}]} {set object_name ""}
    #my log "-- $data, cl=[$data info class] [[$data info class] object_type]"
    
    #my log "--e [my name] final fields [my fields]"
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
}
namespace import -force ::Generic::*



