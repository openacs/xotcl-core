xo::library doc {
  A simple OO interface for ad_form for
  acs_objects and content repository items.

  @author Gustaf Neumann
  @creation-date 2005-08-13
  @cvs-id $Id$
}

namespace eval ::Generic {
  #
  # Form template class
  #
  Class create Form -parameter {
    fields
    data
    {package_id ""}
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
    <p>This class was designed
    together with the content repository class
    <a href='/xotcl/show-object?object=::xo::db::CrClass'>::xo::db::CrClass</a>
    for the generation of HTML forms, but it can be used also with different classes.
    The only hard requirement is the  presence of an 'item_id' form field.
    For generic acs_objects, 'item_id' will correspond to 'object_id' column in 'acs_objects'
    table. For content repository items, 'item_id' will be the column by the same name in
    cr_revisions/cr_items.

    @param add_page_title  page title when adding content items
    @param edit_page_title page title when editing content items
    @param data            data object (e.g. instance if CrItem)
    @param folder_id       associated folder id. Will default to data's 'parent_id' variable.
                           If 'parent_id' is missing too, package's 'folder_id' will be used.
    @param name            name of the form, used for naming the template, defaults to the object name
    @param package_id      package_id of the object. Will default to data's 'package_id' variable
    @param submit_link     link for page after submit
    @param with_categories display form with categories (default false)
    </ul>
  }

  Form instproc init {} {
    set level [template::adp_level]
    :forward var uplevel #$level set

    if {${:package_id} eq ""} {set :package_id [${:data} package_id]}
    if {${:folder_id} < 0} {
      set :folder_id [expr {[${:data} exists parent_id] ? [${:data} parent_id] : [${:package_id} folder_id]}]
    }

    set class [${:data} info class]
    set :data_id [$class id_column]

    if {![info exists :add_page_title]} {
      set :add_page_title [_ xotcl-core.create_new_type \
                                 [list type [$class pretty_name]]]
    }
    if {![info exists :edit_page_title]} {
      set :edit_page_title [_ xotcl-core.edit_type \
                                  [list type [$class pretty_name]]]
    }

    if {![info exists :fields]} {my mkFields}
    #my log --fields=[:fields]
  }

  Form instproc form_vars {} {
    set vars {}
    foreach varspec [:fields] {
      lappend vars [lindex [split [lindex $varspec 0] :] 0]
    }
    return $vars
  }
  Form instproc get_id_field {} {
    if {[${:data} istype ::xo::db::CrItem]} {
      return item_id
    }
    return object_id
  }
  Form instproc new_data {} {
    #:log "--- new_data ---"
    ${:data} save_new
    return [${:data} set [:get_id_field]]
  }
  Form instproc edit_data {} {
    #:log "--- edit_data --- setting form vars=[:form_vars]"
    ${:data} save
    # Renaming is meant for cr_items and such
    if {[${:data} istype ::xo::db::CrItem]} {
      set old_name [::xo::cc form_parameter __object_name ""]
      set new_name [${:data} set name]
      if {$old_name ne $new_name} {
        #
        # The item was renamed.
        #
        #my log "--- rename from $old_name to $new_name"
        ${:data} rename -old_name $old_name -new_name $new_name
        #
        # Check, whether we have to change the redirect url due to
        # renaming. When the method returns non-empty use this value.
        #
        set url [${:data} changed_redirect_url]
        if {$url ne ""} {
          :submit_link $url
        }
      }
    }
    return [${:data} set [:get_id_field]]
  }

  Form instproc request {privilege} {
    if {[:isobject ::${:package_id}] && ![::${:package_id} exists policy]} {
      # not needed, if governed by a policy
      auth::require_login
      permission::require_permission \
          -object_id ${:package_id} \
          -privilege $privilege
    }
    set :edit_form_page_title [if {$privilege eq "create"} \
                                  {my add_page_title} {my edit_page_title}]

    set :context [list ${:edit_form_page_title}]
  }

  Form instproc set_form_data {} {
    foreach var [${:data} info vars] {
      if {![${:data} array exists $var]} {
        :var $var [list [${:data} set $var]]
      }
    }
    # Alias object_id to the id of our object
    if {[${:data} exists ${:data_id}]} {
      ${:data} set object_id [${:data} set ${:data_id}]
    }
  }

  Form instproc new_request {} {
    #:log "--- new_request ---"
    :request create
    :set_form_data
  }
  Form instproc edit_request {item_id} {
    #:log "--- edit_request ---"
    :request write
    :set_form_data
  }

  Form instproc on_submit {item_id} {
    # On redirects after a submit to the same page, ensure
    # the setting of edit_form_page_title and context
    :request write
    # Put form content into data object
    foreach __var [:form_vars] {
      ${:data} set $__var [:var $__var]
    }
    ${:data} initialize_loaded_object
  }

  Form instproc on_validation_error {} {
    #my log "-- "
    set :edit_form_page_title [:edit_page_title]
    set :context [list ${:edit_form_page_title}]
  }
  
  Form instproc after_submit {item_id} {
    set link [:submit_link]
    if {$link eq "view"} {
      set link [export_vars -base $link {item_id}]
    }
    #:log "-- redirect to $link // [string match "*\?*" $link]"
    #
    # We we use here the classical idiom
    #      ad_returnredirect
    #      ad_script_abort
    #
    # For the old-style wiki-forms we should use
    #      $package_id returnredirect $return_url
    #
    # maybe via proving a Form->redirect method to be able to overload
    # it for wiki forms, but I am not sure, we should touch this
    # rather outdated code (FormPages are the preferred method in
    # xowiki).
    #
    ad_returnredirect $link
    ad_script_abort
  }

  Form ad_instproc generate {
    {-template "formTemplate"}
    {-mode "edit"}
    {-export}
  } {
    The method generate is used to actually generate the form template
    from the specifications and to set up page_title and context
    when appropriate.
    @param template is the name of the Tcl variable to contain the filled in template
    @param export list of attribute value pairs to be exported to the form (nested list)
  } {
    # set form name for adp file
    set :$template [:name]

    set object_type [[${:data} info class] object_type]
    set object_name [expr {[${:data} exists name] ? [${:data} set name] : ""}]
    #my log "-- ${:data}, cl=[${:data} info class] [[${:data} info class] object_type]"

    #my log "--e [:name] final fields [:fields]"
    set exports [list \
      [list object_type $object_type] \
                     [list folder_id ${:folder_id}] \
                     [list __object_name $object_name]]
    if {[info exists export]} {
      foreach pair $export {lappend exports $pair}
    }

    ad_form -name [:name] -form [:fields] -mode $mode \
        -export $exports -action [:action] -html [:html]

    set new_data            "set item_id \[[self] new_data\]"
    set edit_data           "set item_id \[[self] edit_data\]"
    set new_request         "[self] new_request"
    set edit_request        "[self] edit_request \$item_id"
    set after_submit        "[self] after_submit \$item_id"
    set on_validation_error "[self] on_validation_error"
    set on_submit           "[self] on_submit \$item_id"

    if {[:with_categories]} {
      set coid [expr {[${:data} exists item_id] ? [${:data} set item_id] : ""}]
      category::ad_form::add_widgets -form_name [:name] \
          -container_object_id ${:package_id} \
          -categorized_object_id $coid

      # When editing, fill category form widgets
      # with current mappings for this object
      append edit_request {
        category::ad_form::fill_widgets \
            -container_object_id ${:package_id} \
            -categorized_object_id $item_id
      }
      append new_data {
        category::map_object -remove_old -object_id $item_id $category_ids
      }
      append edit_data {
        category::map_object -remove_old -object_id $item_id $category_ids
      }
      append on_submit {
        set category_ids [category::ad_form::get_categories \
                              -container_object_id ${:package_id}]
      }
    }
    #ns_log notice "-- ad_form new_data=<$new_data> edit_data=<$edit_data> edit_request=<$edit_request>"

    # action blocks must be added last
    # -new_data and -edit_data are enclosed in a transaction only in the end,
    # so eventual additional code from category management is executed safely
    ad_form -extend -name [:name] \
        -validate [:validate] \
        -new_data "xo::dc transaction \{ $new_data \}" -edit_data "xo::dc transaction \{ $edit_data \}" \
        -on_submit $on_submit -new_request $new_request -edit_request $edit_request \
        -on_validation_error $on_validation_error -after_submit $after_submit
  }


  #
  # List template class
  #
  Class create List -parameter {
    {actions ""}
    {name {[namespace tail [self]]}}
    {bulk_actions ""}
    {bulk_action_method "post"}
    {bulk_action_export_vars ""}
    elements
    {filters ""}
    {formats ""}
    {selected_format ""}
    {rows_per_page 30}
    {page 1}
    {orderby ""}
    {page_groupsize 10}
    {row_code ""}
    class
    {create_url ""}
    {edit_url   ""}
    {edit_template {<img src="/resources/acs-subsite/Edit16.gif" width="16" height="16" border="0">}}
    {delete_url ""}
    {delete_template {<img src="/resources/acs-subsite/Delete16.gif" width="16" height="16" border="0">}}
    {no_create_p f}
    {no_edit_p   f}
    {no_delete_p f}
    {package_id ""}
    {ulevel 1}
    {pass_properties ""}
    {checkbox_name ""}
    {orderby_name ""}
    {no_data ""}
    {html_main_class ""}
    {html_sub_class ""}
    {html_class ""}
    {html ""}
    {caption ""}
    {bulk_action_click_function ""}
  } -ad_doc {

    Simple OO interface to template::list.
    This class has been built to allow quick creation of list UIs for generic acs_objects.<br/>
    <br/>
    Many parameters are homonimous to those for <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a><br/>
    and work in the same way, unless stated differently in this documentation.<br/>
    Despite the high number of object's members, most of them are there for backward compatibility with the procedural API
    and they seldom need to be specified.<br/>
    <br/>
    An example of instantiation could just look as this:<br/>
    <pre>
    # Must be an existing acs_object class on the system.
    set class "::dev::Location"

    # As we are talking about acs_objects, our 'delete'
    # page could of course be the same for every object
    # in the system.
    ::Generic::List create list1 \
        -class $class \
        -package_id $package_id \
        -rows_per_page $rows_per_page \
        -delete_url "../delete" \
        -elements {
          name {
            label "Name"
          }
          street {
            label "Street"
          }
          number {
            label "Number"
          }
          city {
            label "City"
          }
          region {
            label "Region"
          }
          country {
            label "Country"
          }
          coords {
            label "Coordinates"
          }
        } -orderby {
          default_value name
          name {
            label "Name"
            orderby_desc "name desc"
            orderby_asc "name asc"
          }
        } -row_code {
          set coords "$latitude $longitude"
        }

    list1 generate
    </pre>
    ...while the ADP template would include this:

    <pre>
    &lt;listtemplate name="list1"&gt;&lt;/listtemplate&gt;
    </pre>

    Notice that in this case we didn't have to specify queries,
    nor populate any multirow by hand: they have come directly
    from class's data-model. A list built in this way will be
    paginated automatically.

    @parameter actions Behaves as in <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>.
    If missing, can be automatically generated acting on <code>create_url</code> and <code>no_create_p</code>
    parameters (see below).

    @param bulk_action_method Behaves as in <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>,
    but will default to POST method, as it is safer with respect to possible high number of query parameters.

    @param elements Behaves as in <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>.
    It must be possible to build every element either through class's instance members, or programmatically
    (see <code>row_code</code> below).

    @param rows_per_page Behaves as <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>'s
    <code>page_size</code> parameter. Pagination is automatical for this class. To turn it off, just
    set this parameter to "" .

    @param row_code This snippet will be executed for every instance/row in the list, so is similar in spirit to
    <code>code_block</code> argument for <code>db_multirow</code>. This will allow the user to
    build programmatically other elements outside object's data model, override edit and delete
    url and so on. Code will have access to every variable in the caller scope and to each instance's
    variable.

    @param class Is the class (descendant of acs_object) for which this list will be built.

    @param no_create_p Tells to the list we don't want instance creation action button to be built automatically.

    @param create_url When instance creation url is automatically built, tells the list to which url make it point.

    @param no_edit_p Tells to the list we don't want instance edit action button to be built automatically.

    @param edit_url When instance edit element is automatically built, tells the list to which url make it point.
    Page pointed must accept an <code>item_id</code> parameter, that will be the primary key of
    edited instance.

    @param edit_template When instance edit element is automatically built, use this template to build the element.

    @param no_delete_p Tells to the list we don't want instance delete action button to be built automatically.

    @param delete_url When instance delete url is automatically built, tells the list to which url make it point.
    Page pointed must accept an <code>item_id</code> parameter, that will be the primary key of
    deleted instance.

    @param delete_template When instance delete element is automatically built, use this template to build the element.

    @param package_id Is the package for this instance. It has no use for now.

    @param html_class Behaves as <code>class</code> parameter in
    <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>.

    @param html_main_class Behaves as <code>main_class</code> parameter in
    <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>.

    @param html_sub_class Behaves as <code>sub_class</code> parameter in
    <a href='/api-doc/proc-view?proc=template::list::create'>template::list::create</a>.

    @author Antonio Pisano (antonio@elettrotecnica.it)

  }

  List instproc init {} {
    set :id_column     [${:class} id_column]
    set :pretty_name   [${:class} pretty_name]
    set :pretty_plural [${:class} pretty_plural]
    set :list_name     ${:name}
  }

  List instproc get_actions {} {
    if {[string is false ${:no_create_p}]} {
      set type ${:pretty_name}
      if {${:create_url} eq ""} {set create_url add-edit}
      set create_action [list \
                             [_ xotcl-core.create_new_type] ${:create_url} [_ xotcl-core.create_new_type]]
      set :actions [concat $create_action ${:actions}]
    }
    return ${:actions}
  }

  List instproc get_elements {} {
    set elements {}
    # build the edit button
    if {!${:no_edit_p}} {
      set type ${:pretty_name}
      set title [_ xotcl-core.edit_type]
      lappend elements \
          edit [list \
                    link_url_col edit_url \
                    display_template ${:edit_template} \
                    link_html [list title $title] \
                    sub_class narrow]
    }
    # edit button will be the first list element,
    # in between there will be user's elements,
    # delete button will be last
    set elements [concat $elements ${:elements}]
    # build delete button
    if {!${:no_delete_p}} {
      set title [_ xotcl-core.delete_item]
      lappend elements \
          delete [list \
                      link_url_col delete_url \
                      link_html [list title $title class acs-confirm] \
                      display_template ${:delete_template} \
                      sub_class narrow]
    }
    return $elements
  }

  List instproc page_query {} {
    if {${:orderby} ne ""} {
      set orderby {:orderby}
      return [${:class} instance_select_query \
                  -select_attributes [list ${:id_column}] \
                  -where_clause "\[template::list::filter_where_clauses -name ${:list_name} -and\]" \
                  -orderby "\[lrange \[template::list::orderby_clause -name ${:list_name} -orderby\] 2 end\]"]
    } else {
      return [${:class} instance_select_query \
                  -select_attributes [list ${:id_column}] \
                  -where_clause "\[template::list::filter_where_clauses -name ${:list_name} -and\]"]
    }
  }

  List instproc get_filters {} {
    if {${:rows_per_page} ne "" &&
        "rows_per_page" ni ${:filters}} {
      set opts {}
      set opt [expr {int(${:rows_per_page} / 2)}]
      for {set i 0} {$i < 3} {incr i} {
        lappend opts [list $opt $opt]
        set opt [expr {$opt*($i+2)}]
      }
            append :filters "
        rows_per_page {
          label \"[_ acs-templating.Page_Size]\"
          values {$opts}
          default_value ${:rows_per_page}
        }"
    }
    set ulevel [expr {${:ulevel} + 1}]
    set :filters [uplevel $ulevel [list subst ${:filters}]]
    return ${:filters}
  }

  List instproc extend_cols {} {
    set cols {}
    set specs {}
    foreach {el spec} [:get_elements] {
      lappend cols $el
      foreach {prop val} $spec {
        if {$prop in
            {display_col
              link_url_col}} {
          lappend cols $val}
      }}; return $cols
  }

  List instproc get_ids {} {
    if {${:rows_per_page} ne ""} {
      return [template::list::page_get_ids -name ${:list_name} -tcl_list]
    }
    # If we are not paginating, just get all ids in table
    return [::xo::dc list query [subst [:page_query]]]
  }

  List instproc multirow {} {
    :instvar list_name id_column no_edit_p {edit_url base_edit_url} no_delete_p {delete_url base_delete_url} row_code
    set ulevel [expr {${:ulevel} + 1}]
    if {$base_edit_url   eq ""} {set base_edit_url "add-edit"}
    if {$base_delete_url eq ""} {set base_delete_url "delete"}
    set this_url [::xo::cc url]
    set extend_cols [:extend_cols]
    # Create the multirow
    {*}"template::multirow create $list_name $extend_cols"
    set multirow_append "template::multirow append $list_name"
    foreach col $extend_cols {lappend multirow_append "\$$col"}
    # Loop through objects in this page...
    foreach item_id [:get_ids] {
      # ...get the object
      set o [::xo::db::Class get_instance_from_db -id $item_id]
      set obj_vars [$o info vars]
      {*}"$o instvar $obj_vars"
      set item_id [set $id_column]
      if {!$no_edit_p} {
        set edit_url [export_vars -base $base_edit_url {item_id}]
      }
      if {!$no_delete_p} {
        set delete_url [export_vars -base $base_delete_url {item_id {return_url $this_url}}]
      }
      # ensure object variables exist and
      # bring them to the caller scope
      set upvars [lsort -unique [concat $extend_cols $obj_vars]]
      foreach col $upvars {
        if {![info exists $col]} {set $col ""}
        uplevel $ulevel [list set $col [set $col]]
      }
      if {$row_code ne ""} {
        uplevel $ulevel $row_code
      }
      {*}[uplevel $ulevel [list subst $multirow_append]]
      # Need to clear the area or code block could suffer
      # variable pollution from leftovers
      {*}"unset $upvars"
    }
  }

  List instproc generate {} {
    set cmd [list \
                 template::list::create \
                 -ulevel [expr {${:ulevel}+1}] \
                 -name ${:list_name} \
                 -multirow ${:list_name} \
                 -actions [:get_actions] \
                 -elements [:get_elements] \
                 -filters [:get_filters] \
                 -orderby ${:orderby}]
    if {${:bulk_actions} ne ""} {
      lappend cmd \
          -bulk_actions ${:bulk_actions} \
          -bulk_action_method ${:bulk_action_method} \
          -bulk_action_export_vars ${:bulk_action_export_vars} \
          -key ${:id_column}
    }
    if {${:formats} ne ""} {
      lappend cmd \
          -formats ${:formats} \
          -selected_format ${:selected_format}
    }
    if {${:rows_per_page} ne ""} {
      lappend cmd \
          -page_flush_p t \
          -page_size ${:rows_per_page} \
          -page_groupsize ${:page_groupsize} \
          -page_query [:page_query]
    }
    lappend cmd \
        -row_pretty_plural ${:pretty_plural}
    # These properties will be passed as they are
    foreach prop {
      pass_properties
      checkbox_name
      orderby_name
      no_data
      caption
      bulk_action_click_function
      html
    } {
      set val [set :$prop]
      if {$val ne ""} {
        lappend cmd -${prop} $val
      }
    }
    foreach prop {
      html_main_class
      html_sub_class
      html_class
    } {
      set val [set :$prop]
      set prop [string range $prop 5 end]
      if {$val ne ""} {
        lappend cmd -${prop} $val
      }
    }
    {*}$cmd
    :multirow

    # Don't put handlers directly on the HTML, but rather define them in JavaScript afterwards
    template::add_confirm_handler -CSSclass acs-confirm -message [_ acs-subsite.Delete]?
  }

  List instproc to_csv {} {
    template::list::write_csv -name ${:list_name}
  }
}
namespace import -force ::Generic::*
::xo::library source_dependent

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
