::xo::library doc {
  Definition of a package manager for creating XOTcl package objects

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @creation-date 2007-09-24
  @cvs-id $Id$
}

namespace eval ::xo {
  #
  # Meta-Class for Application Package Classes
  #
  Class create ::xo::PackageMgr \
      -superclass ::xo::db::Class \
      -parameter {
        package_key
        {default_package_parameters ""}
        {default_package_parameter_page_info ""}
        {site_wide_package_parameters ""}
        {site_wide_package_parameter_page_info ""}
        {site_wide_pages ""}
      }

  PackageMgr ad_instproc first_instance {-privilege -party_id} {
    @return return first mounted instance of this type
  } {
    set package_key ${:package_key}
    set sql {
      select min(package_id)
      from apm_packages, site_nodes s
      where package_key = :package_key
        and s.object_id = package_id
    }
    if {[info exists privilege]} {
      append sql {
        and acs_permission.permission_p(package_id, :party_id, :privilege)
      }
    }
    return [::xo::dc get_value get_package_id $sql]
  }

  PackageMgr ad_instproc instances {{-include_unmounted false} {-closure false}} {
    @param include_unmounted include unmounted package instances
    @param closure include instances of subclasses of the package
    @return list of package_ids of xowiki instances
  } {
    set package_key ${:package_key}
    if {$include_unmounted} {
      set result [::xo::dc list get_xowiki_packages {select package_id \
                                                         from apm_packages where package_key = :package_key}]
    } else {
      set result [::xo::dc list get_mounted_packages {select package_id \
                                                          from apm_packages p, site_nodes s \
                                                          where package_key = :package_key \
                                                          and s.object_id = p.package_id}]
    }
    if {$closure} {
      foreach subclass [:info subclass] {
        foreach id [$subclass instances -include_unmounted $include_unmounted -closure true] {
          lappend result $id
        }
      }
    }
    return [lsort -integer $result]
  }

  PackageMgr instproc get_nls_language_from_lang {lang} {
    #
    # Return the first nls_language matching the provided lang
    # prefix. This method is not precise (when e.g. two nls_languages
    # are defined with the same lang), but the only thing relevant is
    # the lang anyhow.  If nothing matches return empty.
    #
    foreach nls_language [lang::system::get_locales] {
      if {[string range $nls_language 0 1] eq $lang} {
        return $nls_language
      }
    }
    return ""
  }


  PackageMgr instproc import_prototype_page {
    -package_key:required
    -name:required
    -parent_id:required
    -package_id:required
    {-lang en}
    {-add_revision:boolean true}
  } {
    set page ""
    set fn [acs_root_dir]/packages/$package_key/www/prototypes/$name.page
    #:log "--W check $fn"
    if {[file readable $fn]} {
      #
      # We have the file of the prototype page. We try to create
      # either a new item or a revision from definition in the file
      # system.
      #
      if {[regexp {^(..):(.*)$} $name _ lang local_name]} {
        set fullName $name
      } else {
        set fullName en:$name
      }
      :log "--sourcing page definition $fn, using name '$fullName'"
      set page [source $fn]
      $page configure \
          -name $fullName \
          -parent_id $parent_id \
          -package_id $package_id
      #
      # xowiki::File has a different interface for build-name to
      # derive the "name" from a file-name. This is not important for
      # prototype pages, so we skip it
      #
      if {![$page istype ::xowiki::File]} {
        set nls_language [:get_nls_language_from_lang $lang]
        $page name [$page build_name -nls_language $nls_language]
        #:log "--altering name of page $page to '[$page name]'"
        set fullName [$page name]
      }
      if {![$page exists title]} {
        $page set title $object
      }
      $page destroy_on_cleanup
      $page set_content [string trim [$page text] " \n"]
      $page initialize_loaded_object

      xo::Package require $package_id
      set p [::$package_id get_page_from_name -name $fullName -parent_id $parent_id]
      #:log "--get_page_from_name --> '$p'"
      if {$p eq ""} {
        #
        # We have to create the page new. The page is completed with
        # missing vars on save_new.
        #
        #:log "--save_new of $page class [$page info class]"
        $page save_new
      } else {
        #:log "--save revision $add_revision"
        if {$add_revision} {
          #
          # An old page exists already, create a revision.  Update the
          # existing page with all scalar variables from the prototype
          # page (which is just partial)
          #
          foreach v [$page info vars] {
            if {[$page array exists $v]} continue ;# don't copy arrays
            $p set $v [$page set $v]
          }
          #:log "--save of $p class [$p info class]"
          $p save
        }
        set page $p
      }
      if {$page ne ""} {
        # we want to be able to address the page via the canonical name ::$item_id
        set page [::xo::db::CrClass get_instance_from_db -item_id [$page item_id]]
      }
    }
    return $page
  }

  PackageMgr instproc require_site_wide_info {} {
    if {![info exists :site_wide_info]} {
      set cmd [list [self] configure_fresh_instance \
                   -parameter_page_info ${:site_wide_package_parameter_page_info} \
                   -parameters ${:site_wide_package_parameters} \
                  ]
      set site_wide_instance_id [acs_admin::require_site_wide_package \
                                     -package_key ${:package_key} \
                                     -configuration_command $cmd]
      ::xowiki::Package require $site_wide_instance_id
      #
      # During bootstrap, no xo::cc is available, but it seems to be
      # needed for instantiating prototype pages. So provide a best
      # effort initialization in such cases.
      #
      if {![nsf::is object ::xo::cc]} {
        ::xowiki::Package initialize -package_id $site_wide_instance_id -init_url false
      }
      dict set :site_wide_info folder_id [::$site_wide_instance_id folder_id]
      dict set :site_wide_info instance_id $site_wide_instance_id
    }
    return ${:site_wide_info}
  }

  PackageMgr instproc configure_fresh_instance {
    {-package_id:required}
    {-parameter_page_info ""}
    {-parameters ""}
  } {
    if {[llength $parameter_page_info] > 0} {
      ::xowiki::require_parameter_page \
          -package_id $package_id \
          -name [dict get $parameter_page_info name] \
          -title [dict get $parameter_page_info title] \
          -instance_attributes [dict get $parameter_page_info instance_attributes]
    }
    #
    # Configuring of the parameters is performed after the optional
    # configuration of the parameter page, since by setting the
    # package parameter "parameter_page" to a page that does not exist
    # yet, would lead to errors.
    #
    if {[llength $parameters] > 0} {
      foreach {parameter value} $parameters {
        ::parameter::set_value \
            -package_id $package_id \
            -parameter $parameter \
            -value $value
      }
    }
  }

  PackageMgr instproc require_site_wide_pages {
    {-refetch:boolean false}
    {-pages ""}
  } {
    #
    # If no pages are provided, take the default of the definition of
    # the package class.
    #
    if {$pages eq ""} {
      set pages ${:site_wide_pages}
    }
    set info [:require_site_wide_info]
    foreach n $pages {
      set item_id [::xo::db::CrClass lookup -name en:$n -parent_id [dict get $info folder_id]]
      #:log "lookup en:$n => $item_id"
      if {!$item_id || $refetch} {
        :log "require_site_wide_pages tries to load en:$n"
        set page [:import_prototype_page \
                      -name $n \
                      -package_key ${:package_key} \
                      -parent_id [dict get $info folder_id] \
                      -package_id [dict get $info instance_id] ]
        :log "Page en:$n loaded as '$page'"
      }
    }
  }

  PackageMgr instproc lookup_side_wide_page {-name:required} {
    set id [::xo::db::CrClass lookup \
                -name $name \
                -parent_id [dict get [:require_site_wide_info] folder_id]]
    #:log "lookup_side_wide_page <$name> uses [:require_site_wide_info] => $id"
    return $id
  }

  PackageMgr instproc get_site_wide_page {-name:required} {
    set item_id [:lookup_side_wide_page -name $name]
    # :ds "lookup from base objects $name => $item_id"
    if {$item_id} {
      set page [::xo::db::CrClass get_instance_from_db -item_id $item_id]
      set package_id [$page package_id]
      if {$package_id ne ""} {
        #$form set_resolve_context -package_id $package_id -parent_id $parent_id
        ::xo::Package require $package_id
      }

      return $page
    }
    return ""
  }




  PackageMgr ad_instproc initialize {
    -ad_doc
    {-parameter ""}
    {-package_id 0}
    {-url ""}
    {-user_id -1}
    {-actual_query " "}
    {-original_url_and_query}
    {-init_url true}
    {-keep_cc false}
    {-form_parameter}
    {-export_vars true}
  } {
    Create the connection context ::xo::cc and a package object
    if these are none defined yet. The connection context ::xo::cc
    and the package object will be destroyed on cleanup,
    when the global variables are reclaimed.

    As a side effect this method sets in the calling context
    the query parameters and package_id as variables, using the
    "defaults" for default values.

    init_url false requires the package_id to be specified and
    a call to Package instproc set_url to complete initialization.

    keep_cc true means that the original connection context
    is preserved (i.e. not altered) in case it exists already.
  } {
    #:msg "--i [self args], URL=$url, init_url=$init_url"

    if {[info exists ad_doc] && [api_page_documentation_mode_p]} {
      ad_parse_documentation_string $ad_doc doc_elements
      set doc_elements(query) $parameter
      error [array get doc_elements] "ad_page_contract documentation"
    }

    if {$url eq "" && $init_url} {
      set url [acs::root_of_host [ad_host]][ns_conn url]
      #:log "--CONN ns_conn url -> $url"
    }

    # get package_id from url in case it is not known
    set package_id [ConnectionContext require_package_id_from_url \
                        -package_id $package_id $url]

    # require connection context if needed
    ConnectionContext require \
        -keep_cc $keep_cc \
        -package_id $package_id -user_id $user_id \
        -parameter $parameter -url $url -actual_query $actual_query

    if {[info exists original_url_and_query]} {
      ::xo::cc original_url_and_query $original_url_and_query
    }

    if {[info exists form_parameter]} {
      ::xo::cc array set form_parameter $form_parameter
    }

    # create package object if necessary
    if {$keep_cc} {
      :require $package_id
    } else {
      :require -url $url $package_id
    }

    #
    # In case the login expired, we can force an early login to
    # prevent later login redirects, which can cause problems
    # from within catch operations. The package can decide, if
    # it want to force a refresh of the login, even if some pages
    # might not require the real user_id.
    #
    #:msg "force [::$package_id force_refresh_login] &&\
        #    [::xo::cc set untrusted_user_id] != [::xo::cc user_id]"
    if {[::$package_id force_refresh_login] &&
        [::xo::cc set untrusted_user_id] != [::xo::cc user_id]} {
      auth::require_login
    }

    if {$export_vars} {::xo::cc export_vars -level 2}
    return $package_id
  }

  PackageMgr ad_proc get_package_class_from_package_key {package_key} {
    Obtain the package class from a package key
  } {
    set key ::xo::package_class($package_key)
    if {[info exists $key]} {return [set $key]}

    foreach p [::xo::PackageMgr allinstances] {
      # Sanity check for old apps, having not set the package key.
      # TODO: remove this in future versions, when package_keys are enforced
      #if {![$p exists package_key]} {
      #  ns_log notice "!!! You should provide a package_key for $p [$p info class] !!!"
      #  continue
      #}
      if {[$p package_key] eq $package_key} {
        return [set $key $p]
      }
    }

    return ""
  }

  PackageMgr ad_instproc require {{-url ""} package_id} {
    Create package object if needed.
  } {
    if {$package_id eq ""} {
      #::xo::show_stack
      error "package_id must not be empty"
    }

    #:log "--R $package_id exists? [nsf::is object ::$package_id] url='$url'"

    if {![nsf::is object ::$package_id]} {
      #:log "--R we have to create ::$package_id //url='$url'"
      #
      # To make initialization code generic, we obtain from the
      # package_id the class of the package.
      #
      set package_key [apm_package_key_from_id $package_id]
      set package_class [[self class] get_package_class_from_package_key $package_key]
      if {$package_class eq ""} {
        #
        # For some unknown reason, we did not find the key.  We want
        # to be conservative, behave like in older versions that did
        # not provide a package_key, but required for this call to be
        # invoked on the actual class of the package. We provide
        # compatibility, but complain in ns_log.
        #
        # (E.g. hypermail2xowiki uses this)
        ns_log warning "Could not find ::xo::Package with key $package_key ($package_id)"
        set package_class [self]
      }

      if {$url ne ""} {
        $package_class create ::$package_id -destroy_on_cleanup -id $package_id -url $url
      } else {
        $package_class create ::$package_id -destroy_on_cleanup -id $package_id
      }
    } else {
      if {$url ne ""} {
        ::$package_id set_url -url $url
      }
    }
  }

  #
  # generic Package class
  #
  # get apm_package class  #### missing in acs_attributes: instance_name, default_locale
  #::xo::db::Class get_class_from_db -object_type apm_package

  #ns_log notice [::xo::db::apm_package serialize]
  #ns_log notice =======================================

  PackageMgr create ::xo::Package \
      -superclass ::xo::db::Object \
      -table_name apm_packages -id_column package_id \
      -object_type apm_package -package_key apm_package \
      -slots {
        ::xo::db::Attribute create package_key -datatype string -sqltype varchar(100)
        ::xo::db::Attribute create instance_name -datatype string -sqltype varchar(300)
        ::xo::db::Attribute create default_locale -datatype string -sqltype varchar(30)
      } \
      -parameter {
        id
        url
        {context ::xo::cc}
        package_url
        {force_refresh_login false}
      }

  ::xo::Package instforward query_parameter        {%set :context} %proc
  ::xo::Package instforward exists_query_parameter {%set :context} %proc
  ::xo::Package instforward form_parameter         {%set :context} %proc
  ::xo::Package instforward exists_form_parameter  {%set :context} %proc
  ::xo::Package instforward returnredirect         {%set :context} %proc

  ::xo::Package instproc get_parameter {attribute {default ""}} {
    set package_id ${:id}
    set parameter_obj [::xo::parameter get_parameter_object \
                           -parameter_name $attribute \
                           -package_id $package_id \
                           -retry false]
    set success 0

    if {$parameter_obj ne "" && [$parameter_obj set scope] ne "global"} {
      set value [$parameter_obj get -package_id $package_id]
      #ns_log notice "core: get_param for $attribute after GET: [$parameter_obj serialize] -> '$value'"
      #if {$value ne "" || [$parameter_obj set __success]} {return $value}
      #
      # The returned '$value' might be a value set for the actual
      # package instance, or the default for the package_parameter as
      # defined by the package parameter definition in the XML file. If
      # the value was not specified explicitly, and the provided
      # default for this command is not empty, return the provided
      # default.
      #
      if {![$parameter_obj set __success] && $value eq "" && $default ne ""} {
        return $default
      } else {
        return $value
      }
    }
    return [parameter::get_global_value \
                   -package_key ${:package_key} \
                   -parameter $attribute \
                   -default $default]
  }

  ::xo::Package instproc init args {
    set id ${:id}
    set package_url [lindex [site_node::get_url_from_object_id -object_id $id] 0]
    #:log "--R creating package_url='$package_url'"
    if {$package_url ne ""} {
      array set info [site_node::get -url $package_url]
      #set package_url $info(url)
      :package_key $info(package_key)
      :instance_name $info(instance_name)
    } else {
      ::xo::dc 1row package_info {
        select package_key, instance_name from apm_packages where package_id = :id
      }
      :package_key $package_key
      :instance_name $instance_name
    }

    if {[ns_conn isconnected]} {
      # in case of host-node map, simplify the url to avoid redirects
      # .... but ad_host works only, when we are connected....
      # TODO: solution for syndication
      set root [acs::root_of_host [ad_host]]
      regexp "^${root}(.*)$" $package_url _ package_url
    }
    #:log "--R package_url= $package_url (was $info(url))"
    :package_url $package_url

    if {[info exists :url] && [info exists root]} {
      regexp "^${root}(.*)$" ${:url} _ :url
    } elseif {![info exists :url]} {
      #:log "--R we have no url, use package_url '$package_url'"
      # if we have no more information, we use the package_url as actual url
      set :url $package_url
    }
    :set_url -url ${:url}
    set :mime_type text/html
    set :delivery ns_return
    set target_class ::${:package_key}::Package
    if {[:info class] ne $target_class && [:isclass $target_class]} {
      :class $target_class
    }

    #
    # Save the relation between class and package_key for fast lookup
    #
    set ::xo::package_class(${:package_key}) [:info class]

    :initialize
  }

  ::xo::Package instproc initialize {} {
    # empty hook for user level initialization
  }

  Package ad_instproc require_root_folder {
    {-parent_id -100}
    {-content_types {}}
    -name:required
  } {

    Make sure, the root folder for the given package exists. If not,
    create it and register all allowed content types.

    Note that xowiki (and derived packages) define their own version
    of "require_root_folder" based on form pages. Therefore, this
    function is just for packages not based on xowiki.

    @return folder_id
  } {
    set folder_id [::xo::xotcl_package_cache eval root_folder-${:id} {

      set folder_id [::xo::db::CrClass lookup -name $name -parent_id $parent_id]
      if {$folder_id == 0} {
        :log "folder with name '$name' and parent $parent_id does NOT EXIST"
        set folder_id [::xo::db::sql::content_folder new \
                           -name $name \
                           -label ${:instance_name} \
                           -parent_id $parent_id \
                           -package_id ${:id} \
                           -context_id ${:id}]
        :log "CREATED folder '$name' and parent $parent_id ==> $folder_id"
      }

      # Register all specified content types
      ::xo::db::CrFolder register_content_types \
          -folder_id $folder_id \
          -content_types $content_types
      #:log "returning from cache folder_id $folder_id"
      return $folder_id
    }]
    #:log "returning from require folder_id $folder_id"
    return $folder_id
  }

  ::xo::Package instproc set_url {-url} {
    :url $url
    set :object [string range [:url] [string length [:package_url]] end]
    #:msg "--R object set to ${:object}, url=$url, [:serialize]"
  }

  ::xo::Package instproc handle_http_caching {} {
    #
    # Subpackages can overload this method for realizing
    #
    # a) package specific caching policies
    # b) page-type specific caching policies
    # c) page specific caching policies
    #
    # Items (b) and (c) can e realized via the instance variable of
    # the package object named "invoke_object", which is set for
    # non-error cases in e.g. xowiki.
    #
    ns_set put [ns_conn outputheaders] "Cache-Control" \
        "max-age=0, no-cache, no-store"
    #
  }

  ::xo::Package instproc reply_to_user {text} {

    :handle_http_caching

    #:log "REPLY [::xo::cc exists __continuation]"
    if {[::xo::cc exists __continuation]} {
      #:log "REPLY [::xo::cc set __continuation]"
      eval [::xo::cc set __continuation]
    } else {
      if {[string length $text] > 1} {
        set default_status_code 200
      } else {
        set default_status_code 204
      }
      set status_code [expr {[::xo::cc exists status_code] ? [::xo::cc set status_code] : $default_status_code}]
      #:log "REPLY ${:delivery} $status_code ${:mime_type} - length: [string length $text] - [ad_conn peeraddr]"
      ${:delivery} $status_code ${:mime_type} $text
    }
  }

  ::xo::Package instproc return_page {-adp:required -variables -form} {
    #:log "--vars=[self args]"
    set __vars [list]
    foreach _var $variables {
      if {[llength $_var] == 2} {
        #
        # The variable specification "$_var" is a pair of name and
        # value.
        #
        lappend __vars [lindex $_var 0] [uplevel subst [lindex $_var 1]]
      } else {
        #
        # We have just a variable name, provide a linked variable to
        # access the value.
        #
        set localvar local.$_var
        upvar $_var $localvar
        if {[array exists $localvar]} {
          lappend __vars &$_var $localvar
        } elseif {[info exists $localvar]} {
          # ignore undefined variables
          lappend __vars $_var [set $localvar]
        }
      }
    }

    if {[info exists form]} {
      set level [template::adp_level]
      foreach f [uplevel #$level info vars ${form}:*] {
        lappend __vars &$f $f
        upvar #$level $f $f
      }
    }
    #
    # Substitute the template with the themed template
    #
    set adp [template::themed_template $adp]

    set text [template::adp_include $adp $__vars]
    if { [lang::util::translator_mode_p] } {
      set text [::xo::localize $text 1]
    }
    #:log "--after adp $text"

    return [::xo::remove_escapes $text]
  }


  #ns_log notice [::xo::Package serialize]

}
::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
