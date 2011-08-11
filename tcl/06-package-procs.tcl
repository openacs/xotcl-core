ad_library {
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
      }

  PackageMgr ad_instproc first_instance {-privilege -party_id} {
    @return return first mounted instance of this type
  } {
    my instvar package_key
    if {[info exists privilege]} {
      set sql [::xo::db::sql select -vars package_id \
                   -from "apm_packages, acs_object_party_privilege_map ppm, site_nodes s" \
                   -where {
                     package_key = :package_key 
                     and s.object_id = package_id 
                     and ppm.object_id = package_id 
                     and ppm.party_id = :party_id
                     and ppm.privilege = :privilege
                   } -limit 1]
      db_string [my qn get_package_id] $sql
    } else {
      ::xo::parameter get_package_id_from_package_key -package_key $package_key
    }
  }

  PackageMgr ad_instproc instances {{-include_unmounted false} {-closure false}} {
    @param include_unmounted include unmounted package instances
    @param closure include instances of subclasses of the package 
    @return list of package_ids of xowiki instances
  } {
    my instvar package_key
    if {$include_unmounted} {
      set result [db_list [my qn get_xowiki_packages] {select package_id \
        from apm_packages where package_key = :package_key}]
    } else {
      set result [db_list [my qn get_mounted_packages] {select package_id \
        from apm_packages p, site_nodes s  \
        where package_key = :package_key and s.object_id = p.package_id}]
    }
    if {$closure} {
      foreach subclass [my info subclass] {
	foreach id [$subclass instances -include_unmounted $include_unmounted -closure true] {
	  lappend result $id
	}
      }
    }
    return [lsort -integer $result]
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
    #my msg "--i [self args], URL=$url, init_url=$init_url"

    if {$url eq "" && $init_url} {
      set url [root_of_host [ad_host]][ns_conn url]
      #my log "--CONN ns_conn url -> $url"
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
      my require $package_id
    } else {
      my require -url $url $package_id
    }
      
    #
    # In case the login expired, we can force an early login to
    # prevent later login redirects, which can cause problems
    # from within catch operations. The package can decide, if
    # it want to force a refresh of the login, even if some pages 
    # might not require the real user_id.
    #
    #my msg "force [$package_id force_refresh_login] &&\
    #	[::xo::cc set untrusted_user_id] != [::xo::cc user_id]"
    if {[$package_id force_refresh_login] && 
	[::xo::cc set untrusted_user_id] != [::xo::cc user_id]} {
      auth::require_login
    }

    if {$export_vars} {::xo::cc export_vars -level 2}
    return $package_id
  }

  PackageMgr ad_proc get_package_class_from_package_key {package_key} {
    Obtain the package class from a package key
  } {
    foreach p [::xo::PackageMgr allinstances] {
      # Sanity check for old apps, having not set the package key.
      # TODO: remove this in future versions, when package_keys are enforced
      if {![$p exists package_key]} {
        ns_log notice "!!! You should provide a package_key for $p [$p info class] !!!"
        continue
      }
      if {[$p package_key] eq $package_key} {
        return $p
      }
    }
    return ""
  }

  PackageMgr ad_instproc require {{-url ""} package_id} {
    Create package object if needed.
  } {
    if {$package_id eq ""} {error "package_id must not be empty"}

    #my log "--R $package_id exists? [my isobject ::$package_id] url='$url'"

    if {![my isobject ::$package_id]} {
      #my log "--R we have to create ::$package_id //url='$url'"
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
        ns_log notice "Could not find ::xo::Package with key $package_key ($package_id)"
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

  ::xo::Package instforward query_parameter        {%my set context} %proc
  ::xo::Package instforward exists_query_parameter {%my set context} %proc
  ::xo::Package instforward form_parameter         {%my set context} %proc
  ::xo::Package instforward exists_form_parameter  {%my set context} %proc
  ::xo::Package instforward returnredirect         {%my set context} %proc

  ::xo::Package instproc get_parameter {attribute {default ""}} {
    set param [::xo::parameter get \
                   -parameter $attribute \
                   -package_id [my id] \
                   -default $default \
                   -retry false]
    #my log "--get_parameter <$attribute> <$default> returned <$param>"
    return $param
  }
 
  ::xo::Package instproc init args {
    my instvar id url
    set package_url [lindex [site_node::get_url_from_object_id -object_id $id] 0]
    #my log "--R creating package_url='$package_url'"
    if {$package_url ne ""} {
      array set info [site_node::get -url $package_url]
      #set package_url $info(url)
      my package_key $info(package_key)
      my instance_name $info(instance_name)
    } else {
      db_1row [my qn package_info] {
        select package_key, instance_name from apm_packages where package_id = :id
      }
      my package_key $package_key
      my instance_name $instance_name
    }
    if {[ns_conn isconnected]} {
      # in case of of host-node map, simplify the url to avoid redirects
      # .... but ad_host works only, when we are connected.... 
      # TODO: solution for syndication
      set root [root_of_host [ad_host]]
      regexp "^${root}(.*)$" $package_url _ package_url
    }
    #my log "--R package_url= $package_url (was $info(url))"
    my package_url $package_url

    if {[my exists url] && [info exists root]} {
      regexp "^${root}(.*)$" $url _ url
    } elseif {![my exists url]} {
      #my log "--R we have no url, use package_url '$package_url'"
      # if we have no more information, we use the package_url as actual url
      set url $package_url
    }
    my set_url -url $url
    my set mime_type text/html
    my set delivery ns_return
    set target_class ::[my package_key]::Package
    if {[my info class] ne $target_class && [my isclass $target_class]} {
      my class $target_class
    }
    my initialize
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

    @return folder_id
  } {
    my instvar id

    set folder_id [ns_cache eval xotcl_object_type_cache root_folder-$id {
      
      set folder_id [::xo::db::CrClass lookup -name $name -parent_id $parent_id]
      if {$folder_id == 0} {
        my log "folder with name '$name' and parent $parent_id does NOT EXIST"
        set folder_id [::xo::db::sql::content_folder new \
                           -name $name -label $name \
                           -parent_id $parent_id \
                           -package_id $id -context_id $id]
        my log "CREATED folder '$name' and parent $parent_id ==> $folder_id"
      }

      # register all specified content types
      ::xo::db::CrFolder register_content_types \
          -folder_id $folder_id \
          -content_types $content_types
      #my log "returning from cache folder_id $folder_id"
      return $folder_id
    }]
    #my log "returning from require folder_id $folder_id"
    return $folder_id
  }

  ::xo::Package instproc set_url {-url} {
    my url $url
    my set object [string range [my url] [string length [my package_url]] end]
    #my msg "--R object set to [my set object], url=$url, [my serialize]"
  }

  ::xo::Package instproc reply_to_user {text} {
    #my log "REPLY [::xo::cc exists __continuation]"
    if {[::xo::cc exists __continuation]} {
      #my log "REPLY [::xo::cc set __continuation]"
      eval [::xo::cc set __continuation]
    } else {
      if {[string length $text] > 1} {
	set status_code [expr {[::xo::cc exists status_code] ? [::xo::cc set status_code] : 200}]
        #my log "REPLY [my set delivery] 200 [my set mime_type]"
        [my set delivery] $status_code [my set mime_type] $text
      }
    }
  }

  ::xo::Package instproc return_page {-adp:required -variables -form} {
    #my log "--vars=[self args]"
    set __vars [list]
    foreach _var $variables {
      if {[llength $_var] == 2} {
        lappend __vars [lindex $_var 0] [uplevel subst [lindex $_var 1]]
      } else {
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
    #my log "--before adp"   ; # $__vars
    set text [template::adp_include $adp $__vars]
    #my log "--after adp"
    if { [lang::util::translator_mode_p] } {
      set text [::xo::localize $text 1]
    }
    #my log "--after adp"
    return $text
  }
 
  #ns_log notice [::xo::Package serialize]

}