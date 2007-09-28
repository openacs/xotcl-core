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

  PackageMgr ad_instproc instances {{-include_unmounted false}} {
    @return list of package_ids of xowiki instances
  } {
    my instvar package_key
    if {$include_unmounted} {
      return [db_list [my qn get_xowiki_packages] {select package_id \
        from apm_packages where package_key = :package_key}]
    } else {
      return [db_list [my qn get_mounted_packages] {select package_id \
        from apm_packages p, site_nodes s  \
        where package_key = :package_key and s.object_id = p.package_id}]
    }
  }

  PackageMgr ad_instproc initialize {
    -ad_doc
    {-parameter ""}
    {-package_id 0}
    {-url ""}
    {-user_id -1}
    {-actual_query " "}
    {-init_url true}
    {-form_parameter}
  } {
    Create a connection context if there is none available.
    The connection context should be reclaimed after the request
    so we create it as a volatile object in the toplevel scope,
    it will be destroyed automatically with destroy_on_cleanup, 
    when the global variables are reclaimed.
    
    As a side effect this method sets in the calling context
    the query parameters and package_id as variables, using the 
    "defaults" for default values.

    init_url false requires the package_id to be specified and
    a call to Package instproc set_url to complete initialization
  } {
    #my log "--i [self args], URL=$url, init_url=$init_url"

    if {$url eq "" && $init_url} {
      #set url [ns_conn url]
      #my log "--CONN ns_conn url"
      set url [root_of_host [ad_host]][ns_conn url]
    }
    #my log "--cc actual_query = <$actual_query>"

    # require connection context
    ConnectionContext require \
	-package_id $package_id -user_id $user_id \
	-parameter $parameter -url $url -actual_query $actual_query
    set package_id [::xo::cc package_id]
    if {[info exists form_parameter]} {
      ::xo::cc array set form_parameter $form_parameter
    }

    # create package object if necessary
    my require -url $url $package_id
    ::xo::cc export_vars -level 2
  }

  PackageMgr ad_instproc require {{-url ""} package_id} {
    Create package object if needed.
  } {
    #my log "--R $package_id exists? [my isobject ::$package_id] url='$url'"
    if {![my isobject ::$package_id]} {
      #my log "--R we have to create ::$package_id //url='$url'"
      if {$url ne ""} {
        my create ::$package_id -url $url
      } else {
        my create ::$package_id
      }
      ::$package_id destroy_on_cleanup
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
                -default $default]
    #my log "--get_parameter <$attribute> <$default> returned $param"
    return $param
  }
 
  ::xo::Package instproc init args {
    #my log "--R creating"
    my instvar id url
    set id [namespace tail [self]]
    array set info [site_node::get_from_object_id -object_id $id]
    set package_url $info(url)
    if {[ns_conn isconnected]} {
      # in case of of host-node map, simplify the url to avoid redirects
      # .... but ad_host works only, when we are connected.... TODO: solution for syndication
      set root [root_of_host [ad_host]]
      regexp "^${root}(.*)$" $package_url _ package_url
    }
    #my log "--R package_url= $package_url (was $info(url))"
    my package_url $package_url
    my package_key $info(package_key)
    my instance_name $info(instance_name)
    if {[my exists url] && [info exists root]} {
      regexp "^${root}(.*)$" $url _ url
    } elseif {![my exists url]} {
      my log "--R we have no url, use package_url"
      # if we have no more information, we use the package_url as actual url
      set url $package_url
    } 
    my set_url -url $url
  }
 
  ::xo::Package instproc set_url {-url} {
    my url $url
    my set object [string range [my url] [string length [my package_url]] end]
    #my log "--R object set to [my set object], [my serialize]"
  }

 
  #ns_log notice [::xo::Package serialize]

}