# Motivations: 
#
#  - Huge number of parameter_values in larger dotlrn installations
#    Learn: currently > 0.3 mio entries, 
#    Galileo: > 2mio (2nd most frequent kind of object type)
#    Small oacs installations: 1000 objects (38 package instances)
#
#  - High growth, when parameters are used more intensively
#    Size grows quadratically: #parameter * #package_instances,
#     independent of changed parameter values
#    -> does not scale well.
#
# - High degree of redundancy:
#   Most parameters are stored multiple times with the same values
#   (e.g. most dotlrn parameters > 4000 times on dotlrn; cause:
#   Cause: high number of communities.
#  
#   Do we really need to store 4000 times what the pretty-plural 
#   string is one and the same string?
#
# - Most parameter_values are identical to default values
#   For 1 parameter in learn, we have 8 different values, for
#   4 parameters we have 3 different values, ... for most, 
#   all values are the same
#
# - Huge space improvements, when redundancy is removed.
#   Learn: from 300000 entries -> 406 necessary entries
#   Small oacs installation: 1000 objects -> 256 necessary entries
#   => especially big savings on larger installations.
#
# Other shortcomings:
# 
# - Existent design is 2 level: 
#     package-key provides default
#     package-instance keeps values (materialized cross-product)
#
# - Consquences
#   1) Since default values are copied into
#      per-package-instance-values altering the default has no
#      immediate effect.  It would be nice to alter in an OpenACS
#      installation e.g. the default-values for all forums for a
#      certain parameter, and that this value is used in cases, where
#      the admin has not changed the package parameters
#
#   2) No inheritance between packages is possible.  It would be nice
#      to define derived packages (such as e.g. s5 derived from
#      xowiki) where the parameters do not have to be duplicated
#      (e.g. a new parameter added to xowiki should be available in
#      the s5 package as well, otherwise code reuese is limited)
#
# ======================================================================
#
# The implementation below addressed these issues (i.e. is much more
# flexible) and is substantially faster (current implementation):
#      parameter get_from_package_key   old: 172.92 new: 32.16 (5x)
#      parameter get                    old: 63.09 new: 31.29 (2x)
#      
# The implementation uses the OpenACS datamodel (apm_packages, 
# apm_package_values) and loads the parameters during startup.
#
# Missing: 
#   - definition of new parameters (based on ::xo::db interface)
#   - changing of per-package-key values
#   - user interface 
#   - alternate permissions for changing/deleting per-package-instance and
#     per-package-key values (simple approach: use swa for the latter)
#
# ======================================================================
#
# Illustrative example for lookup logic
#
# Package class hierarchy
#
#   ::xo::Package (apm_package)
#      <- ::xowiki::Package
#           <- ::s5::Package
#   
#   package_parameter: 
#       parameter_id  package_key  parameter_name         default_value
#        835          xowiki       with_yahoo_publisher     0
#       2071          s5           with_yahoo_publisher     0
#
#   apm_packages:
#       package_id  parameter_id  attr_value
#       2075        2071          0
#
# Lookup for package_id=2075 "with_yahoo_publisher"
#   1) lookup parameter_id for "with_yahoo_publisher" from s5 (::s5::Package)
#      1.1) parameter_id exists for s5 => parameter_id=2071
#           lookup value for parameter_id=2071,package_id=2075 
#           1.1.1) value for parameter=2071 and package_id=2075 exists 
#                  => return value
#           1.1.2) value for parameter=2071 and package_id=2075 does not exist 
#                  => return default value for parameter and package_key=s5
#      1.2) no parameter_id for s5 + "with_yahoo_publisher"
#           search for parameter_id in superclasses ...
#
#   2) lookup parameter_id for "with_yahoo_publisher" from superclass
#      2.1) parameter_id exists for xowiki => parameter_id=835
#           lookup value for parameter_id=835,package_id=2075 
#           2.1.1) value for parameter=835 and package_id=2075 exists 
#                  => return value
#           2.1.2) value for parameter=835 and package_id=2075 does not exist 
#                  => return default value for parameter and package_key=xowiki
#      2.2) no parameter_id for xowiki + "with_yahoo_publisher"
#           search for parameter_id in superclasses ...

namespace eval ::xo {

  Class create ::xo::parameter
  
  # Every OpenACS parameter should work with the methods defined here.
  # So, fetch first the apm_parameter class from the definitions
  # in the database, and ...
  ::xo::db::Class get_class_from_db -object_type apm_parameter

  #
  # Complete attribute definition in acs_attributes
  #
  ::xo::db::apm_parameter slots {
    ::xo::db::Attribute create description  \
        -datatype string -sqltype varchar(2000) \
        -pretty_name "Description"
    ::xo::db::Attribute create section_name \
        -datatype string -sqltype varchar(200) \
        -pretty_name "Section Name"
    ::xo::db::Attribute create datatype \
        -datatype string -sqltype "varchar(100) not null" \
        -constraint_values [list number string text] \
        -default "string" \
        -pretty_name "Datatype"
    #
    # TODO: Constraint_values are dummies for now.
    #
    # Should be for db::Attributes:
    #    constraint apm_parameters_datatype_ck 
    #    check(datatype in ('number', 'string','text')),
    #
    # Could be used directly for UI selections as well.
    #
    #
    # Complete some slot definitions:
    #
    # TODO: the following two settings making package_key and
    # default_value required are semantically correct. However, this
    # prohibits that apm_parameters can be created via ::xo::db::Class
    # instantiate_objects, since this functions tries to create
    # objects first without parameters.
    #
    #package_key   configure -required true
    #default_value configure -required true

    section_name  configure -default ""
  }

  # ... add the methods of ::xo::parameter by adding this as a mixin
  ::xo::db::apm_parameter instmixin parameter

  #
  # Methods on the parameter class object
  #
  parameter proc get_package_key_from_id {-package_id:required} {
    return [apm_package_key_from_id $package_id]
  }
  parameter proc get_package_id_from_package_key {-package_key:required} {
    return [ns_cache eval xotcl_package_cache package_id-$package_key {
      ::xo::dc get_value get_package_id \
          [::xo::dc select -vars package_id -from apm_packages \
               -where "package_key = :package_key" -limit 1]
    }]
  }

  parameter proc get_parameter_object {
                                       -parameter_name:required
                                       -package_id
                                       -package_key
                                       {-retry true}
                                     } {
    #::xo::PackageMgr instvar package_class
    if {![info exists package_key]} {
      set package_key [:get_package_key_from_id -package_id $package_id]
    }
    while {$package_key ne ""} {
      set key Parameter_id($package_key,$parameter_name)
      if {[info exists :$key]} {
        return [set :$key]
      }
      # 
      # We did not find the parameter object for the current package
      # key. Loop up the parameter class (TODO: should be done from
      # object_type of package_id, but first, we have to store it
      # there).  We simply iterate here of the classes of packages
      # (only a few exist).
      # 
      #my log "--p looking for $parameter_name in superclass of package_key=$package_key"
      set success 0
      set pkg_class [::xo::PackageMgr get_package_class_from_package_key $package_key]
      if {$pkg_class ne ""} {
        set sc [$pkg_class info superclass]
        if {[$sc exists package_key]} {
          set package_key [$sc package_key]
          set success 1
        }
      }
      if {!$success} break
    }
    if {$retry} {
      #
      # The parameter object was not found. Maybe this is a new 
      # parameter, not known in this thread. We try to load it
      #
      set r [::xo::db::apm_parameter instantiate_objects \
                 -sql [::xo::db::apm_parameter instance_select_query \
                           -where_clause {
                             and parameter_name = :parameter_name
                             and package_key = :package_key
                           }] \
                 -object_class ::xo::db::apm_parameter \
                 -ignore_missing_package_ids true \
                 -as_ordered_composite false -named_objects true -destroy_on_cleanup false]
      #
      # Check for "retry" to avoid potential recursive loops
      #
      if {$r ne ""} {
        #
        # seems as if this parameter was newly defined
        #
        if {![info exists package_id]} {set package_id ""}
        return [:get_parameter_object \
                    -retry false \
                    -parameter_name $parameter_name \
                    -package_id $package_id \
                    -package_key $package_key]
      }
    }
    #
    # If everything fails, return empty.
    #
    return ""
  }

  parameter proc get_from_package_key {
                                       -package_key:required
                                       -parameter:required
                                       -default
                                     } {
    set parameter_obj [:get_parameter_object -package_key $package_key -parameter_name $parameter]
    if {$parameter_obj eq ""} {
      if {[info exists default]} {return $default}
      error "No parameter '$parameter' for package_key '$package_key' defined"
    }
    set package_id [:get_package_id_from_package_key -package_key $package_key]
    set value [$parameter_obj get -package_id $package_id]
    if {$value eq "" && [$parameter_obj set __success] == 0 && [info exists default]} {
      return $default
    } else {
      return $value
    }
  }

  parameter proc get {
                      -package_id
                      -parameter:required
                      -default
                      {-retry true}
                    } {
    if {![info exists package_id]} {
      #
      # Try to get the package id; if everything fails, use kernel_id
      # (to be compatible with traditional parameter::get)
      #
      set package_id [expr {[info commands ::xo::cc] ne "" ? 
                            [::xo::cc package_id] : 
                            [ns_conn isconnected] ? [ad_conn package_id] : [ad_acs_kernel_id]}]
    }
    set parameter_obj [:get_parameter_object -parameter_name $parameter -package_id $package_id -retry $retry]
    if {$parameter_obj ne ""} {
      set value [$parameter_obj get -package_id $package_id]
      if {$value eq "" && [$parameter_obj set __success] == 0} {return $default}
      return $value
    } else {
      return $default
    } 
  }
  
  #
  # Methods for parameter instances
  #
  parameter instproc set_per_package_instance_value {package_id value} {
    set array [:per_package_id_name $package_id]
    nsv_set $array [:parameter_name] $value
  }
  parameter instproc clear_per_package_instance_value {package_id value} {
    set array [:per_package_id_name $package_id]
    if {[nsv_exists $array [:parameter_name]]} {
      nsv_unset $array [:parameter_name]
    }
  }
  parameter instproc initialize_loaded_object {} {
    [self class] set Parameter_id(${:package_key},${:parameter_name}) [self]
  }
  parameter instproc per_package_id_name {package_id} {
    return "CFG-$package_id"
  }
  #   parameter instproc per_package_class_name {package_class} {
  #     return "CFG-$package_class"
  #   }
  parameter instproc get {-package_id:required} {
    set key [:parameter_name]
    set nsv_array_name [:per_package_id_name $package_id]
    if {[nsv_exists $nsv_array_name $key]} {
      #my log "--parameter get <$key> from $nsv_array_name --> '[nsv_get $nsv_array_name $key]'"
      set :__success 1
      return [nsv_get $nsv_array_name $key]
    }
    # We could as well store per-package-key values,
    # but most probably, this is not needed if we use
    # the parameter default (which is per package-key).
    # With additional  per-package-key values, we could implement
    # a very simple "reset to default" for package-key values.
    #
    #     foreach cls $package_class_hierarchy {
    #       set nsv_array_name [:per_package_class_name $cls]
    #       if {[nsv_exists $nsv_array_name $key]} {
    #         #my log "--parameter get <$key> from $nsv_array_name --> '[nsv_get $nsv_array_name $key]'"
    #         return [nsv_get $nsv_array_name $key]
    #       }
    #     }
    #
    #my log "--parameter get <$key> from default of [:package_key] --> '[:default_value]'"
    set :__success 0
    return [:default_value]
  }

  # get apm_parameter objects
  ::xo::db::apm_parameter instantiate_objects \
      -sql [::xo::db::apm_parameter instance_select_query] \
      -object_class ::xo::db::apm_parameter \
      -ignore_missing_package_ids true \
      -as_ordered_composite false -named_objects true -destroy_on_cleanup false
  #  ns_log notice "--p got [llength [::xo::db::apm_parameter info instances]] parameters"
  #foreach p [::xo::db::apm_parameter info instances] { ns_log notice [$p serialize] }

  parameter proc initialize_parameters {} {
    # Get those parameter values, which are different from the default and
    # remember theses per package_id.
    xo::dc foreach get_non_default_values {
      select p.parameter_id, p.package_key, v.package_id, p.parameter_name, 
      p.default_value, v.attr_value 
      from apm_parameters p, apm_parameter_values v 
      where p.parameter_id = v.parameter_id 
      and coalesce(attr_value,'') <> coalesce(p.default_value,'')
    } {
      #      ns_log notice "--p $parameter_id $package_key $package_id $parameter_name <$attr_value>"
      $parameter_id set_per_package_instance_value $package_id $attr_value
    }
  }

  parameter initialize_parameters

  #
  # For the time being: catch changed parameter values
  #
  ad_proc -public -callback subsite::parameter_changed -impl xotcl-param-procs {
    -package_id:required
    -parameter:required
    -value:required
  } {
    Implementation of subsite::parameter_changed for xotcl param procs
    
    @param package_id the package_id of the package the parameter was changed for
    @param parameter  the parameter name
    @param value      the new value
  } {
    #
    # In order to use the existing inerface for parameters, we catch
    # all parameter changes and update accordingly the values in the new
    # interface.
    #
    set package_key [apm_package_key_from_id $package_id]
    set parameter_obj [::xo::parameter get_parameter_object \
                           -package_key $package_key \
                           -parameter_name $parameter]
    
    if {$parameter_obj eq ""} {
      # We have still no parameter. There must be something significantly wrong.
      error "--parameter $parameter for package $package_key, package_id $package_id does not exist"
    } else {
      $parameter_obj clear_per_package_instance_value $package_id $value
      if {[$parameter_obj default_value] ne $value} {
        $parameter_obj set_per_package_instance_value $package_id $value
      }
    }
  }


  #
  #  A few test cases
  #
  #   ns_log notice "xotcl-request-monitor.max-url-stats=[parameter get_from_package_key \
      #       -package_key xotcl-request-monitor \
      #       -parameter max-url-stats]"

  #   set cmd1 "::parameter::get_from_package_key \
      #       -package_key xotcl-request-monitor \
      #       -parameter max-url-stats"
  #   set cmd2 "::xo::parameter get_from_package_key \
      #       -package_key xotcl-request-monitor \
      #       -parameter max-url-stats"
  #   ns_log notice "GET_PACKAGE_KEY old: [time $cmd1 100], new: [time $cmd2 100]"

  #   set pid 4906
  #   set pname trend-elements 
  #   ns_log notice "xotcl-request-monitor.$pname=[parameter get \
      #       -package_id $pid -parameter $pname]"
  #   set cmd1 "::parameter::get -package_id $pid -parameter $pname"
  #   set cmd2 "::xo::parameter get -package_id $pid -parameter $pname"
  #   ns_log notice "GET old: [time $cmd1 100], new: [time $cmd2 100]"

  #
  #
  #
  #
  #   set p [parameter get_parameter_object -package_key xowiki -parameter_name dummy]
  #   ns_log notice "--p getobject => $p"
  #   if {$p eq ""} {
  #     set p [::xo::db::apm_parameter new_persistent_object  \
      #                -package_key "xowiki" \
      #                -parameter_name "dummy" \
      #                -default_value "testing" \
      #                -description "Description of test parameter" \
      #                -section_name ""]
  #     ns_log notice "--p created new parameter $p"
  #   }
  #   $p append default_value "1"
  #   $p save
  # $p delete
  
}

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
