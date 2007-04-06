ad_library {
  XOTcl API for low level db abstraction

  @author Gustaf Neumann
  @creation-date 2006-12-28
  @cvs-id $Id$
}

namespace eval ::xo::db {

  Object call
  call set postgresql_proc {select ${package_name}__${object_name}($psql_args)}
  call set postgresql_func {select ${package_name}__${object_name}($psql_args)}
  call set oracle_proc {
    BEGIN
    ${package_name}.${object_name}($psql_args);
    END; 
  }
  call set oracle_func {
    BEGIN
    :1 := ${package_name}.${object_name}($psql_args);
    END; 
  }
  # during load, we do not have "package_plsql_args" available yet, so we do it by hand
  call set oracle_get_params {
	select args.argument_name
        from user_arguments args
        where args.position > 0
	  and args.object_name = upper(:object_name)
	  and args.package_name = upper(:package_name)
  }
  call set postgresql_get_params {
	select args.arg_name
        from acs_function_args args
        where args.function = upper(:package_name) || '__' || upper(:object_name)
  }
  call set oracle_all_package_functions {
    select distinct package_name, object_name
        from user_arguments args
        where args.position > 0
  }
  call set postgresql_all_package_functions {
    select distinct 
       substring(function from 0 for position('__' in function)) as package_name,
       substring(function from position('__' in function)+2) as object_name 
    from acs_function_args
  }
  call set oracle_is_function {
    select 1 from dual
    where exists (select 1 from user_arguments
		       package_name = upper(:package_name)
		       and object_name = upper(:object_name)
		       and position = 0)
  }
  call set postgresql_is_function {
    select 1 from dual
  }

  Class DbPackage
  DbPackage instproc dbproc {{-f:switch false} object_name} {
    set package_name [namespace tail [self]]
    set function_args [db_list get_function_params [call set [db_driverkey ""]_get_params]]
    set f [db_0or1row is_function [call set [db_driverkey ""]_is_function]]
    #my log "function_args for -object_name $object_name $package_name are: $function_args"

    set psql_args [list]
    foreach arg $function_args {
      lappend psql_args \$_$arg
      set defined($arg) 1
    }

    set psql_args [join $psql_args ", "]
    set driver [db_driverkey ""]_[expr {$f ? "func" : "proc"}]
    set sql_command [list db_exec_plsql exec_${package_name}-${object_name} [subst [call set $driver]]]

    my proc $object_name {{-n:switch false} arglist} [subst -novariables {
      array set defined [list [array get defined]]

      foreach var $arglist {
        if {\[llength $var\]>1} {
          foreach {var value} $var break
          set attribute \[string toupper $var\]
          set $attribute \[uplevel subst $value\]
          #my log "ATT set $attribute \[uplevel subst $value\]"
        } else {
          set attribute \[string toupper $var\]
          my upvar $var $attribute
        }
        if {!\[info exists defined($attribute)\]} {
          my log "ERROR: $attribute not defined in ${package_name}.${object_name}"
        }
      }
      foreach arg [list [set function_args]] {
        set _$arg \[expr {\[info exists $arg\] ? ":$arg" : "null"}\]
      }
      set sql_command \[subst "[set sql_command]"\]
      if {$n} {
        my log "sql=$sql_command"
      } else {
        my log "sql=$sql_command"
        eval $sql_command
      }
    }]
  }

  DbPackage instproc unknown {m args} {
    error "Error: unknown database method $m for dbpackage [self]"
  }
 
  DbPackage proc create_all_functions {} {
    db_foreach get_package_functions [call set [db_driverkey ""]_all_package_functions] {
      if {![my isobject $package_name]} { DbPackage create $package_name }
      $package_name dbproc $object_name
    }
  }
  DbPackage create_all_functions

#   DbPackage CONTENT_FOLDER
#   CONTENT_FOLDER dbproc REGISTER_CONTENT_TYPE
#   CONTENT_FOLDER dbproc UNREGISTER_CONTENT_TYPE

#   DbPackage CONTENT_TYPE
#   CONTENT_TYPE dbproc CREATE_TYPE
#   CONTENT_TYPE dbproc DROP_TYPE
#   CONTENT_TYPE dbproc CREATE_ATTRIBUTE

#   DbPackage content_item
#   CONTENT_ITEM dbproc NEW
#   CONTENT_ITEM dbproc DELETE
#   CONTENT_ITEM dbproc SET_LIVE_REVISION

  #ns_log notice PROC=[content_folder serialize]

  #
  # provide a interface to call stored procedures or functions in Postgres or Oracle
  #
  call proc psql {{-n:switch false} {-f:switch false} package_name object_name arglist} {
    set function_args [util_memoize [list package_plsql_args -object_name $object_name $package_name]]
    foreach arg $function_args { set defined($arg) 1}
    
    foreach var $arglist {
      if {[llength $var]>1} {
	foreach {var value} $var break
	set attribute [string toupper $var]
	set $attribute [uplevel subst $value]
	my log "ATT set $attribute [uplevel subst $value]"
      } else {
	set attribute [string toupper $var]
	my upvar $var $attribute
      }
      if {![info exists defined($attribute)]} {
	my log "ERROR: $attribute not defined in ${package_name}.${object_name}"
      }
    }
    set psql_args [list]
    foreach arg $function_args {
      lappend psql_args [expr {[info exists $arg] ? ":$arg" : "null"}]
    }
    set psql_args [join $psql_args ", "]
    set driver [db_driverkey ""]_[expr {$f ? "func" : "proc"}]
    my log "SQL($driver)= [subst [my set $driver]]"
    if {!$n} {
      db_exec_plsql exec_${package_name}-${object_name} [subst [my set $driver]]
    }
  }

#  call psql -n content_type create_type {
#    {content_type object_type} supertype pretty_name pretty_plural
#    table_name id_column name_method
#  }

  ::xotcl::Object require

  require set postgresql_table_exists {select 1 from pg_tables   where tablename  = '$name'}
  require set postgresql_view_exists  {select 1 from pg_views    where viewname   = '$name'}
  require set postgresql_index_exists {select 1 from pg_indexes  where indexname  = '$name'}
  require set oracle_table_exists     {select 1 from all_tables  where table_name = '$name'}
  require set oracle_view_exists      {select 1 from all_views   where view_name  = '$name'}
  require set oracle_index_exists     {select 1 from all_indexes where index_name = '$name'}

  require proc table {name definition} {
    if {![db_0or1row check-$name [subst [my set [db_driverkey ""]_table_exists]]]} {
      db_dml create-$name "create table $name ($definition)"
    }
  }

  require proc view {name definition} {
    if {![db_0or1row check-$name [subst [my set [db_driverkey ""]_view_exists]]]} {
      db_dml create-$name "create view $name AS $definition"
    }
  }

  require proc index {-table -col {-using ""} {-unique false}} {
    set colpart $col
    regsub -all ", *" $colpart _ colpart
    set suffix [expr {$unique ? "un_idx" : "idx"}]
    set uniquepart [expr {$unique ? "UNIQUE" : ""}]
    set name ${table}_${colpart}_$suffix
    if {![db_0or1row check-$name [subst [my set [db_driverkey ""]_index_exists]]]} {
      set using [expr {$using ne "" ? "using $using" : ""}]
      db_dml create-$name \
          "create $uniquepart index $name ON $table $using ($col)"
    }
  }

  proc has_ltree {} {
    ns_cache eval xotcl_object_cache ::xo::has_ltree {
      if {[db_string check_ltree "select count(*) from pg_proc where proname = 'ltree_in'"] == 0} {
        return 0
      }
      return 1
    }
  }

}

