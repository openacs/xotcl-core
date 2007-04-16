ad_library {
  XOTcl API for low level db abstraction

  @author Gustaf Neumann
  @creation-date 2006-12-28
  @cvs-id $Id$
}

namespace eval ::xo::db {

  Object call
  # during load, we do not have "package_plsql_args" available yet, so we do it by hand
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

  proc map {sql} {
    if {[db_driverkey ""] eq "oracle"} {return [string map [list "__" .] $sql]}
    return $sql
  }

  Class DbPackage
  DbPackage instproc sql-arguments {sql package_name object_name} {
    my array unset defined
    my set function_args [db_list_of_lists [my qn get_function_params] $sql]
    set psql_args [list]  
    my set arg_order [list]
    foreach arg [my set function_args] {
      foreach {arg_name default_value} $arg break
      lappend psql_args \$_$arg_name
      my lappend arg_order $arg_name
      my set defined($arg_name) $default_value
    }
    if {"$package_name-$object_name" eq "CONTENT_ITEM-NEW"} {
      # content_item__new does currently not define null default values.
      # This ugly - temporary - hack is used to keep the :required passing and to allow 
      # the xowiki regression test to run. The correct fix is to define in
      # correct default values in the database with define_function_args()
      my array set defined {RELATION_TAG null DESCRIPTION null TEXT null CREATION_IP null NLS_LANGUAGE null LOCALE null CONTEXT_ID null DATA null TITLE null ITEM_ID null 
      }
    }
    return [join $psql_args ", "]
  }
  DbPackage instproc psql-postgresql {package_name object_name full_statement_name} {
    set psql_args [my sql-arguments {
	select args.arg_name, args.arg_default
        from acs_function_args args
        where args.function = upper(:package_name) || '__' || upper(:object_name)
        order by function, arg_seq
    } $package_name $object_name]
    my set sql [subst {
      select ${package_name}__${object_name}($psql_args)
    }]
    return {ns_pg_bind 0or1row $db $sql}
  }
  DbPackage instproc psql-oracle {package_name object_name full_statement_name} {
    # 
    # in Oracle, we have to distinguish between functions and procs
    #
    set is_function [db_0or1row [my qn is_function] {
       select 1 from dual
       where exists (select 1 from user_arguments
		       package_name = upper(:package_name)
		       and object_name = upper(:object_name)
		       and position = 0)
    }]
    set psql_args [my sql-arguments {
	select args.argument_name, args.default_value
        from user_arguments args
        where args.position > 0
	  and args.object_name = upper(:object_name)
	  and args.package_name = upper(:package_name)
        order by args.position
    } $package_name $object_name]
    if {$is_function} {
      my set sql [subst {BEGIN; :1 := ${package_name}.${object_name}($psql_args); END;}]
      return [subst {db_exec exec_plsql_bind \$db $full_statement_name \$sql 2 1 ""}]
    } else {
      my set sql [subst {BEGIN; ${package_name}.${object_name}($psql_args); END;}]
      return [subst {db_exec dml \$db $full_statement_name \$sql}]
    }
  }

  DbPackage instproc dbproc_exportvars {object_name} {
    #
    # This method compiles a stored procedure into a xotcl method using
    # a export_vars style interface.
    #
    # The current implementation should work on postgres and oracle (not tested)
    # but will not work, when a single openacs instance want to talk to 
    # postgres and oracle simultaneously. Not sure, how important this is...
    #
    set package_name   [namespace tail [self]]
    set statement_name [my qn $package_name-$object_name]
    set sql_command    [my psql-[db_driverkey ""] $package_name $object_name $statement_name] 

    my proc $object_name {{-n:switch false} {-dbn ""} arglist} [subst -novariables {
      array set defined [list [my array get defined]]

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
      foreach {_arg} [list [my set function_args]] {
        foreach {arg default_value} $_arg break
        set _$arg \[expr {\[info exists $arg\] ? ":$arg" : "null"}\]
      }
      set sql \[list "[my set sql]"\]
      if {$n} {
        my log "sql=$sql"
      } else {
        db_with_handle -dbn $dbn db {
          #my log "sql=$sql, sql_command=[set sql_command]"
          set selection \[eval [set sql_command]\]
          return \[ns_set value $selection 0\]
        }
      }
    }]
  }

  DbPackage instproc dbproc_nonposargs {object_name} {
    #
    # This method compiles a stored procedure into a xotcl method 
    # using a classic nonpositional argument style interface.
    #
    # The current implementation should work on postgres and oracle (not tested)
    # but will not work, when a single openacs instance want to talk to 
    # postgres and oracle simultaneously. Not sure, how important this is...
    #
    set package_name   [namespace tail [self]]
    set statement_name [my qn $package_name-$object_name]
    set sql_command    [my psql-[db_driverkey ""] $package_name $object_name $statement_name] 

    set nonposarg_list [list [list -dbn ""]]
    foreach arg_name [my set arg_order] {
      set default_value [my set defined($arg_name)]
      set required [expr {$default_value eq "" ? ":required" : ""}]
      set nonposarg_name [expr {$arg_name eq "DBN" ? "DBN" : [string tolower $arg_name]}]
      lappend nonposarg_list -$nonposarg_name$required
    }

    my ad_proc $object_name $nonposarg_list {} [subst -novariables {
      #defined: [my array get defined]

      foreach var \[list [my set arg_order]\]  {
        set varname \[string tolower $var\]
        if {\[info exists $varname\]} {
          set $var \[set $varname\]
          set _$var :$var
        } else {
          set _$var null
        }
      }

      set sql \[list "[my set sql]"\]
      db_with_handle -dbn $dbn db {
        #my log "sql=$sql, sql_command=[set sql_command]"
        set selection \[eval [set sql_command]\]
        return \[ns_set value $selection 0\]
      }
    }]
  }

  DbPackage instproc unknown {m args} {
    error "Error: unknown database method $m for dbpackage [self]"
  }
 
  DbPackage proc create_all_functions {} {
    db_foreach [my qn ""] [call set [db_driverkey ""]_all_package_functions] {
      if {![my isobject $package_name]} { DbPackage create $package_name }
      $package_name dbproc_exportvars $object_name
      set class_name [string tolower $package_name] 
      if {![my isobject $class_name]} { DbPackage create $class_name }
      $class_name dbproc_nonposargs [string tolower $object_name]
    }
  }
  DbPackage create_all_functions

  ::xotcl::Object create require

  require set postgresql_table_exists {select 1 from pg_tables   where tablename  = '$name'}
  require set postgresql_view_exists  {select 1 from pg_views    where viewname   = '$name'}
  require set postgresql_index_exists {select 1 from pg_indexes  where indexname  = '$name'}
  require set oracle_table_exists     {select 1 from all_tables  where table_name = '$name'}
  require set oracle_view_exists      {select 1 from all_views   where view_name  = '$name'}
  require set oracle_index_exists     {select 1 from all_indexes where index_name = '$name'}

  require proc table {name definition} {
    if {![db_0or1row [my qn ""] [subst [my set [db_driverkey ""]_table_exists]]]} {
      db_dml [my qn create-table-$name] "create table $name ($definition)"
    }
  }

  require proc view {name definition} {
    if {![db_0or1row [my qn ""] [subst [my set [db_driverkey ""]_view_exists]]]} {
      db_dml [my qn create-view-$name] "create view $name AS $definition"
    }
  }

  require proc index {-table -col {-using ""} {-unique false}} {
    set colpart $col
    regsub -all ", *" $colpart _ colpart
    set suffix [expr {$unique ? "un_idx" : "idx"}]
    set uniquepart [expr {$unique ? "UNIQUE" : ""}]
    set name ${table}_${colpart}_$suffix
    if {![db_0or1row [my qn ""] [subst [my set [db_driverkey ""]_index_exists]]]} {
      set using [expr {$using ne "" ? "using $using" : ""}]
      db_dml [my qn create-index-$name] \
          "create $uniquepart index $name ON $table $using ($col)"
    }
  }

  ad_proc has_ltree {} {
    Check, whether ltree is available (postgres only)
  } {
    ns_cache eval xotcl_object_cache ::xo::has_ltree {
      if {[db_driverkey ""] eq "postgresql" && 
          [db_0or1row check_ltree "select count(*) from pg_proc where proname = 'ltree_in'"]} {
        return 1
      }
      return 0
    }
  }

}

