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
        where args.position > 0 and package_name is not null
  }
  call set postgresql_all_package_functions {
    select distinct 
       substring(function from 0 for position('__' in function)) as package_name,
       substring(function from position('__' in function)+2) as object_name 
    from acs_function_args
  }

  proc function_name {sql} {
    if {[db_driverkey ""] eq "oracle"} {return [string map [list "__" .] $sql]}
    return $sql
  }

  Class DbPackage

  # Some stored procs like content_item__new do currently not define null default values.
  # Therefore, we need - temporary - this ugly hack is used to keep
  # :required passing and to allow  the xowiki regression test to run. 
  # The correct fix is to define the correct default values in the 
  # database with define_function_args()
  DbPackage array set defaults {
    "content_item__new" {RELATION_TAG null DESCRIPTION null TEXT null 
      CREATION_IP null NLS_LANGUAGE null LOCALE null CONTEXT_ID null 
      DATA null TITLE null ITEM_ID null
    }
    "content_type__create_attribute" {
      DEFAULT_VALUE null SORT_ORDER null PRETTY_PLURAL null
    }
  }

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
    if {[[self class] exists defaults(${package_name}__$object_name)]} {
      my array set defined [[self class] set defaults(${package_name}__$object_name)]
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
    #return {ns_pg_bind 0or1row $db $sql}
    return {ns_set value [ns_pg_bind 0or1row $db $sql] 0}
  }
  DbPackage instproc psql-oracle {package_name object_name full_statement_name} {
    # 
    # in Oracle, we have to distinguish between functions and procs
    #
    set is_function [db_0or1row [my qn is_function] {
       select 1 from dual
       where exists (select 1 from user_arguments where
		       package_name = upper(:package_name)
		       and object_name = upper(:object_name)
		       and position = 0)
    }]
    # In Oracle, args.default_value appears to be defunct and useless.
    # for now, we simply return "null" as a constant, otherwise the
    # argument would be required
    set psql_args [my sql-arguments {
	select args.argument_name, 'unknown'
        from user_arguments args
        where args.position > 0
	  and args.object_name = upper(:object_name)
	  and args.package_name = upper(:package_name)
        order by args.position
    } $package_name $object_name]
    if {$is_function} {
      my set sql [subst {BEGIN :1 := ${package_name}.${object_name}(\$sql_args); END;}]
      return {ns_ora exec_plsql_bind $db $sql 1 ""}
    } else {
      my set sql [subst {BEGIN ${package_name}.${object_name}(\$sql_args); END;}]
      #return {ns_set value [ns_ora select $db $sql] 0}
      return {ns_ora dml $db $sql}
    }
  }

  DbPackage instproc proc_body-postgresql {} {
    return {
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
      set sql "[my set sql]"
      db_with_handle -dbn $dbn db {
        #my log "sql=$sql, sql_command=[set sql_command]"
        return \[ [set sql_command] \]
      }
    }
  }
  DbPackage instproc proc_body-oracle {} {
    return {
      #defined: [my array get defined]
      set sql_args \[list\]
      foreach var \[list [my set arg_order]\]  {
        set varname \[string tolower $var\]
        if {\[info exists $varname\]} {
          lappend sql_args "$varname => :$varname"
        } 
      }
      set sql_args \[join $sql_args ,\]
      set sql "[my set sql]"
      db_with_handle -dbn $dbn db {
        #my log "sql=$sql, sql_command=[set sql_command]"
        return \[ [set sql_command] \]
      }
    }
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
    if {$object_name eq "set"} {
      my log "We cannot handle object_name = '$object_name' in this version"  
      return
    }
    set package_name   [namespace tail [self]]
    set statement_name [my qn $package_name-$object_name]
    set sql_command    [my psql-[db_driverkey ""] $package_name $object_name $statement_name] 
    set proc_body      [my proc_body-[db_driverkey ""]] 

    set nonposarg_list [list [list -dbn ""]]
    foreach arg_name [my set arg_order] {
      set default_value [my set defined($arg_name)]
      set required [expr {$default_value eq "" ? ":required" : ""}]
      # special rule for DBN ... todo: proc has to handle this as well
      set nonposarg_name [expr {$arg_name eq "DBN" ? "DBN" : [string tolower $arg_name]}]
      lappend nonposarg_list -$nonposarg_name$required
    }
    #my log "-- define $object_name $nonposarg_list"

    my ad_proc $object_name $nonposarg_list {} [subst -novariables $proc_body]
  }

  DbPackage instproc unknown {m args} {
    error "Error: unknown database method $m for dbpackage [self]"
  }
 
  DbPackage proc create_all_functions {} {
    db_foreach [my qn ""] [call set [db_driverkey ""]_all_package_functions] {
      #if {![my isobject $package_name]} { DbPackage create $package_name }
      #$package_name dbproc_exportvars $object_name
      set class_name [string tolower $package_name] 
      if {![my isobject $class_name]} { DbPackage create $class_name }
      $class_name dbproc_nonposargs [string tolower $object_name]
    }
  }
  DbPackage create_all_functions

  ::xotcl::Object create sql
  if {[db_driverkey ""] eq "postgresql"} {
    proc map_sql_datatype {type} {return $type}

    sql proc select {
      -vars:required 
      -from:required 
      -where:required 
      {-groupby ""} 
      {-limit ""} 
      {-offset ""} 
      {-start ""}
      {-orderby ""}
      {-map_function_names false}
    } {
      set offset_clause [expr {$offset  ne "" ? "OFFSET $offset" : ""}]
      set limit_clause  [expr {$limit   ne "" ? "LIMIT $limit" : ""}]
      set order_clause  [expr {$orderby ne "" ? "ORDER BY $orderby" : ""}]
      set group_clause  [expr {$groupby ne "" ? "GROUP BY $groupby" : ""}]
      return "SELECT $vars FROM $from WHERE $where $group_clause $order_clause $limit_clause"
    }
  } else { ;# Oracle
    proc map_sql_datatype {type} {
      switch $type {
        text {set type varchar(64000)}
      }
      return $type
    }

    sql proc select {
      -vars:required 
      -from:required 
      -where:required 
      {-groupby ""} 
      {-limit ""} 
      {-offset ""} 
      {-start ""}
      {-orderby ""}
      {-map_function_names false}
    } {
      set order_clause [expr {$orderby ne "" ? "ORDER BY $orderby" : ""}]
      set group_clause [expr {$groupby ne "" ? "GROUP BY $groupby" : ""}]
      if {$map_function_calls} {set vars [::xo::db::function_name $vars]}
      set sql "SELECT $vars FROM $from $start WHERE $where $group_clause"
      if {$limit ne "" || $offset ne ""} {
        if {$offset eq ""} {
          set limit_clause "ROWNUM <= $limit"
        } else {$limit eq ""} {
          set limit_clause "ROWNUM >= $offset"
        } else {
          set limit_clause "ROWNUM BETWEEN $offset and [expr {$offset+$limit}]"
        }
        # for pagination, we will need an "inner" sort, such as 
        # SELECT * FROM (SELECT ...., ROW_NUMBER() OVER (ORDER BY ...) R FROM table) WHERE R BETWEEN 0 and 100 
        set sql "SELECT * FROM (SELECT $sql) WHERE ROWNUM <= $limit_clause $order_clause"
      } else {
        append sql " " $order_clause
      }
      my log "--returned sql = $sql"
      return $sql
    }
  }
  sql proc since_interval_condition {var interval} {
    set since [clock format [clock scan "-$interval"] -format "%Y-%m-%d %T"]
    return "$var > TO_TIMESTAMP('$since','YYYY-MM-DD HH24:MI:SS')"
  }
  ::xotcl::Object create require

  require set postgresql_table_exists {select 1 from pg_tables   where tablename  = '$name'}
  require set postgresql_view_exists  {select 1 from pg_views    where viewname   = '$name'}
  require set postgresql_index_exists {select 1 from pg_indexes  where indexname  = '$name'}
  require set oracle_table_exists     {select 1 from all_tables  where table_name = '$name'}
  require set oracle_view_exists      {select 1 from all_views   where view_name  = '$name'}
  require set oracle_index_exists     {select 1 from all_indexes where index_name = '$name'}

  require proc table {name definition} {
    if {[db_driverkey ""] eq "oracle"} {set name [string toupper $name]}
    if {![db_0or1row [my qn ""] [subst [my set [db_driverkey ""]_table_exists]]]} {
      #my log "--table $name does not exist, creating with $definition"
      db_dml [my qn create-table-$name] "create table $name ($definition)"
    }
  }

  require proc view {name definition} {
    if {[db_driverkey ""] eq "oracle"} {set name [string toupper $name]}
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
    if {[string length $name]>30} {
      if {[db_driverkey ""] eq "oracle"} {
        set sl [string length $suffix]
        set name [string range ${table}_${colpart}  0 [expr {28 - $sl}]]_$suffix
      }
    }
    if {[db_driverkey ""] eq "oracle"} {set name [string toupper $name]}
    if {![db_0or1row [my qn ""] [subst [my set [db_driverkey ""]_index_exists]]]} {
      set using [expr {$using ne "" ? "using $using" : ""}]
      db_dml [my qn create-index-$name] \
          "create $uniquepart index $name ON $table $using ($col)"
    }
  }

  require proc package name {
    if {[info command ::${name}::*] eq ""} {
      set dir [ns_info tcllib]/../packages/$name
      foreach file [glob $dir/tcl/*-procs.tcl] {
        uplevel #1 source $file
      }
    }
  }

  ad_proc has_ltree {} {
    Check, whether ltree is available (postgres only)
  } {
    ns_cache eval xotcl_object_cache ::xo::has_ltree {
      if {[db_driverkey ""] eq "postgresql" && 
          [db_string check_ltree "select count(*) from pg_proc where proname = 'ltree_in'"]} {
        return 1
      }
      return 0
    }
  }

}

