ad_library {
  XOTcl API for low level db abstraction

  @author Gustaf Neumann
  @creation-date 2006-12-28
  @cvs-id $Id$
}


namespace eval ::xo::db {
  #
  # A few helper functions
  #
  # Constaint names are limited in oracle to 30 characters;
  # Postgres has no such limits. Therefore, we use different
  # rules depending on whether we are running under Oracle or not.
  #
  if {[db_driverkey ""] eq "oracle"} {
    proc mk_sql_constraint_name {table att suffix} {
      set name ${table}_${att}_$suffix
      if {[string length $name]>30} {
        set sl [string length $suffix]
        set name [string range ${table}_${att}  0 [expr {28 - $sl}]]_$suffix
      }
      return [string toupper $name]
    }
  } else {
    proc mk_sql_constraint_name {table att suffix} {
      set name ${table}_${att}_$suffix
      return $name
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

  #
  # The object require provides an interface to create certain
  # resources in case they are not created already.
  #
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
    set name [::xo::db::mk_sql_constraint_name $table $colpart $suffix]
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

  ##########################################################
  #
  # ::xo::db::sql is used for interfacing with the database
  #
  # Many of the differences between postgres and oracle
  # are handled by this object. Most prominently,
  #
  #    ::xo::db::sql select ...
  #
  # provides a portable interface for creating SQL 
  # statments for postgres or oracle, handling e.g. 
  # limit/offset, etc. in a generic way.

  ::xotcl::Object create sql
  
  if {[db_driverkey ""] eq "postgresql"} {

    # during load, we do not have "package_plsql_args" available yet, so we do it by hand
    sql set all_package_functions {
      select distinct 
        substring(function from 0 for position('__' in function)) as package_name,
        substring(function from position('__' in function)+2) as object_name 
       from acs_function_args
    }

    sql proc map_function_name {sql} {
      return $sql
    }

    sql proc map_datatype {type} {
      switch -- $type {
        long_text { set type text }
      }
      return $type
    }
    sql proc datatype_constraint {type table att} {
      # for postgres, we do not need type specific constraints
      return ""
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
      set where_clause  [expr {$where   ne "" ? "WHERE $where" : ""}]
      set offset_clause [expr {$offset  ne "" ? "OFFSET $offset" : ""}]
      set limit_clause  [expr {$limit   ne "" ? "LIMIT $limit" : ""}]
      set order_clause  [expr {$orderby ne "" ? "ORDER BY $orderby" : ""}]
      set group_clause  [expr {$groupby ne "" ? "GROUP BY $groupby" : ""}]
      return "SELECT $vars FROM $from $where_clause $group_clause $order_clause $limit_clause"
    }

    sql proc date_trunc {field date} {
      return "date_trunc('$field',$date)"
    }
    sql proc date_trunc_expression {field date date_string} {
      return "date_trunc('$field',$date) = '$date_string'"
    }

  } else { ;# Oracle

    sql set all_package_functions {
      select distinct package_name, object_name
        from user_arguments args
        where args.position > 0 and package_name is not null
    }

    sql proc map_function_name {sql} {
      return [string map [list "__" .] $sql]
    }

    sql proc map_datatype {type} {
      switch -- $type {
        text      { set type varchar2(4000) }
        long_text { set type clob }
        boolean   { set type char(1) }
      }
      return $type
    }
    sql proc datatype_constraint {type table att} {
      set constraint ""
      switch $type {
        boolean {
          set cname [::xo::db::mk_sql_constraint_name $table $att _ck]
          set constraint "constraint $cname check ($att in ('t','f'))"}
      }
      return $constraint
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
      # "-start" not used so far
      set where_clause [expr {$where   ne "" ? "WHERE $where" : ""}]
      set order_clause [expr {$orderby ne "" ? "ORDER BY $orderby" : ""}]
      set group_clause [expr {$groupby ne "" ? "GROUP BY $groupby" : ""}]
      if {$map_function_names} {set vars [my map_function_name $vars]}
      set sql "SELECT $vars FROM $from $where_clause $group_clause"
      if {$limit ne "" || $offset ne ""} {
        if {$offset eq ""} {
          set limit_clause "ROWNUM <= $limit"
        } elseif {$limit eq ""} {
          set limit_clause "ROWNUM >= $offset"
        } else {
          set limit_clause "ROWNUM BETWEEN $offset and [expr {$offset+$limit}]"
        }
        # for pagination, we will need an "inner" sort, such as 
        # SELECT * FROM (SELECT ...., ROW_NUMBER() OVER (ORDER BY ...) R FROM table) WHERE R BETWEEN 0 and 100 
        set sql "SELECT * FROM ($sql $order_clause) WHERE $limit_clause"
      } else {
        append sql " " $order_clause
      }
      my log "--returned sql = $sql"
      return $sql
    }
    sql proc date_trunc {field date} {
      return "to_char(trunc($date,'$field'), 'YYYY-MM-DD HH24:MI:SS')"
    }
    sql proc date_trunc_expression {field date date_string} {
      return "trunc($date,'$field') = trunc(to_date('$date_string','YYYY-MM-DD'),'$field')"
    }
  }
  sql proc since_interval_condition {var interval} {
    set since [clock format [clock scan "-$interval"] -format "%Y-%m-%d %T"]
    return "$var > TO_TIMESTAMP('$since','YYYY-MM-DD HH24:MI:SS')"
  }
}

namespace eval ::xo::db {
  #
  # ::xo::db::Class is a meta class for interfacing with acs_object_types.
  # acs_object_types are instances of this meta class. The meta class defines
  # the behavior common to all acs_object_types
  #
  ::xotcl::Class create ::xo::db::Class \
      -superclass ::xotcl::Class \
      -parameter {
	pretty_name
	pretty_plural
	{supertype acs_object}
	table_name
	id_column
	{abstract_p f}
	{name_method ""}
	{object_type [self]}
	{security_inherit_p t}
	{auto_save false}
	{with_table true}
      } -ad_doc {
	::xo::db::Class is a meta class for interfacing with acs_object_types.
	acs_object_types are instances of this meta class. The meta class defines
	the behavior common to all acs_object_types. The behavior common to
	all acs_objects is defined by the class ::xo::db::Object.
	
	@see ::xo::db::Object
      }
  
  ::xo::db::Class set __default_superclass ::xo::db::Object  ;# will be supported in XOTcl 1.6

  #
  # Define an XOTcl interface for creating new object types
  #
  # Methods for the meta class
  #

  ::xo::db::Class ad_proc exists_in_db {
    -id:required
  } {
    Check, if an acs_object exists in the database.

    @return 0 or 1
  } {
    return [db_string [my qn select_object] {
      select 1 from acs_objects where object_id = :id
    } -default 0]
  }

  ::xo::db::Class ad_proc delete {
    -id:required
  } {
    Delete the object from the database
  } {
    ::xo::db::sql::acs_object delete -object_id $id
  }

  ::xo::db::Class ad_proc get_object_type {
    -id:required
  } {
    Return the object type for the give id.

    @retun object_type, typically an XOTcl class
  } {
    db_1row [my qn get_class] \
	"select object_type from acs_objects where object_id=$id"
    return $object_type
  }

  ::xo::db::Class ad_proc get_instance_from_db {
    -id:required
  } {
    Create an XOTcl object from an acs_object_id. This method
    determines the type and initializes the object from the
    information stored in the database. The XOTcl object is
    destroyed automatically on cleanup (end of a connection request).

    @return fully qualified object
  } {
    set type  [my get_object_type -id $id]
    set class [::xo::db::Class object_type_to_class $type]
    if {![my isclass $class]} {
      error "no class $class defined"
    }
    set r [$class create ::$id]
    $r db_1row dbq..get_instance [$class fetch_query $id]
    $r set object_id $id
    $r destroy_on_cleanup
    return $r
  }

  ::xo::db::Class ad_proc get_table_name {
    -object_type:required
  } {
    Get the table_name of an object_type from the database. If the
    object_type does not exist, the return value is empty.
    
    @return table_name
  } {
    return [db_string [my qn get_table_name] {
      select table_name from acs_object_types where object_type = :object_type
    } -default ""]
  }

  ::xo::db::Class ad_proc object_type_exists_in_db {-object_type} {
    Check, if an object_type exists in the database.

    @return 0 or 1
  } {
    return [db_string [my qn check_type] {
      select 1 from acs_object_types where object_type = :object_type
    } -default 0]
  }

  ::xo::db::Class ad_proc drop_type {
    -object_type:required 
    {-drop_table f} 
    {-cascade_p t}
  } {
    Drop the object_type from the database and drop optionally the table.
    This method deletes as well all acs_objects of the object_type from the database. 
  } {
    set table_name [::xo::db::Class get_table_name -object_type $object_type]
    if {$table_name ne ""} {
      if {[catch {
	db_dml [my qn delete_instances] "delete from $table_name"
	if {$drop_table} {
	  db_dml [my qn drop_table] "drop table $table_name"
	}
      } errorMsg]} {
	my log "error during drop_type"
      }
    }
    ::xo::db::sql::acs_object_type drop_type \
	-object_type $object_type -cascade_p $cascade_p
    return ""
  }

  ::xo::db::Class ad_proc delete_all_acs_objects {-object_type:required} {
    Delete all acs_objects of the object_type from the database. 
  } {
    set table_name [::xo::db::Class get_table_name -object_type $object_type]
    if {$table_name ne ""} {
      db_dml delete_instances {delete from :table_name}
    }
  }

  # acs_attribute defines bio and bio_mime_type for object_type person, but
  # table persons does not have these attributes.
  #
  # select * from acs_attributes  where object_type = 'person';
  ::xo::db::Class array set exclude_attribute {persons,bio 1 persons,bio_mime_type 1}

  ::xo::db::Class ad_proc get_class_from_db {-object_type} {
    Fetch an acs_object_type from the database and create
    an XOTcl class from this information.

    @return class name of the created XOTcl class
  } {
    db_1row dbqd..fetch_class {
      select object_type, supertype, pretty_name, id_column, table_name  
      from acs_object_types where object_type = :object_type
    }
    set classname [my object_type_to_class $object_type]
    if {![my isclass $classname]} {
      # the XOTcl class does not exist, we create it
      #switch $supertype {
      #acs_object       {set superclass ::xo::db::Object}
      #content_revision {set superclass ::xo::db::CrItem}
      #default          {[my object_type_to_class $supertype]}
      #}
      #my log "creating class $classname superclass $superclass"
      ::xo::db::Class create $classname \
          -superclass [my object_type_to_class $supertype] \
          -object_type $object_type \
          -supertype $supertype \
          -pretty_name $pretty_name \
          -id_column $id_column \
          -table_name $table_name \
	  -noinit
    } else {
      #my log "we have a class $classname"
    }
    set attributes [db_list_of_lists dbqd..get_atts {
      select attribute_name, pretty_name, pretty_plural, datatype, 
      default_value, min_n_values, max_n_values
      from acs_attributes where object_type = :object_type
    }]
    
    set slots ""
    foreach att_info $attributes {
      foreach {attribute_name pretty_name pretty_plural datatype default_value
        min_n_values max_n_values} $att_info break

      # ignore some erroneous definitions in the acs meta model
      if {[my exists exclude_attribute($table_name,$attribute_name)]} continue

      set defined_att($attribute_name) 1
      set cmd [list ::xo::db::Attribute create $attribute_name \
		   -pretty_name $pretty_name \
		   -pretty_plural $pretty_plural \
		   -datatype $datatype \
		   -min_n_values $min_n_values \
		   -max_n_values $max_n_values]
      
      if {$default_value ne ""} {
	# if the default_value is "", we assume, no default
	lappend cmd -default $default_value
      }
      append slots $cmd \n
    }
    if {[catch {$classname slots $slots} errorMsg]} {
      error "Error during slots: $errorMsg"
    }

    $classname init
    return $classname
  }
  
  #
  # interface for stored procedures
  #

  # Some stored procedures like content_item__new do currently not
  # define null default values.  Therefore, we need - temporary - this
  # ugly redundancy to keep :required passing and to allow the xowiki
  # regression test to run.  The correct fix is to define the correct
  # default values in the database with define_function_args()

  ::xo::db::Class array set defaults {
    "content_item__new" {RELATION_TAG null DESCRIPTION null TEXT null 
      CREATION_IP null NLS_LANGUAGE null LOCALE null CONTEXT_ID null 
      DATA null TITLE null ITEM_ID null
    }
    "content_type__create_attribute" {
      DEFAULT_VALUE null SORT_ORDER null PRETTY_PLURAL null
    }
    "content_type__drop_type" {
      DROP_CHILDREN_P f DROP_TABLE_P f DROP_OBJECTS_P f
    }
  }
  
  ::xo::db::Class instproc sql-arguments {sql package_name object_name} {
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
      set prototype_args [[self class] set defaults(${package_name}__$object_name)]
      foreach {arg_name default_value} $prototype_args {
        if {![my exists defined($arg_name)]} {
          lappend psql_args \$_$arg_name
          my lappend arg_order $arg_name
        }
      }
      my array set defined $prototype_args
    }
    return [join $psql_args ", "]
  }
  
  ::xo::db::Class instproc psql-postgresql {package_name object_name full_statement_name} {
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
  
  ::xo::db::Class instproc psql-oracle {package_name object_name full_statement_name} {
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

  ::xo::db::Class instproc proc_body-postgresql {} {
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

  ::xo::db::Class instproc proc_body-oracle {} {
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

  ::xo::db::Class instproc dbproc_nonposargs {object_name} {
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

  ::xo::db::Class instproc unknown {m args} {
    error "Error: unknown database method '$m' for [self]"
  }
 
  ::xo::db::Class proc create_all_functions {} {
    db_foreach [my qn ""] [::xo::db::sql set all_package_functions] {
      #if {![my isobject $package_name]} { ::xo::db::Class create $package_name }
      #$package_name dbproc_exportvars $object_name
      set class_name ::xo::db::sql::[string tolower $package_name] 
      if {![my isobject $class_name]} { ::xo::db::Class create $class_name }
      $class_name dbproc_nonposargs [string tolower $object_name]
    }
  }
  
  ::xo::db::Class proc class_to_object_type {name} {
    if {[my isclass $name]} {
      return [$name object_type]
    }
    switch --glob -- $name {
      ::xo::db::Object {return acs_object}
      ::xo::db::CrItem {return content_revision}
      ::xo::db::*      {return [string range $name 10 end]}
      default          {return $name}
    }
  }

  ::xo::db::Class proc object_type_to_class {name} {
    switch -glob -- $name {
      acs_object       {return ::xo::db::Object}
      content_revision {return ::xo::db::CrItem}
      ::*              {return $name}
      default          {return ::xo::db::$name}
    }
  }

  #
  # now, create all stored procedures in postgres or Oracle
  #
  ::xo::db::Class create_all_functions
  
  #
  # Methods for instances of the meta class (methods for object_types)
  #
  if {[db_driverkey ""] eq "postgresql"} {
    #
    # Postgres
    #
    ::xo::db::Class instproc object_types_query {
      {-subtypes_first:boolean false}
    } {
      my instvar object_type_key
      set order_clause [expr {$subtypes_first ? "order by tree_sortkey desc":""}]
      return "select object_type from acs_object_types where 
        tree_sortkey between '$object_type_key' and tree_right('$object_type_key')
        $order_clause"
    }
    ::xo::db::Class instproc init_type_hierarchy {} {
      my instvar object_type
      my set object_type_key [db_list [my qn get_tree_sortkey] {
        select tree_sortkey from acs_object_types 
        where object_type = :object_type
      }]
    }
  } else {
    #
    # Oracle
    #
    ::xo::db::Class instproc object_types_query {
      {-subtypes_first:boolean false}
    } {
      my instvar object_type
      set order_clause [expr {$subtypes_first ? "order by LEVEL desc":""}]
      return "select object_type from acs_object_types 
        start with object_type = '$object_type' 
        connect by prior object_type = supertype $order_clause"
    }
    ::xo::db::Class instproc init_type_hierarchy {} {
      my set object_type_key {}
    }
  }

  ::xo::db::Class ad_instproc object_types {
    {-subtypes_first:boolean false}
  } {
    Return the type and subtypes of the class, on which
    the method is called. If subtypes_first is specified,
    the subtypes are returned first.
    
    @return list of object_types
  } {
    return [db_list [my qn get_object_types] \
		[my object_types_query -subtypes_first $subtypes_first]]
  }

  ::xo::db::Class ad_instproc create_object_type {} {
    Create an acs object_type for the current XOTcl class
  } {
    my instvar object_type supertype pretty_name pretty_plural \
        table_name id_column name_method abstract_p
    
    my check_table_atts

    # The default supertype is acs_object. If the supertype
    # was not changed, we map the class to the object_type. 
    if {$supertype ne "acs_object"} {
      set supertype [my class_to_object_type [my info superclass]]
    }
    if {![info exists pretty_name]}   {set pretty_name [namespace tail [self]]}
    if {![info exists pretty_plural]} {set pretty_plural $pretty_name}

    ::xo::db::sql::acs_object_type create_type \
        -object_type $object_type \
        -supertype $supertype \
        -pretty_name $pretty_name \
        -pretty_plural $pretty_plural \
        -table_name $table_name \
        -id_column $id_column \
        -abstract_p $abstract_p \
        -name_method $name_method
  }
  
  ::xo::db::Class ad_instproc drop_object_type {{-cascade true}} {
    Drop an acs object_type; cascde true means that the attributes
    are droped as well.
  } {
    my instvar object_type 
    ::xo::db::sql::acs_object_type drop_type \
        -object_type $object_type \
        -cascade_p [expr {$cascade ? "t" : "f"}]
  }

  ::xo::db::Class instproc db_slots {} {
    my instvar id_column db_slot
    array set db_slot [list]
    #
    # First get all ::xo::db::Attribute slots and check later, 
    # if we have to add the id_column automatically.
    #
    my log "--setting db_slot all=[my info slots]"
    foreach att [my info slots] {
      my log "--checking $att [$att istype ::xo::db::Attribute] [$att info class]"
      if {![$att istype ::xo::db::Attribute]} continue
      set db_slot([$att name]) $att
    }
    if {[self] ne "::xo::db::Object"} {
      if {[my exists id_column] && ![info exists db_slot($id_column)]} {
	# create automatically the slot for the id column
	my slots [subst {
	  ::xo::db::Attribute create $id_column \
	      -pretty_name "ID" \
	      -datatype integer 
	}]
	set db_slot($id_column) [self]::slot::$id_column
      }
    }
    my log "--setting db_slot of [self] to [array names db_slot]"
  }

  ::xo::db::Class instproc table_definition {} {
    my instvar id_column table_name db_slot
    array set column_specs [list]
    #
    # iterate over the slots and collect the column_specs for table generation
    #
    foreach {slot_name slot} [my array get db_slot] {
      set column_name [$slot column_name]
      set column_specs($column_name) \
	  [$slot column_spec -id_column [expr {$column_name eq $id_column}]]
    }

    if {[array size column_specs]>0} {
      if {$table_name eq ""} {error "no table_name specified"}
      if {$id_column eq ""}  {error "no id_column specified"}
      if {![info exists column_specs($id_column)]} {
	error "no ::xo::db::Attribute slot for id_column '$id_column' specified"
      }
      set table_specs [list]
      foreach {att spec} [array get column_specs] {lappend table_specs "    $att $spec"}
      set table_definition [join $table_specs ",\n"]
    } else {
      set table_definition ""
    }
    # my log table_definition=$table_definition
    return $table_definition
  }

  ::xo::db::Class instproc mk_save_method {} {
    set updates [list]
    set vars [list]
    foreach {slot_name slot} [my array get db_slot] {
      $slot instvar name column_name
      if {$column_name ne [my id_column]} {
	lappend updates "$column_name = :$name"
	lappend vars $name
      }
    }
    if {[llength $updates] == 0} return
    my instproc save {} [subst {
      db_transaction {
	next
	my instvar object_id $vars
	db_dml dbqd..update_[my table_name] {update [my table_name]
	  set [join $updates ,] where [my id_column] = :object_id
	}
      }
    }]
  }

  ::xo::db::Class instproc mk_insert_method {} {
    # create method 'insert' for the application class
    # The caller (e.g. method new) should care about db_transaction
    my instproc insert {} {
      set __table_name [[self class] table_name]
      set __id [[self class] id_column]
      my set $__id [my set object_id]
      my log "ID insert in $__table_name, id = $__id = [my set $__id]"
      next
      foreach {__slot_name __slot} [[self class] array get db_slot] {
	my instvar $__slot_name
	if {[info exists $__slot_name]} { 
	  lappend __vars $__slot_name
	  lappend __atts [$__slot column_name]
	}
      }
      db_dml dbqd..insert_$__table_name "insert into $__table_name
	    ([join $__atts ,]) values (:[join $__vars ,:])"
    }
  }
  
  ::xo::db::Class ad_instproc check_table_atts {} {
    Check table_name and id_column and set meaningful
    defaults, if these attributes are not provided.
  } {
    if {![my exists table_name]} {
      if {[regexp {^::([^:]+)::} [self] _ head]} {
	set tail [namespace tail [self]]
	my set table_name [string tolower ${head}_$tail]
	#my log "created table_name '[my table_name]'"
      } else {
	error "Cannot determine automatically table name for class [self]. \
		Use namespaces for classes."
      }
    }
    if {![my exists id_column]} {
      my set id_column [string tolower [namespace tail [self]]]_id
    }
  }

  ::xo::db::Class instproc init {} {

    if {![::xo::db::Class object_type_exists_in_db -object_type [my object_type]]} {
      my create_object_type
    }
    my init_type_hierarchy
    my db_slots

    if {[my with_table]} {
      my check_table_atts
      set table_definition [my table_definition]
      if {$table_definition ne ""} {
	::xo::db::require table [my table_name] $table_definition
      }
      
      my mk_save_method
      my mk_insert_method
    }
    next
  }

  ::xo::db::Class instproc get_context {package_id_var user_id_var ip_var} {
    my upvar \
	$package_id_var package_id \
	$user_id_var user_id \
	$ip_var ip

    if {![info exists package_id]} {
      if {[info command ::xo::cc] ne ""} {
	set package_id    [::xo::cc package_id]
      } elseif {[ns_conn isconnected]} {
        set package_id    [ad_conn package_id]
      } else {
        set package_id ""
      }
    }
    if {![info exists user_id]} {
      if {[info command ::xo::cc] ne ""} {
	set user_id    [::xo::cc user_id]
      } elseif {[ns_conn isconnected]} {
        set user_id    [ad_conn user_id]
      } else {
        set user_id 0
      }
    }
    if {![info exists ip]} {
      if {[ns_conn isconnected]} {
	set ip [ns_conn peeraddr]
      } else {
	set ip [ns_info address]
      }
    }
  }

  ::xo::db::Class instproc new_acs_object {
    -package_id 
    -creation_user 
    -creation_ip 
    {object_title ""}
  } {
    my get_context package_id creation_user creation_ip

    set id [::xo::db::sql::acs_object new \
		-object_type [::xo::db::Class class_to_object_type [self]] \
                -title $object_title \
                -package_id $package_id \
                -creation_user $creation_user \
                -creation_ip $creation_ip \
                -security_inherit_p [my security_inherit_p]]
    return $id
  }

  ::xo::db::Class instproc initialize_acs_object {obj id} {
    $obj set object_id $id
    # construct the same object_title as acs_object.new() does
    $obj set object_title "[my pretty_name] $id"
    #$obj set object_type [my object_type]    
  }

  ::xo::db::Class ad_instproc new_persistent_object {
    -package_id 
    -creation_user 
    -creation_ip 
    args
  } {
    Create a new instance of the given class,
    configure it with the given arguments and 
    insert it into the database.  The XOTcl object is
    destroyed automatically on cleanup (end of a connection request).

    @return fully qualified object
  } {
    my get_context package_id creation_user creation_ip
    db_transaction {
      set id [my new_acs_object \
		  -package_id $package_id \
		  -creation_user $creation_user \
		  -creation_ip $creation_ip \
		  ""]
      #[self class] set during_fetch 1
      if {[catch {eval my create ::$id $args} errorMsg]} {
	my log "Error: $errorMsg, $::errorInfo"
      }
      #[self class] unset during_fetch
      my initialize_acs_object ::$id $id
      ::$id insert
    }
    ::$id destroy_on_cleanup
    return ::$id
  }


  ##################
  # query interface
  ##################

  ::xo::db::Class ad_instproc instantiate_objects {
    {-dbn ""}
    {-sql ""}
    {-full_statement_name ""}
  } {
    Return a set of objects where each object is a tuple of the
    answer-set of the SQL query. This method creates 
    plain objects of the type of the specified class 
    (default ::xotcl::Object) containing the variables that
    the SQL query returns.

    The container and contained objects are automatically 
    destroyed on cleanup of the connection thread.
  } {
    set __result [::xo::OrderedComposite new -destroy_on_cleanup]
    #$__result proc destroy {} {my log "-- "; next}

    db_with_handle -dbn $dbn db {
      set selection [db_exec select $db $full_statement_name $sql]
      while {1} {
        set continue [ns_db getrow $db $selection]
        if {!$continue} break
        set o [::xotcl::Object new]
        foreach {att val} [ns_set array $selection] {$o set $att $val}

        if {[$o exists object_type]} {
          # set the object type if it looks like from xotcl
          if {[string match "::*" [set ot [$o set object_type]] ]} {
            $o class $ot
          }
        }
        #my log "--DB more = $continue [$o serialize]" 
        $__result add $o
      }
    }
    return $__result
  }
 
  ::xo::db::Class instproc fetch_query {id} {
    set tables [list]
    set attributes [list]
    set id_column [my id_column]
    set join_expressions [list "$id_column = $id"]
    foreach cl [concat [self] [my info heritage]] {
      #if {$cl eq "::xo::db::Object"} break
      if {$cl eq "::xotcl::Object"} break
      set tn [$cl table_name]
      if {$tn  ne ""} {
        lappend tables $tn
	my log "--db_slots of $cl = [$cl array get db_slot]"
	foreach {slot_name slot} [$cl array get db_slot] {
	  lappend attributes [$slot attribute_reference $tn]
	}
        if {$cl ne [self]} {
          lappend join_expressions "[$cl id_column] = $id_column"
        }
      }
    }
    return "SELECT [join $attributes ,]\nFROM [join $tables ,]\nWHERE [join $join_expressions { and }]"
  }

  ::xo::db::Class ad_instproc instance_select_query {
    {-select_attributes ""}
    {-orderby ""}
    {-where_clause ""}
    {-from_clause ""}
    {-count:boolean false}
    {-page_size 20}
    {-page_number ""}
  } {
    Returns the SQL-query to select ACS Objects of the object_type 
    of the class.
    @select_attributes attributes for the SQL query to be retrieved.
      if no attributes are specified, all attributes are retrieved.
    @param orderby for ordering the solution set
    @param where_clause clause for restricting the answer set
    @param count return the query for counting the solutions
    @return SQL query
  } {
    set tables [list]
    set id_column [my id_column]

    if {$count} {
      set select_attributes "count(*)"
      set orderby ""         ;# no need to order when we count
      set page_number  ""    ;# no pagination when count is used
    } 

    set all_attributes [expr {$select_attributes eq ""}]
    set join_expressions [list]
    foreach cl [concat [self] [my info heritage]] {
      #if {$cl eq "::xo::db::Object"} break
      if {$cl eq "::xotcl::Object"} break
      set tn [$cl table_name]
      if {$tn  ne ""} {
        lappend tables $tn
	if {$all_attributes} {
	  foreach {slot_name slot} [$cl array get db_slot] {
	    lappend select_attributes [$slot attribute_reference $tn]
	  }
	}
        if {$cl ne [self]} {
          lappend join_expressions "[$cl id_column] = $id_column"
        }
      }
    }

    if {$page_number ne ""} {
      set limit $page_size
      set offset [expr {$page_size*($page_number-1)}]
    } else {
      set limit ""
      set offset ""
    }

    set sql [::xo::db::sql select \
		 -vars   [join $select_attributes ,] \
		 -from  "[join $tables ,] $from_clause" \
		 -where  [string trim "[join $join_expressions { and }] $where_clause"] \
		 -orderby $orderby \
		 -limit $limit -offset $offset]
    return $sql
  }

  ::xo::db::Class ad_instproc get_instances_from_db {
    {-select_attributes ""}
    {-from_clause ""}
    {-where_clause ""}
    {-orderby ""}
    {-page_size 20}
    {-page_number ""}
  } {
    Returns a set (ordered composite) of the answer tuples of 
    an 'instance_select_query' with the same attributes. Note, that
    the returned objects might by partially instantiated.

    @return ordered composite
  } {
    set s [my instantiate_objects -sql \
	       [my instance_select_query \
		    -select_attributes $select_attributes \
		    -from_clause $from_clause \
		    -where_clause $where_clause \
		    -orderby $orderby \
		    -page_size $page_size \
		    -page_number $page_number \
		   ]]
    return $s
  }
  ##############

  ::xo::db::Class create ::xo::db::Object \
      -superclass ::xotcl::Object \
      -object_type "acs_object" \
      -pretty_name "Object" \
      -pretty_plural "Objects" \
      -table_name "acs_objects" -id_column "object_id"

  ::xo::db::Object instproc insert {} {my log no-insert;}

  ::xo::db::Object ad_instproc delete {} {
    Delete the object from the database and from memory
  } {
    ::xo::db::sql::acs_object delete -object_id [my set object_id]
    my destroy
  }

  ::xo::db::Object ad_instproc save {-package_id -modifying_user} {
    Save the current object in the database
  } {
    my instvar object_id
    if {![info exists package_id] && [my exists package_id]} {
      set package_id [my package_id]
    }
    [my info class] get_context package_id modifying_user modifying_ip
    db_dml dbqd..update_object {update acs_objects 
      set modifying_user = :modifying_user, modifying_ip = :modifying_ip
      where object_id = :object_id}
  }

  ::xo::db::Object ad_instproc save_new {
    -package_id -creation_user -creation_ip
  } {
    Save the XOTcl Object with a fresh acs_object 
    in the database.

    @return new object id
  } {
    if {![info exists package_id] && [my exists package_id]} {
      set package_id [my package_id]
    }
    [my info class] get_context package_id creation_user creation_ip
    db_transaction {
      set id [[my info class] new_acs_object \
		  -package_id $package_id \
		  -creation_user $creation_user \
		  -creation_ip $creation_ip \
		  ""]
      [my info class] initialize_acs_object [self] $id
      my insert
    }
    return $id
  }

  ##############
  ::xo::db::Class create ::xo::db::Attribute \
      -superclass {::xo::Attribute} \
      -pretty_name "Attribute" \
      -with_table false \
      -parameter {
        {sqltype} 
        {column_name} 
        {references ""}
        {min_n_values 1} 
        {max_n_values 1}
      }

  ::xo::db::Attribute instproc create_attribute {} {
    my instvar name datatype pretty_name min_n_values max_n_values domain
    set object_type [$domain object_type]
    if {[db_string dbqd..check_att {select 0 from acs_attributes where 
      attribute_name = :name and object_type = :object_type} -default 1]} {

      if {![::xo::db::Class object_type_exists_in_db -object_type $object_type]} {
	$domain create_object_type
      }

      ::xo::db::sql::acs_attribute create_attribute \
          -object_type $object_type \
          -attribute_name $name \
          -datatype $datatype \
          -pretty_name $pretty_name \
          -min_n_values $min_n_values \
          -max_n_values $max_n_values
      #my save
    }
  }

  ::xo::db::Attribute instproc attribute_reference {tn} {
    my instvar column_name name
    if {$column_name ne $name} {
      return "$tn.$column_name AS $name"
    } else {
      return "$tn.$name"
    }
  }
  
  ::xo::db::Attribute instproc column_spec {{-id_column false}} {
    my instvar sqltype name references default
    set column_spec ""
    append column_spec " " [::xo::db::sql map_datatype $sqltype]
    if {[info exists default]} {append column_spec " DEFAULT '$default'" }
    #
    # References
    #
    if {[info exists references] && $references ne ""} {
      append column_spec " REFERENCES $references" 
    } elseif {$id_column} {
      set sc [[my domain] info superclass]
      #todo: 2x set not necessary (critem) 
      append column_spec " REFERENCES [$sc set table_name]([$sc set id_column])\
		ON DELETE CASCADE"
    }
    #
    # Constraints
    #
    set table_name [[my domain] table_name]
    if {$id_column} {
      # add automatically a constraints for the id_column
      set cname [::xo::db::mk_sql_constraint_name $table_name $name pk]
      append column_spec "\n\tCONSTRAINT $cname PRIMARY KEY"
    }
    append column_spec " " [::xo::db::sql datatype_constraint $sqltype $table_name $name]
    return $column_spec
  }
  
  ::xo::db::Attribute instproc init {} {
    next    ;# do first ordinary slot initialization
    my instvar datatype name
    if {![my exists sqltype]} {my set sqltype $datatype}
    if {![my exists column_name]} {my set column_name $name}

    my create_attribute
  }

  ##############
  ::xo::db::Class create ::xo::db::CrAttribute \
      -superclass {::xo::db::Attribute} \
      -pretty_name "Cr Attribute" \
      -with_table false \
      -parameter {
	{create_acs_attribute true}
      }

  ::xo::db::CrAttribute instproc create_attribute {} {
    # do nothing, if create_acs_attribute is set to false
    if {![my create_acs_attribute]} return

    my instvar name column_name datatype pretty_name domain
    set object_type [$domain object_type]

    if {[db_string dbqd..check_att {select 0 from acs_attributes where 
      attribute_name = :name and object_type = :object_type} -default 1]} {

      if {![::xo::db::Class object_type_exists_in_db -object_type $object_type]} {
	$domain create_object_type
      }

      ::xo::db::sql::content_type create_attribute \
	  -content_type $object_type \
	  -attribute_name $column_name \
	  -datatype $datatype \
	  -pretty_name $pretty_name \
	  -column_spec [my column_spec]
    }
  }


  ##############
  ::xo::db::Object slots {
    ::xo::db::Attribute create object_id    -pretty_name "Object ID" -sqltype integer
    #::xo::db::Attribute create object_type  -pretty_name "Object Type"
    ::xo::db::Attribute create object_title -pretty_name "Object Title" -column_name title
  }
  ::xo::db::Object db_slots
  ##############

 
  ad_proc tcl_date {timestamp tz_var} {
    Convert the time stamp (coming from the database) into a format, which
    can be passed to Tcl's "clock scan".
  } {
    upvar $tz_var tz
    set tz 00
    regexp {^([^.]+)[.][0-9]+(.*)$} $timestamp _ timestamp tz
    return $timestamp
  }
}


