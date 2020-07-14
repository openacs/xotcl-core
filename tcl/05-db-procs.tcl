::xo::library doc {

  XOTcl API for low-level db abstraction

  @author Gustaf Neumann
  @creation-date 2006-12-28
  @cvs-id $Id$
}

namespace eval ::xo::db {

  ##########################################################################
  #
  # XOTcl based Database Abstraction Layer
  #
  # The communication to the database is determined by
  # - the SQL Dialect
  # - the database driver
  #
  # The following classes define means to compose the behavior in
  # connection objects based on these two aspects. The default
  # database connection is configured in an object ::xo::dc (for
  # database context) quite similar to ::xo::cc (the default
  # connection context). In general ::xo::dc can be reconfigured at
  # run time, and multiple database context can be established,
  # although there is no high-level support to connect to multiple
  # different OpenACS databases at the same time.
  #
  ##########################################################################

  #
  # Backend language specific (SQL Dialects)
  #
  ::xotcl::Class create ::xo::db::SQL
  ::xo::db::SQL abstract instproc select {type}
  ::xo::db::SQL abstract instproc date_trunc {type}
  ::xo::db::SQL abstract instproc date_trunc_expression {type}

  #
  # generic (fallback) methods
  #
  ::xo::db::SQL instproc map_datatype {type} {
    # If a mapping is not found we keep the type unaltered, but this
    # will currently break acs_attributes_datatype_fk when creating
    # acs_attributes with an unmapped type.
    return [::xo::dc get_value map "
     select database_type from acs_datatypes
      where datatype = :type" $type]
  }
  ::xo::db::SQL instproc map_function_name        {sql} {return $sql}
  ::xo::db::SQL instproc datatype_constraint      {type table att} {return ""}
  ::xo::db::SQL instproc interval {interval} {
    return [clock format [clock scan "-$interval"] -format "%Y-%m-%d %T"]
  }
  ::xo::db::SQL instproc since_interval_condition {var interval} {
    set since '[clock format [clock scan "-$interval"] -format "%Y-%m-%d %T"]'
    return "$var > TO_TIMESTAMP($since,'YYYY-MM-DD HH24:MI:SS')"
  }
  ::xo::db::SQL instproc has_ltree {} {return 0}
  ::xo::db::SQL instproc has_hstore {} {return 0}
  ::xo::db::SQL instproc mk_sql_constraint_name {table att suffix} {
    return ${table}_${att}_$suffix
  }


  ##########################################################################
  #
  # PostgreSQL specific methods
  #
  ##########################################################################

  ::xotcl::Class create ::xo::db::postgresql -superclass ::xo::db::SQL

  ::xo::db::postgresql instproc map_datatype {type} {
    switch -- $type {
      string    { set type text }
      long_text { set type text }
      date      { set type "timestamp with time zone" }
      ltree     { set type [expr {[:has_ltree] ? "ltree" : "text" }] }
      default   { return [next] }
    }
    return $type
  }

  ::xo::db::postgresql instproc select {
    -vars:required
    -from:required
    {-where ""}
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
    return "SELECT $vars FROM $from $where_clause $group_clause $order_clause $limit_clause $offset_clause"
  }

  ::xo::db::postgresql instproc date_trunc {field date} {
    return "date_trunc('$field',$date)"
  }
  ::xo::db::postgresql instproc date_trunc_expression {field date date_string} {
    if {![string match :* $date_string]} {set date_string "'$date_string'"}
    return "date_trunc('$field',$date) = $date_string"
  }

  ::xo::db::postgresql instproc has_ltree {} {
    ::xo::xotcl_package_cache eval [self]::has_ltree {
      if {[:get_value check_ltree "select count(*) from pg_proc where proname = 'ltree_in'"] > 0} {
        return 1
      }
      return 0
    }
  }
  ::xo::db::postgresql instproc has_hstore {} {
    ::xo::xotcl_package_cache eval [self]::has_hstore {
      if {[:get_value check_ltree "select count(*) from pg_proc where proname = 'hstore_in'"] > 0} {
        return 1
      }
      return 0
    }
  }

  namespace eval ::db {}
  ::xo::db::postgresql instproc nextval {sequence} {
    if {![info exists ::db::sequences]} {
      ns_log notice "-- creating per thread sequence table"
      foreach s [::xo::dc list relnames "select relname from pg_class where relkind = 'S'"] {
        set ::db::sequences($s) 1
      }
    }
    if {[info exists ::db::sequences(t_$sequence)]} {
      #ns_log notice "-- found t_$sequence"
      set sequenceName t_$sequence
      set nextval [::xo::dc get_value nextval "select nextval(:sequenceName)"]
    } elseif {[info exists ::db::sequences($sequence)]} {
      #ns_log notice "-- found $sequence"
      set sequenceName $sequence
      set nextval [::xo::dc get_value nextval "select nextval(:sequenceName)"]
    } elseif { [::xo::dc db_0or1row nextval_sequence {
      select nextval(:sequence) as nextval
      where (select relkind
             from pg_class
             where relname = :sequence) = 'S'
    }]} {
      #
      # We do not have an according sequence-table. Use the system catalog to check
      # for the sequence
      #
      # ... the query sets nextval if it succeeds
      #
    } else {
      #
      # finally, there might be a view with a nextval
      #
      ns_log debug "db_nextval: sequence($sequence) is not a real sequence.  Perhaps it uses the view hack."
      set nextval [::xo::dc get_value nextval "select nextval from :sequence"]
    }
    return $nextval
  }


  ##########################################################################
  #
  # Oracle specific methods
  #
  ##########################################################################

  ::xotcl::Class create ::xo::db::oracle -superclass ::xo::db::SQL

  ::xo::db::oracle instproc map_function_name {sql} {
    return [string map [list "__" .] $sql]
  }

  ::xo::db::oracle instproc limit_clause {
    {-sql}
    {-limit ""}
    {-offset ""}
  } {
    if {$limit ne "" || $offset ne ""} {
      if {$offset eq ""} {
        #
        # Only a limit is given.
        #
        set limit_clause "ROWNUM <= $limit"
      } elseif {$limit eq ""} {
        #
        # Only an offset is given.
        #
        set limit_clause "ROWNUM >= $offset"
      } else {
        #
        # Both, an offset and limit is given.
        #
        set limit_clause "ROWNUM BETWEEN $offset and [expr {$offset+$limit}]"
      }
      # for pagination, we will need an "inner" sort, such as
      # SELECT * FROM (SELECT ...., ROW_NUMBER() OVER (ORDER BY ...) R FROM table) WHERE R BETWEEN 0 and 100
      set sql "SELECT * FROM ($sql) WHERE $limit_clause"
    }
    return $sql
  }

  ::xo::db::oracle instproc map_datatype {type} {
    switch -- $type {
      string    { set type varchar2(1000) }
      text      { set type varchar2(4000) }
      long_text { set type clob }
      boolean   { set type char(1) }
      ltree     { set type varchar2(1000) }
      default   { return [next] }
    }
    return $type
  }

  ::xo::db::oracle instproc datatype_constraint {type table att} {
    set constraint ""
    switch -- $type {
      boolean {
        set cname [:mk_sql_constraint_name $table $att _ck]
        set constraint "constraint $cname check ($att in ('t','f'))"}
    }
    return $constraint
  }

  ::xo::db::oracle instproc select {
    -vars:required
    -from:required
    {-where ""}
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
    if {$map_function_names} {set vars [:map_function_name $vars]}
    set sql "SELECT $vars FROM $from $where_clause $group_clause"
    if {$limit ne "" || $offset ne ""} {
      set sql [:limit_clause -sql "$sql $order_clause" -limit $limit -offset $offset]
    } else {
      append sql " " $order_clause
    }
    :log "--returned sql = $sql"
    return $sql
  }
  ::xo::db::oracle instproc date_trunc {field date} {
    return "to_char(trunc($date,'$field'), 'YYYY-MM-DD HH24:MI:SS')"
  }
  ::xo::db::oracle instproc date_trunc_expression {field date date_string} {
    if {![string match :* $date_string]} {set date_string "'$date_string'"}
    return "trunc($date,'$field') = trunc(to_date($date_string,'YYYY-MM-DD'),'$field')"
  }
  ::xo::db::oracle instproc mk_sql_constraint_name {table att suffix} {
    #
    # Constraint names are limited in oracle to 30 characters;
    # PostgreSQL has no such limits. Therefore, we use different
    # rules depending on whether we are running under Oracle or not.
    #
    set name ${table}_${att}_$suffix
    if {[string length $name] > 30} {
      set sl [string length $suffix]
      set name [string range ${table}_${att}  0 [expr {28 - $sl}]]_$suffix
    }
    return [string toupper $name]
  }

  ::xo::db::oracle instproc nextval {sequence} {
    return [xo::dc get_value nextval "select $sequence.nextval from dual"]
  }

  ##########################################################################
  #
  # Database Driver
  #
  # Abstract form the Tcl interface that the drivers are offering to
  # issue SQL commands and to perform profiling.
  #

  ::xotcl::Class create ::xo::db::Driver -parameter dialect
  ::xo::db::Driver abstract instproc sets           {{-dbn ""} {-bind ""} -prepare qn sql}
  ::xo::db::Driver abstract instproc 0or1row        {{-dbn ""} {-bind ""} -prepare qn sql}
  ::xo::db::Driver abstract instproc 1row           {{-dbn ""} {-bind ""} -prepare qn sql}
  ::xo::db::Driver abstract instproc get_value      {{-dbn ""} {-bind ""} -prepare qn sql {default ""}}
  ::xo::db::Driver abstract instproc list_of_lists  {{-dbn ""} {-bind ""} -prepare qn sql}
  ::xo::db::Driver abstract instproc list           {{-dbn ""} {-bind ""} -prepare qn sql}
  ::xo::db::Driver abstract instproc dml            {{-dbn ""} {-bind ""} -prepare qn sql}
  ::xo::db::Driver abstract instproc foreach        {{-dbn ""} {-bind ""} -prepare qn sql {script}}
  ::xo::db::Driver abstract instproc transaction    {{-dbn ""} script args}
  ::xo::db::Driver abstract instproc ds {onOff}
  ::xo::db::Driver abstract instproc prepare        {-handle {-argtypes ""} sql}

  #
  # Driver specific and Driver/Dialect specific hooks
  #
  ::xotcl::Class create ::xo::db::DB             -superclass ::xo::db::Driver
  ::xotcl::Class create ::xo::db::DB-postgresql  -superclass {::xo::db::DB ::xo::db::postgresql}
  ::xotcl::Class create ::xo::db::DB-oracle      -superclass {::xo::db::DB ::xo::db::oracle}

  ::xotcl::Class create ::xo::db::DBI            -superclass ::xo::db::Driver
  ::xotcl::Class create ::xo::db::DBI-postgresql -superclass {::xo::db::DBI ::xo::db::postgresql}

  ::xo::db::Driver instproc get_sql {{-dbn ""} qn} {
    set full_statement_name [db_qd_get_fullname $qn 2]
    set full_query [db_qd_fetch $full_statement_name $dbn]
    set sql [db_fullquery_get_querytext $full_query]
    :uplevel 2 [list subst $sql]
  }


  ##########################################################################
  #
  # DBI support
  #
  ::xo::db::DBI instproc profile {onOff} {
    if {$onOff} {
      :mixin ::xo::db::DBI::Profile
    } else {
      if {[:info mixin] ne ""} {:mixin ""}
    }
  }

  ::xo::db::DBI instproc sets {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [:uplevel [list dbi_rows -result sets {*}$bindOpt -- $sql]]
  }

  #
  # foreach based on "dbi_rows + results avlists"
  #
  ::xo::db::DBI instproc foreach {{-dbn ""} {-bind ""} -prepare qn sql body} {
    #if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$sql eq ""} {set qn [uplevel [list [self] qn $qn]]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set avlists [:uplevel [list dbi_rows -result avlists {*}$bindOpt -- $sql]]
    foreach avlist $avlists {
      foreach {a v} $avlist {:uplevel [list set $a $v]}
      :uplevel $body
    }
  }
  #
  # foreach based on "dbi_eval"
  #
  #::xo::db::DBI instproc foreach {{-dbn ""} {-bind ""} -prepare qn sql body} {
  #  if {$sql eq ""} {set sql [:get_sql $qn]}
  #  if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
  #  :uplevel [list dbi_foreach $sql $body]
  #}

  ::xo::db::DBI instproc 0or1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [:uplevel [list ::dbi_0or1row {*}$bindOpt $sql]]
  }
  ::xo::db::DBI instproc 1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [:uplevel [list ::dbi_1row {*}$bindOpt $sql]]
  }
  ::xo::db::DBI instproc list_of_lists {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [:uplevel [list ::dbi_rows -result lists -max 1000000 {*}$bindOpt -- $sql]]
  }
  ::xo::db::DBI instproc list {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set flat [:uplevel [list ::dbi_rows -columns __columns {*}$bindOpt -- $sql]]
    if {[:uplevel {llength $__columns}] > 1} {error "query is returning more than one column"}
    return $flat
  }
  ::xo::db::DBI instproc dml {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [:uplevel [list ::dbi_dml {*}$bindOpt -- $sql]]
  }
  ::xo::db::DBI instproc transaction {{-dbn ""} script args} {
    if {$args ne ""} {
      lassign $args op on_error_code
      set result ""
      if {$op ne "on_error"} {
        error "only 'on_error' as argument after script allowed"
      }
      try {
        set result [:uplevel [list ::dbi_eval -transaction committed $script]]
      } on error {$errorMsg} {
        :uplevel $on_error_code
      }
      return $result
    } else {
      return [:uplevel [list ::dbi_eval -transaction committed $script]]
    }
  }
  ::xo::db::DBI instproc prepare {-handle {-argtypes ""} sql} {
    return $sql
  }
  ::xo::db::DBI instproc get_value {{-dbn ""} -prepare qn sql {default ""}} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    set answers [:uplevel [list ::dbi_rows -result sets -max 1 $sql]]
    if {$answers ne ""} {
      set result [ns_set value $answers 0]
      ns_set free $answers
      return $result
    }
    return $default
  }

  #
  # DBI profiling with developer support
  #
  ::xotcl::Class create ::xo::db::DBI::Profile

  foreach call {sets 0or1row 1row list_of_lists list dml} {

    ::xo::db::DBI::Profile instproc $call {{-dbn ""} qn sql} {
      if {$sql eq ""} {set sql [:get_sql $qn]}
      set start_time [expr {[clock clicks -microseconds]/1000.0}]
      set result [next]
      ds_add db $dbn [:ds_map [self proc]] $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
      return $result
    }
  }

  #
  # foreach based on "dbi_rows + results avlists"
  #
  ::xo::db::DBI::Profile instproc foreach {{-dbn ""} {-bind ""} -prepare qn sql body} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set start_time [expr {[clock clicks -microseconds]/1000.0}]
    set avlists [:uplevel [list dbi_rows -result avlists {*}$bindOpt -- $sql]]
    ds_add db $dbn "exec foreach" $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
    foreach avlist $avlists {
      foreach {a v} $avlist {:uplevel [list set $a $v]}
      :uplevel $body
    }
  }

  #
  # foreach based on "dbi_foreach"
  #
  #::xo::db::DBI::Profile instproc foreach {{-dbn ""} {-bind ""} -prepare qn sql body} {
  #  if {$sql eq ""} {set sql [:get_sql $qn]}
  #  if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
  #  set start_time [expr {[clock clicks -microseconds]/1000.0}]
  #  set result [next]
  #  ds_add db $dbn "exec [self proc]" $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
  #  return $result
  #}

  ::xo::db::DBI::Profile instproc ds_map {name} {
    if {$name in {dml exec 1row 0or1row select}} {return $name}
    return "exec $name"
  }

  # The following should not be necessary, but there seems to be a bad
  # interaction when "ns_cache eval" calls 1row with a mixin, doing a
  # :uplevel (the mixin should be transparent). Without "ns_cache eval"
  # things look fine.
  ::xo::db::DBI::Profile instproc 1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    set start_time [expr {[clock clicks -microseconds]/1000.0}]
    set result [:uplevel [list ::dbi_1row $sql]]
    ds_add db $dbn [:ds_map [self proc]] $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
    return $result
  }


  ##########################################################################
  #
  # DB support
  #
  ::xo::db::DB instproc profile {onOff} {
    # built-in
  }

  ::xo::db::DB instproc transaction {{-dbn ""} script args} {
    return [:uplevel [list ::db_transaction -dbn $dbn $script {*}$args]]
  }
  ::xo::db::DB instproc prepare {-handle {-argtypes ""} sql} {
    return $sql
  }

  ::xo::db::DB instproc sets {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    db_with_handle -dbn $dbn db {
      if {[info exists prepare]} {set sql [:prepare -handle $db -argtypes $prepare $sql]}
      set result [list]
      set answers [uplevel [list ns_pg_bind select $db {*}$bindOpt $sql]]
      while { [::db_getrow $db $answers] } {
        lappend result [ns_set copy $answers]
      }
    }
    return $result
  }

  ::xo::db::DB instproc foreach {{-dbn ""} {-bind ""} -prepare qn sql body} {
    #if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set qn [uplevel [list [self] qn $qn]]
    #
    # The prepare statement in the next line works probably only with
    # inline SQL statements.
    #
    #if {[info exists prepare]} {set sql [:prepare -dbn $dbn -argtypes $prepare $sql]}
    #ns_log notice "### [list ::db_foreach -dbn $dbn $qn $sql $body {*}$bindOpt]"
    uplevel [list ::db_foreach -dbn $dbn $qn $sql $body {*}$bindOpt]
  }

  ::xo::db::DB instproc exec_0or1row {-prepare {-bind ""} sql} {
    # Helper, used from several postgres-specific one-tuple queries
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    ::db_with_handle h {
      if {[info exists prepare]} {set sql [:prepare -handle $h -argtypes $prepare $sql]}
      return [uplevel [list ns_pg_bind 0or1row $h {*}$bindOpt $sql]]
    }
  }

  ::xo::db::DB instproc 0or1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_0or1row [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }
  ::xo::db::DB instproc 1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_1row [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }
  ::xo::db::DB instproc dml {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_dml [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
    return [db_resultrows]
  }
  ::xo::db::DB instproc get_value {{-dbn ""} {-bind ""} -prepare qn sql {default ""}} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_string [uplevel [list [self] qn $qn]] $sql -default $default {*}$bindOpt]
  }
  ::xo::db::DB instproc list_of_lists {{-bind ""} -prepare qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_list_of_lists [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }
  ::xo::db::DB instproc list {{-bind ""} -prepare qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_list [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }

  proc ::xo::db::pg_0or1row {sql} {
    ns_log notice "::xo::db::pg_0or1row deprecated"
    ::db_with_handle h {
      return [uplevel [list ns_pg_bind 0or1row $h {*}$bindOpt $sql]]
    }
  }

  #
  # The default insert-view operation (different in PostgreSQL and Oracle)
  #
  ::xo::db::Driver instproc insert-view-operation {} { return dml }
  ::xo::db::DB-postgresql instproc insert-view-operation {} { return 0or1row }

  #
  # DB driver functions, optimized for PostgreSQL
  #
  ::xo::db::DB-postgresql instproc 0or1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    set prepOpt [expr {[info exists prepare] ? [list -prepare $prepare] : ""}]
    set answers [uplevel [list [self] exec_0or1row {*}$prepOpt -bind $bind $sql]]
    if {$answers ne ""} {
      foreach {att val} [ns_set array $answers] { uplevel [list set $att $val] }
      ns_set free $answers
      return 1
    }
    return 0
  }
  ::xo::db::DB-postgresql instproc 1row {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    set prepOpt [expr {[info exists prepare] ? [list -prepare $prepare] : ""}]
    set answers [uplevel [list [self] exec_0or1row {*}$prepOpt -bind $bind $sql]]
    if {$answers ne ""} {
      foreach {att val} [ns_set array $answers] { uplevel [list set $att $val] }
      ns_set free $answers
      return 1
    }
    error "query $sql did not return an answer"
  }
  ::xo::db::DB-postgresql instproc get_value {{-dbn ""} {-bind ""} -prepare qn sql {default ""}} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    set prepOpt [expr {[info exists prepare] ? [list -prepare $prepare] : ""}]
    set answers [uplevel [list [self] exec_0or1row {*}$prepOpt -bind $bind $sql]]
    if {$answers ne ""} {
      set result [ns_set value $answers 0]
      ns_set free $answers
      return $result
    }
    return $default
  }
  ::xo::db::DB-postgresql instproc list_of_lists {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    db_with_handle db {
      if {[info exists prepare]} {set sql [:prepare -handle $db -argtypes $prepare $sql]}
      set result [list]
      set answers [uplevel [list ns_pg_bind select $db {*}$bindOpt $sql]]
      while { [db_getrow $db $answers] } {
        set row [list]
        foreach {att value} [ns_set array $answers] {lappend row $value}
        lappend result $row
      }
      ns_set free $answers
    }
    return $result
  }
  ::xo::db::DB-postgresql instproc list {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    db_with_handle db {
      if {[info exists prepare]} {set sql [:prepare -handle $db -argtypes $prepare $sql]}
      set result [list]
      set answers [uplevel [list ns_pg_bind select $db {*}$bindOpt $sql]]
      while { [::db_getrow $db $answers] } {
        lappend result [ns_set value $answers 0]
      }
      ns_set free $answers
    }
    return $result
  }
  ::xo::db::DB-postgresql instproc dml {{-dbn ""} {-bind ""} -prepare qn sql} {
    if {$sql eq ""} {set sql [:get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set bind $bindOpt
    db_with_handle -dbn $dbn db {
      if {[info exists prepare]} {set sql [:prepare -handle $db -argtypes $prepare $sql]}
      ::db_exec dml $db [uplevel [list [self] qn $qn]] $sql 2
    }
    return [db_resultrows]
  }

  ::xo::db::DB-postgresql instproc prepare {-handle:required {-argtypes ""} sql} {
    #
    # Define a md5 key for the prepared statement in nsv based on the
    # SQL statement.
    #
    set key [ns_md5 $sql]

    #
    # Get local variables "prepare", "execute", "prepName", and "sql"
    # keeping the relevant prepared statement context.
    #
    set per_interp_cache ::xo::prepared($key)
    if {[info exists $per_interp_cache]} {
      #
      # The prepared statement exists in the per-interp cache, get the
      # values from there.
      #
      lassign [set $per_interp_cache] prepare execute prepName sql

    } elseif {[nsv_exists prepared_statement $key]} {
      #
      # The prepared statement exists already in the nsv-cache.
      #
      set nsv_cached_value [nsv_get prepared_statement $key]
      #
      # Save the nsv-cached value as well in the per-interpreter cache
      # and set the output variables.
      #
      set $per_interp_cache $nsv_cached_value
      lassign $nsv_cached_value prepare execute prepName sql

    } else {
      #
      # Compute a PREPARE statement and an EXECUTE statement on the
      # fly. Notice, that the incoming SQL statement must not have Tcl
      # vars, but has to use bind vars.
      #
      set c 0; set l ""; set last 0
      set execArgs {}; set prepArgs {}
      foreach pair [regexp -all -inline -indices {[^:]:[a-zA-Z0_9_]+\M} $sql ] {
        lassign $pair from to
        lappend execArgs [string range $sql $from+1 $to]
        lappend prepArgs unknown
        append l [string range $sql $last $from] \$[incr c]
        set last [incr to]
      }
      append l [string range $sql $last end]

      set argtypes [split $argtypes ,]
      if {[llength $argtypes] == [llength $prepArgs]} {
        set prepArgs $argtypes
      }
      set c [nsv_incr prepared_statement count]
      set prepName __p$c
      set prepare "PREPARE $prepName ([join $prepArgs ,]) AS $l"
      set execute "EXECUTE $prepName ([join $execArgs ,])"
      nsv_set prepared_statement $key [list $prepare $execute $prepName $sql]
    }

    #
    # Cache the information, whether the prepared statement was
    # defined per pg session. Depending on the version of the driver,
    # we can obtain a session_id from the db driver. If we can't,
    # we fall back to a per request-cache (via toplevel variable).
    #
    try {
      set session_id [ns_db session_id $handle]
    } on ok {r} {
      #ns_log notice "=== $handle $session_id"
      set varName ::xo::prepared($session_id,$key)
    } on error {errorMsg} {
      set session_id "-"
      set varName __prepared($key)
    }

    if {![info exists $varName]} {
      #
      # We have to check for the prepared statement in the current
      # session and we have to create it if necessary there.
      #
      if {[ns_pg_bind 0or1row $handle {
        select 1 from pg_prepared_statements where name = :prepName
      }] eq ""} {
        #ns_log notice "=== do prepare handle $handle $prepare session_id $session_id"
        ::db_exec dml $handle dbqd..create_preapared $prepare
        set $varName 1
      }
    }
    #ns_log notice "=== prepare done, handle $handle execute $execute session_id $session_id"
    return $execute
  }

  ##########################################################################
  #
  # Depending on the configured and available driver, select the SQL
  # interface.  For the time being, we use just a single DB backend
  # per server and therefore a single database connection object,
  # namely ::xo::dc
  #
  ##########################################################################

  ad_proc ::xo::db::select_driver {{driver ""}} {
    Select the driver based on the specified argument (either DB or
    DBI) or based on the defaults for the configuration.  This
    function can be used to switch the driver as well dynamically.
  } {
    set sqlDialect [db_driverkey ""]
    if {$driver eq ""} {
      set driver DB
      if {[info exists ::acs::preferdbi]} {
        set driver DBI
      }
    }
    ::xo::db::$driver-$sqlDialect create ::xo::dc -dialect $sqlDialect
  }

  ::xo::db::select_driver

  ##########################################################################
  #
  # The ns_caches below should exist, before any cached objects are
  # created. Under most conditions, it is sufficient to do this in
  # object-cache-init.tcl, which is performed after xotcl-core procs
  # are read, but before applications using it (e.g. xowiki). However,
  # if e.g. xowiki is loaded via install.xml, the -init methods of
  # xotcl-core are not executed (probably a bug). Without the
  # ns_cache, creating objects fails with an error. So, we moved the
  # cache creation here and create caches, when they do not exist
  # already.
  #
  # Unfortunately, AOLserver's ns_cache has no command to check, whether
  # a cache exists, so we use the little catch below to check.
  #
  try {
    ns_cache flush xotcl_object_cache NOTHING
  } on error {errorMsg} {
    ns_log notice "xotcl-core: creating xotcl-object caches"

    ::acs::PartitionedCache create ::xo::xotcl_object_cache \
        -maxentry 200000 \
        -package_key xotcl-core \
        -parameter XOTclObjectCache \
        -default_size 400000 \
        -partitions 2
    ns_log notice "... created ::xo::xotcl_object_cache"

    ::acs::KeyPartitionedCache create ::xo::xotcl_object_type_cache \
        -package_key xotcl-core \
        -parameter XOTclObjectTypeCache \
        -default_size 50000 \
        -partitions 2
    ns_log notice "... created ::xo::xotcl_object_type_cache"

    ::acs::Cache create ::xo::xotcl_package_cache \
        -package_key xotcl-core \
        -parameter XOTclPackageCache \
        -default_size 10000
    ns_log notice "... created ::xo::xotcl_package_cache"
  }

  ##########################################################################
  #
  # Deprecated functions, obsoleted by xo::dc
  #
  ad_proc -deprecated has_ltree {} {
    Check, whether ltree is available (postgres only)
    @see ::xo::dc has_ltree
  } {
    ::xo::dc has_ltree
  }

  ad_proc -deprecated has_hstore {} {
    Check, whether hstore is available (postgres only)
    @see ::xo::dc has_hstore
  } {
    ::xo::dc has_hstore
  }

  ##########################################################################
  #
  # Support for requiring database artifacts
  #
  ##########################################################################
  ::xotcl::Object create require

  require proc exists_table {name} {
    if {[db_driverkey ""] eq "oracle"} {
      set name [string toupper $name]
    } else {
      set name [string tolower $name]
    }
    ::xo::db::sql::util table_exists -name $name
  }

  require proc exists_column {table_name column_name} {
    if {[db_driverkey ""] eq "oracle"} {
      set table_name  [string toupper $table_name]
      set column_name [string toupper $column_name]
    } else {
      set table_name  [string tolower $table_name]
      set column_name [string tolower $column_name]
    }
    ::xo::db::sql::util table_column_exists \
        -p_table $table_name \
        -p_column $column_name
  }

  require proc table {name definition {populate ""}} {
    #:log "==== require table $name exists: [:exists_table $name]\n$definition"
    if {![:exists_table $name]} {
      set lines {}
      foreach col [dict keys $definition] {lappend lines "$col [dict get $definition $col]"}
      set definition [join $lines ",\n"]
      # :log "--table $name does not exist, creating with definition: $definition"
      ::xo::dc dml create-table-$name "create table $name ($definition)"
      if {$populate ne ""} {
        ::xo::dc dml populate-table-$name $populate
      }
    } else {
      # The table exists already. Check the columns, whether we have to
      # add columns. We do not alter attribute types, and we do not
      # delete columns.
      foreach col [dict keys $definition] {
        if {![:exists_column $name $col]} {
          ns_log notice "xodb: adding column <alter table $name add column $col [dict get $definition $col]>"
          ::xo::dc dml alter-table-$name \
              "alter table $name add column $col [dict get $definition $col]"
        }
      }
    }
  }

  require proc view {name definition {-rebuild_p false}} {
    if {[db_driverkey ""] eq "oracle"} {set name [string toupper $name]}
    if {$rebuild_p} {
      ::xo::dc dml drop-view-$name "drop view if exists $name"
    }
    if {![::xo::db::sql::util view_exists -name $name]} {
      ::xo::dc dml create-view-$name "create view $name AS $definition"
    }
  }

  require proc index {-table -col -expression -expression_name {-using ""} {-unique false}} {
    if {![info exists col] && ![info exists expression]} {error "Neither col nor expression were provided"}
    if { [info exists col] &&  [info exists expression]} {error "Please provide either col or expression"}

    if {[info exists col]} {
      set colExpSQL $col
      regsub -all ", *" $col _ colExpName
    } else {
      set colExpSQL ($expression)
      if {[info exists expression_name]} {
        set colExpName $expression_name
      } else {
        regsub -all {[^[:alnum:]]} $expression "" colExpName
      }
    }
    set suffix [expr {$unique ? "un_idx" : "idx"}]
    set uniquepart [expr {$unique ? "UNIQUE" : ""}]
    set name [::xo::dc mk_sql_constraint_name $table $colExpName $suffix]
    if {![::xo::db::sql::util index_exists -name $name]} {
      if {[db_driverkey ""] eq "oracle"} {set using ""}
      set using [expr {$using ne "" ? "using $using" : ""}]
      ::xo::dc dml create-index-$name \
          "create $uniquepart index $name ON $table $using ($colExpSQL)"
    }
  }

  require proc sequence {
    -name
    -start_with
    -increment_by
    -minvalue
    -maxvalue
    {-cycle false}
    {-cache 1}
  } {
    if {[db_driverkey ""] eq "oracle"} {
      set name [string toupper $name]
      if {[::xo::dc 0or1row exists "
         SELECT 1 FROM user_sequences
          WHERE sequence_name = :name limit 1"]} return
    } else {
      #
      # PostgreSQL could avoid this check and use 'if not exists' in
      # versions starting with 9.5.
      #
      if {[::xo::dc 0or1row exists "
         SELECT 1 FROM information_schema.sequences
          WHERE sequence_schema = 'public'
            AND sequence_name = :name"]} return
    }

    set clause {}
    if {[info exists start_with]} {
      lappend clause "START WITH $start_with"
    }
    if {[info exists increment_by]} {
      lappend clause "INCREMENT BY $increment_by"
    }
    if {[info exists minvalue]} {
      lappend clause "MINVALUE $minvalue"
    }
    if {[info exists maxvalue]} {
      lappend clause "MAXVALUE $maxvalue"
    }
    if {!$cycle} {
      lappend clause "NO"
    }
    lappend clause "CYCLE"
    lappend clause "CACHE $cache"
    ::xo::dc dml create-seq "
       CREATE SEQUENCE $name [join $clause]"
  }

  require proc package {package_key} {
    if {![info exists :required_package($package_key)]} {
      foreach path [apm_get_package_files \
                        -package_key $package_key \
                        -file_types tcl_procs] {
        # Use apm_source instead of source to prevent double
        # sourcing by the apm_loader (temporary solution, double
        # sourcing should no happen)
        uplevel #1 apm_source "[acs_root_dir]/packages/$package_key/$path"
      }
      set :required_package($package_key) 1
    }
  }

  require ad_proc function_args {
    -kernel_older_than
    -package_key_and_version_older_than
    -check_function
    sql_file
  } {
    Load the SQL file, if the kernel is older than the specified
    version, and the version of the specified package is older, and
    the check_function does not exist in function_args.
    <p>
    Sample usage: <tt>
    ::xo::db::require function_args \<br>
    &nbsp;&nbsp;-kernel_older_than 5.5.0 \<br>
    &nbsp;&nbsp;-older_than_package_key_and_version "xowiki 0.50" \<br>
    &nbsp;&nbsp;-check_function "acs_object_type__create_type" \<br>
    &nbsp;&nbsp;[acs_package_root_dir xotcl-request-broker]/patches/funcs-1.sql</tt>
  } {
    if {[db_driverkey ""] eq "postgresql"} {
      # only necessary with PostgreSQL
      if {[info exists kernel_older_than]} {
        if {[apm_version_names_compare \
                 $kernel_older_than [ad_acs_version]] < 1} {
          # nothing to do
          return
        }
      }
      if {[info exists package_key_and_version_older_than]} {
        set p [split $package_key_and_version_older_than]
        if {[llength $p] != 2} {
          error "package_key_and_version_older_than should be\
        of the form 'package_key version'"
        }
        lassign $p package_key version
        set installed_version [apm_highest_version_name $package_key]
        if {[apm_version_names_compare $installed_version $version] > -1} {
          # nothing to do
          return
        }
      }
      if {[info exists check_function]} {
        set check_function [string toupper $check_function]
        set function_exists [::xo::dc get_value query_version {
          select 1 from acs_function_args where function = :check_function
          limit 1
        } 0]
        if {$function_exists} {
          # nothing to do
          return
        }
      }

      if {[ad_file readable $sql_file]} {
        :log "Sourcing '$sql_file'"
        db_source_sql_file $sql_file
        ::xo::db::Class create_all_functions
        return 1
      } else {
        :log "Could not source '$sql_file'"
      }
    }
    return 0
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
        {sql_package_name}
      } -ad_doc {
        ::xo::db::Class is a meta class for interfacing with acs_object_types.
        acs_object_types are instances of this meta class. The meta class defines
        the behavior common to all acs_object_types. The behavior common to
        all acs_objects is defined by the class ::xo::db::Object.

        @see ::xo::db::Object
      }

  #::xo::db::Class set __default_superclass ::xo::db::Object  ;# will be supported in XOTcl 1.6


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
    return [::xo::dc get_value select_object {
      select 1 from acs_objects where object_id = :id
    } 0]
  }

  ::xo::db::Class ad_proc delete {
    -id:required
  } {
    Delete the object from the database
  } {
    ::xo::db::sql::acs_object delete -object_id $id
  }

  ::xo::db::Class ad_proc get_object_type {
    -id:integer,required
  } {
    Return the object type for the give id.

    @return object_type, typically an XOTcl class
  } {
    xo::xotcl_object_type_cache eval -partition_key $id $id {
      ::xo::dc 1row get_class "select object_type from acs_objects where object_id=:id"
      return $object_type
    }
  }

  ::xo::db::Class ad_proc get_instance_from_db {
    -id:required,integer
  } {
    Create an XOTcl object from an acs_object_id. This method
    determines the type and initializes the object from the
    information stored in the database. The XOTcl object is
    destroyed automatically on cleanup (end of a connection request).

    @return fully qualified object
  } {
    set type  [:get_object_type -id $id]
    set class [::xo::db::Class object_type_to_class $type]
    if {![:isclass $class]} {
      error "no class $class defined"
    }
    set r [$class create ::$id]
    $r db_1row dbqd..get_instance [$class fetch_query $id]
    $r set object_id $id
    $r destroy_on_cleanup
    $r initialize_loaded_object
    return $r
  }

  ::xo::db::Class ad_proc get_table_name {
    -object_type:required
  } {
    Get the table_name of an object_type from the database. If the
    object_type does not exist, the return value is empty.

    @return table_name
  } {
    return [::xo::dc get_value get_table_name {
      select lower(table_name) as table_name from acs_object_types where object_type = :object_type
    } ""]
  }

  ::xo::db::Class ad_proc object_type_exists_in_db {-object_type} {
    Check, if an object_type exists in the database.

    @return 0 or 1
  } {
    return [::xo::dc get_value check_type {
      select 1 from acs_object_types where object_type = :object_type
    } 0]
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
        ::xo::dc dml delete_instances "delete from $table_name"
        if {$drop_table} {
          ::xo::dc dml drop_table "drop table $table_name"
        }
      } errorMsg]} {
        ns_log error "error during drop_type: $errorMsg"
      }
    }
    ::xo::db::sql::acs_object_type drop_type \
        -object_type $object_type -drop_children_p $cascade_p
    return ""
  }

  ::xo::db::Class ad_proc delete_all_acs_objects {-object_type:required} {
    Delete all acs_objects of the object_type from the database.
  } {
    set table_name [::xo::db::Class get_table_name -object_type $object_type]
    if {$table_name ne ""} {
      ::xo::dc dml delete_instances {delete from :table_name}
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
    # some table_names and id_columns in acs_object_types are unfortunately uppercase,
    # so we have to convert to lowercase here....
    ::xo::dc 1row fetch_class {
      select object_type, supertype, pretty_name, lower(id_column) as id_column, lower(table_name) as table_name
      from acs_object_types where object_type = :object_type
    }
    set classname [:object_type_to_class $object_type]
    if {![:isclass $classname]} {
      # the XOTcl class does not exist, we create it
      #:log "--db create class $classname superclass $supertype"
      ::xo::db::Class create $classname \
          -superclass [:object_type_to_class $supertype] \
          -object_type $object_type \
          -supertype $supertype \
          -pretty_name $pretty_name \
          -id_column $id_column \
          -table_name $table_name \
          -sql_package_name [namespace tail $classname] \
          -noinit
    } else {
      #:log "--db we have a class $classname"
    }
    set attributes [::xo::dc list_of_lists get_atts {
      select attribute_name, pretty_name, pretty_plural, datatype,
      default_value, min_n_values, max_n_values
      from acs_attributes where object_type = :object_type
    }]

    set slots ""
    foreach att_info $attributes {
      lassign $att_info attribute_name pretty_name pretty_plural datatype \
          default_value min_n_values max_n_values

      # ignore some erroneous definitions in the acs meta model
      if {[info exists :exclude_attribute($table_name,$attribute_name)]} {
        continue
      }

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
    ad_try {
      $classname slots $slots
    } on error {errorMsg} {
      error "Error during slots: $errorMsg"
    }

    $classname init
    return $classname
  }

  #
  # interface for stored procedures
  #

  ::xo::db::postgresql instproc get_all_package_functions {} {
    #
    # Load definitions in one step from function args; only for
    # those definitions where we do not have function args, we parse
    # the function arg aliases.
    #
    set definitions [::xo::dc list_of_lists get_all_package_functions0 {
      select
      args.function,
      args.arg_name,
      args.arg_default
      from acs_function_args args
      order by function, arg_seq
    }]
    set last_function ""
    set function_args {}
    foreach definition $definitions {
      lassign $definition function arg_name default
      if {$last_function ne "" && $last_function ne $function} {
        set ::xo::db::fnargs($last_function) $function_args
        #puts stderr "$last_function [list $function_args]"
        set function_args {}
      }
      lappend function_args [list $arg_name $default]
      set last_function $function
    }
    set ::xo::db::fnargs($last_function) $function_args
    #puts stderr "$last_function [list $function_args]"
    ns_log notice "loaded [array size ::xo::db::fnargs] definitions from function args"
    #ns_log notice "... [lsort [array names ::xo::db::fnargs *__*]]"

    #
    # Get all package functions (package name, object name) from PostgreSQL
    # system catalogs.
    #
    return [::xo::dc list_of_lists [self proc] {
      select distinct
      upper(substring(proname from 0 for position('__' in proname))) as package_name,
      upper(substring(proname from position('__' in proname)+2)) as object_name
      from pg_proc
      where strpos(proname,'__') > 1
    }]
  }

  ::xo::db::postgresql instproc get_function_args {package_name object_name} {
    set key [string toupper ${package_name}__${object_name}]
    #
    # If we have function definitions already loaded, there is nothing
    # to do.
    #
    if {[info exists ::xo::db::fnargs($key)]} {
      return $::xo::db::fnargs($key)
    }

    ns_log notice "obtain fnargs for $key from PostgreSQL via parsing function definition"

    #
    # Get function_args for a single sql-function from PostgreSQL
    # system catalogs by retrieving the function source code and
    # passing it. We retrieve always the function with the longest
    # argument list for our definition, since we use an interface with
    # non positional arguments, where in most situations, many
    # arguments are optional.  In cases, where more function with the
    # same number of arguments are available, we sort by the type as
    # well to obtain a predictable ordering and to give string
    # interfaces (text, varchar) a higher priority than integer or
    # boolean arguments (e.g. int4, int8m bool).
    #
    # Note: based on the ordering, char has lower priority over int*
    # which is probably a bug, but is not a problem in OpenACS.
    #
    # Note that we can as well get the type in future versions.
    #
    ::xo::dc foreach get_function_params {
      select proname, pronargs, proargtypes, prosrc
      from pg_proc
      where proname = lower(:package_name) || '__' || lower(:object_name)
      order by pronargs desc, proargtypes desc
    } {
      set n 1
      set function_args [list]
      foreach line [split $prosrc \n] {
        if {[regexp -nocase "alias +for +\\\$$n" $line]} {
          if {![regexp {^[^a-zA-Z]+([a-zA-Z0-9_]+)\s} $line _ fq_name]} {
            #ns_log notice "proname $proname line <$line> fq_name <$fq_name>"
            ns_log notice "--***** Could not retrieve argument name for $proname\
                argument $n from line '$line' i    n $prosrc'"
              set fq_name arg$n
          }
          set name $fq_name
          set default ""
          if {![regexp {^.+__(.+)$} $fq_name _ name]} {
            regexp {^[vp]_(.+)$} $fq_name _ name
          }
          if {[regexp {^.*-- default +([^, ]+) *$} $line _ default]} {
            set default [string trim $default '\n\r]
          }
          lappend function_args [list [string toupper $name] $default]
          if {[incr n]>$pronargs} break
        }
      }
      if {$n == 1 && $pronargs > 0} {
        set comment [string map [list \n "\n----\t"] $prosrc]
        ns_log notice "---- no aliases for $proname/$pronargs $comment"
        #continue
      }
      #break
    }
    return $function_args
  }

  #
  # The generation of the code interface code is driver specific, since
  # e.g. dbi supports option "-autonull", which simplified the
  # interface code significantly
  #

  #
  #  DBI interface method generation (with autonull):
  #

  ::xo::db::DBI instproc generate_psql {package_name object_name} {
    set function_args [:get_function_args $package_name $object_name]
    set function_args [:fix_function_args $function_args $package_name $object_name]
    set sql_info [:sql_arg_info $function_args $package_name $object_name]
    #ns_log notice "-- select ${package_name}__${object_name}($psql_args)"
    set sql_suffix [:psql_statement_suffix ${package_name} ${object_name}]
    dict set sql_info sql [subst { select ${package_name}__${object_name}([dict get $sql_info psql_args]) $sql_suffix}]
    dict set sql_info sql_cmd [subst {dbi_1row -autonull {[dict get $sql_info sql] as result}}]
    dict set sql_info body [subst {
      #function_args: $function_args
      [dict get [set sql_info] sql_cmd]
      return \$result
    }]
    return $sql_info
  }

  ::xo::db::DBI instproc sql_arg_info {function_args package_name object_name} {
    set defined {}
    set psql_args [list]
    set arg_order [list]
    # TODO function args not needed in dict
    foreach arg $function_args {
      lassign $arg arg_name default_value
      lappend psql_args :[string tolower $arg_name]
      lappend arg_order $arg_name
      lappend defined $arg_name $default_value
    }
    return [list \
                psql_args [join $psql_args ", "] \
                arg_order $arg_order \
                defined       $defined \
                function_args $function_args]
  }

  #
  # In some cases, we need locks on SQL select statements, when the
  # select updates tuples, e.g. via a function. This is required at
  # least in PostgreSQL.
  #
  set ::xo::db::sql_suffix(postgresql,content_item,set_live_revision) "FOR NO KEY UPDATE"
  set ::xo::db::sql_suffix(postgresql,content_item,del) "FOR UPDATE"
  set ::xo::db::sql_suffix(postgresql,content_item,new) "FOR UPDATE"

  ::xo::db::DB instproc psql_statement_suffix {package_name object_name} {
    set key ::xo::db::sql_suffix(${:dialect},$package_name,$object_name)
    return [expr {[info exists $key] ? [set $key] : ""}]
  }

  #
  #  DB-postgresql interface method generation (no autonull):
  #
  ::xo::db::DB-postgresql instproc generate_psql {package_name object_name} {
    set function_args [:get_function_args $package_name $object_name]
    set function_args [:fix_function_args $function_args $package_name $object_name]
    set sql_info [:sql_arg_info $function_args $package_name $object_name]
    #ns_log notice "-- select ${package_name}__${object_name} ($psql_args)"
    set sql_suffix [:psql_statement_suffix ${package_name} ${object_name}]
    set sql [subst {
      select ${package_name}__${object_name}([dict get $sql_info psql_args]) $sql_suffix
    }]
    set sql_cmd {ns_set value [ns_pg_bind 0or1row $db $sql] 0}
    dict set sql_info body [subst {
      #function_args: $function_args
      foreach var \[list [dict get $sql_info arg_order]\]  {
        set varname \[string tolower \$var\]
        if {\[info exists \$varname\]} {
          set \$var \[set \$varname\]
          set _\$var :\$var
        } else {
          set _\$var null
        }
      }
      set sql "$sql"
      db_with_handle -dbn \$dbn db {
        #ns_log notice "--sql=\$sql"
        return \[ $sql_cmd \]
      }
    }]
    return $sql_info
  }

  #
  #  DB and Oracle interface method generation (no autonull):
  #

  ::xo::db::DB-oracle instproc get_all_package_functions {} {
    #
    # Get all package functions (package name, object name) from Oracle
    # system catalogs.
    #
    return [::xo::dc list_of_lists [self proc] {
      select distinct package_name, object_name
      from user_arguments args
      where args.position > 0 and package_name is not null
    }]
  }

  ::xo::db::DB-oracle instproc get_function_args {package_name object_name} {
    #
    # In Oracle, args.default_value appears to be defunct and useless.
    # for now, we simply return a constant "unknown", otherwise the
    # argument would be required
    return [::xo::dc list_of_lists get_function_params {
      select args.argument_name, 'NULL'
      from user_arguments args
      where args.position > 0
      and args.object_name = upper(:object_name)
      and args.package_name = upper(:package_name)
      order by args.position
    }]
  }

  ::xo::db::DB-oracle instproc generate_psql {package_name object_name} {
    #
    # in Oracle, we have to distinguish between functions and procs
    #
    set is_function [::xo::dc 0or1row is_function {
      select 1 from dual
      where exists (select 1 from user_arguments where
                    package_name = upper(:package_name)
                    and object_name = upper(:object_name)
                    and position = 0)
    }]

    set function_args [:get_function_args $package_name $object_name]
    set function_args [:fix_function_args $function_args $package_name $object_name]
    set sql_info [:sql_info $function_args $package_name $object_name]

    if {$is_function} {
      set sql [subst {BEGIN :1 := ${package_name}.${object_name}(\$sql_args); END;}]
      set sql_cmd {ns_ora exec_plsql_bind $db $sql 1 ""}
    } else {
      set sql [subst {BEGIN ${package_name}.${object_name}(\$sql_args); END;}]
      set sql_cmd {ns_ora dml $db $sql}
    }
    dict set sql_info body  return [subst {
      #function_args: $function_args
      set sql_args \[list\]
      foreach var \[list [dict get $sql_info arg_order]\]  {
        set varname \[string tolower \$var\]
        if {\[info exists \$varname\]} {
          lappend sql_args "\$varname => :\$varname"
        }
      }
      set sql_args \[join \$sql_args ,\]
      set sql "$sql"
      db_with_handle -dbn \$dbn db {
        #:log "sql=$sql, sql_command=$sql_cmd"
        return \[ $sql_cmd \]
      }
    }]
    return $sql_info
  }

  # Some stored procedures like content_item__new do currently not
  # define correctly default values.  Therefore, we need - temporary -
  # this ugly redundancy to complete the definitions.  The correct fix
  # is to define the correct default values in the database with
  # define_function_args()

  ::xo::db::SQL array set fallback_defaults {
    "content_item__new" {
      RELATION_TAG null DESCRIPTION null TEXT null
      CREATION_IP null NLS_LANGUAGE null LOCALE null CONTEXT_ID null
      DATA null TITLE null ITEM_ID null
      CREATION_DATE now
      ITEM_SUBTYPE content_item
      CONTENT_TYPE content_revision
      MIME_TYPE text/plain
      IS_LIVE f
      STORAGE_TYPE lob
    }
    "content_type__create_attribute" {
      DEFAULT_VALUE null SORT_ORDER null PRETTY_PLURAL null
    }
    "content_type__drop_type" {
      DROP_CHILDREN_P f DROP_TABLE_P f DROP_OBJECTS_P f
    }
    "acs_attribute__create_attribute" {
      PRETTY_PLURAL null TABLE_NAME null COLUMN_NAME null
      DEFAULT_VALUE null SORT_ORDER null DATABASE_TYPE null SIZE null
      REFERENCES null CHECK_EXPR null COLUMN_SPEC null
    }
    "acs_object_type__create_type" {
      TYPE_EXTENSION_TABLE null NAME_METHOD null
    }
  }

  ::xo::db::SQL instproc fix_function_args {function_args package_name object_name} {
    #
    # Load fallback defaults for buggy function args. The values
    # provided here are only used for function args without specified
    # defaults. This is a transitional solution; actually, the
    # function args should be fixed.
    #

    if {![::xo::db::SQL exists fallback_defaults(${package_name}__$object_name)]} {
      return $function_args
    }
    array set additional_defaults [::xo::db::SQL set fallback_defaults(${package_name}__$object_name)]
    set result [list]
    foreach arg $function_args {
      lassign $arg arg_name default_value
      if {$default_value eq "" && [info exists additional_defaults($arg_name)]} {
        lappend result [list $arg_name $additional_defaults($arg_name)]
      } else {
        lappend result [list $arg_name $default_value]
      }
    }
    return $result
  }




  ::xo::db::SQL instproc sql_arg_info {function_args package_name object_name} {
    set defined {}
    set psql_args [list]
    set arg_order [list]
    foreach arg $function_args {
      lassign $arg arg_name default_value
      lappend psql_args \$_$arg_name
      lappend arg_order $arg_name
      lappend defined $arg_name $default_value
    }
    return [list \
                psql_args     [join $psql_args ", "] \
                arg_order     $arg_order \
                defined       $defined \
                function_args $function_args]
  }

  ::xo::db::Class instproc dbproc_nonposargs {object_name} {
    #
    # This method compiles a stored procedure into a xotcl method
    # using a classic nonpositional argument style interface.
    #
    # The current implementation should work on PostgreSQL and Oracle
    # (not tested) but will not work, when a single OpenACS instance
    # want to talk to PostgreSQL and Oracle simultaneously. Not sure,
    # how important this is...
    #
    if {$object_name eq "set"} {
      :log "We cannot handle object_name = '$object_name' in this version"
      return
    }
    #
    # Object names have the form of e.g. ::xo::db::apm_parameter.
    # Therefore, we use the namspace tail as sql_package_name.
    #
    set package_name  [:sql_package_name [namespace tail [self]]]
    set sql_info      [::xo::dc generate_psql $package_name $object_name]

    # puts "sql_command=$sql_command"
    # puts "sql_info=$sql_info"
    array set defined [dict get $sql_info defined]

    set nonposarg_list [list [list -dbn ""]]
    foreach arg_name [dict get $sql_info arg_order] {
      # special rule for DBN ... todo: proc has to handle this as well
      set nonposarg_name [expr {$arg_name eq "DBN" ? "DBN" : [string tolower $arg_name]}]
      #
      # handling of default values:
      #  - no value ("") --> the attribute is required
      #  - value different from NULL --> make it default
      #  - otherwise: non-required argument
      #
      set default_value $defined($arg_name)
      if {$default_value eq ""} {
        set arg -$nonposarg_name:required
      } elseif {[string tolower $default_value] ne "null"} {
        set arg [list -$nonposarg_name $default_value]
      } else {
        set arg -$nonposarg_name
      }
      lappend nonposarg_list $arg
    }
    # When the new method is executed within a contains, -childof is
    # appended. We have to added it here to avoid complains. Xotcl 2.0
    # should find better ways to handle contain or the new invocation.
    if {$object_name eq "new"} {lappend nonposarg_list -childof}
    #:log "-- define $object_name $nonposarg_list"

    #ns_log notice final=[dict get $sql_info body]
    :ad_proc $object_name $nonposarg_list {Automatically generated method} [dict get $sql_info body]
  }

  ::xo::db::Class instproc unknown {m args} {
    error "Error: unknown database method '$m' for [self]"
  }

  ::xo::db::Class proc create_all_functions {} {

    foreach item [::xo::dc get_all_package_functions] {
      lassign $item package_name object_name

      if {[string match "*TRG" [string toupper $object_name]]} {
        # no need to provide interface to trigger functions
        continue
      }

      set class_name ::xo::db::sql::[string tolower $package_name]
      if {![nsf::is object $class_name]} {
        ::xo::db::Class create $class_name
      } elseif {![$class_name istype ::xo::db::Class]} {
        #
        # The methods of ::xo::db::sql::util like "table_exists" fall
        # into this category. Make sure that we do not create new
        # objects via the next command.
        #
        continue
      }
      $class_name dbproc_nonposargs [string tolower $object_name]
    }
  }

  ::xo::db::Class proc class_to_object_type {name} {
    if {[:isclass $name]} {
      if {[$name exists object_type]} {
        # The specified class has an object_type defined; return it
        return [$name object_type]
      }
      if {![$name istype ::xo::db::Object]} {
        # The specified class is not subclass of ::xo::db::Object.
        # return acs_object in your desperation.
        return acs_object
      }
    }
    # Standard mapping rules
    switch -glob -- $name {
      ::xo::db::Object   {return acs_object}
      ::xo::db::CrItem   {return content_revision}
      ::xo::db::image    {return image}
      ::xo::db::CrFolder {return content_folder}
      ::xo::db::*        {return [string range $name 10 end]}
      default            {return $name}
    }
  }

  ::xo::db::Class proc object_type_to_class {name} {
    switch -glob -- $name {
      acs_object       {return ::xo::db::Object}
      content_revision -
      content_item     {return ::xo::db::CrItem}
      content_folder   {return ::xo::db::CrFolder}
      ::*              {return $name}
      default          {return ::xo::db::$name}
    }
  }

  #
  # Now, create all stored procedures in PostgreSQL or Oracle.
  #
  ::xo::db::Class create_all_functions


  #
  # The object require provides an interface to create certain
  # resources in case they are not created already.
  #
  #
  # Most of ::xo::db::sql::util is coming from acs-kernel / utilities.create.sql
  #
  # But still, we add here more procs
  if {[::xo::db::sql::util info commands get_default] ne ""} {
    require proc unique {-table -col} {
      # Unique could be there by an index too
      set idxname [::xo::dc mk_sql_constraint_name $table $col un_idx]
      if {[::xo::db::sql::util index_exists -name $idxname]} return
      if {![::xo::db::sql::util unique_exists -table $table -column $col]} {
        ::xo::dc dml alter-table-$table \
            "alter table $table add unique ($col)"
      }
    }

    require proc not_null {-table -col} {
      if {![::xo::db::sql::util not_null_exists -table $table -column $col]} {
        ::xo::dc dml alter-table-$table \
            "alter table $table alter column $col set not null"
      }
    }

    require proc default {-table -col -value} {
      set default [::xo::db::sql::util get_default -table $table -column $col]
      #
      # Newer versions of PostgreSQL return default values with type
      # casts (e.g. 'en_US'::character varying). In these cases, we
      # remove the type cast from the returned default value before
      # comparison.
      #
      # Depending on the generation and real datatype of the DBMS,
      # certain datatype values are reported differently from the
      # DBMS. Therefore, we use a type cast to check whether
      # specified default value (e.g. '1900-01-01') is in fact
      # equivalent to default stored in db (e.g. '1900-01-01
      # 00:00:00+01'::timestamp with time zone).
      #
      # Booleans can be normalized in advance without involving the
      # database
      if {
          ($default eq "f" && $value eq "false")
          || ($default eq "t" && $value eq "true")
        } {
        set value $default
      }
      if {$default ne $value} {
        if {[regexp {^'(.*)'::(.*)$} $default match default_value default_datatype]} {
          set clause "$default <> cast(:value as $default_datatype)"
        } else {
          set datatype [db_column_type $table $col]
          set clause "cast(:default as $datatype) <> cast(:value as $datatype)"
        }
        # This last coalesce is in case one of the compared values
        # was null: as we know they were different, this is
        # certainly a new default
        if {[::xo::dc get_value check_default "
                select coalesce($clause, true) from dual"]} {
          ::xo::dc dml alter-table-$table \
              "alter table $table alter column $col set default :value"
        }
      }
    }

    require proc references {-table -col -ref} {
      # Check for already existing foreign keys.
      set ref [string trim $ref]
      # try to match the full reftable(refcol) syntax...
      if {![regexp {^(\w*)\s*\(\s*(\w*)\s*\)\s*(.*)$} $ref match reftable refcol rest]} {
        # if fails only table was given, assume refcol is reftable's
        # primary key
        set reftable [lindex $ref 0]
        set refcol [::xo::db::sql::util get_primary_keys -table $reftable]
        # only one primary key is supported for the table
        if {[llength $refcol] != 1} return
      }
      if {[::xo::db::sql::util foreign_key_exists \
               -table $table -column $col \
               -reftable $reftable -refcolumn $refcol]} {
        ns_log debug "foreign key already exists for table $table column $col to ${reftable}(${refcol})"
        return
      }
      ::xo::dc dml alter-table-$table \
          "alter table $table add foreign key ($col) references $ref"
    }
  }

  #
  # Methods for instances of the meta class (methods for object_types)
  #
  if {[db_driverkey ""] eq "postgresql"} {
    #
    # PostgreSQL
    #
    ::xo::db::Class instproc object_types_query {
      {-subtypes_first:boolean false}
    } {
      set object_type_key ${:object_type_key}
      set order_clause [expr {$subtypes_first ? "order by tree_sortkey desc":""}]
      return "select object_type from acs_object_types where
        tree_sortkey between '$object_type_key' and tree_right('$object_type_key')
        $order_clause"
    }
    ::xo::db::Class instproc init_type_hierarchy {} {
      set object_type ${:object_type}
      set :object_type_key [::xo::dc list get_tree_sortkey {
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
      set object_type ${:object_type}
      set order_clause [expr {$subtypes_first ? "order by LEVEL desc":""}]
      return "select object_type from acs_object_types
        start with object_type = '$object_type'
        connect by prior object_type = supertype $order_clause"
    }
    ::xo::db::Class instproc init_type_hierarchy {} {
      set :object_type_key {}
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
    return [::xo::dc list get_object_types \
                [:object_types_query -subtypes_first $subtypes_first]]
  }

  ::xo::db::Class ad_instproc create_object_type {} {
    Create an acs object_type for the current XOTcl class
  } {
    :check_default_values
    :check_table_atts

    # The default supertype is acs_object. If the supertype
    # was not changed (still acs_object), we map the superclass
    # to the object_type to obtain the ACS supertype.
    if {${:supertype} eq "acs_object"} {
      set :supertype [::xo::db::Class class_to_object_type [:info superclass]]
    }

    ::xo::db::sql::acs_object_type create_type \
        -object_type   ${:object_type} \
        -supertype     ${:supertype} \
        -pretty_name   ${:pretty_name} \
        -pretty_plural ${:pretty_plural} \
        -table_name    ${:table_name} \
        -id_column     ${:id_column} \
        -abstract_p    ${:abstract_p} \
        -name_method   ${:name_method} \
        -package_name  [:sql_package_name]
  }

  ::xo::db::Class ad_instproc drop_object_type {{-cascade true}} {
    Drop an acs object_type; cascde true means that the attributes
    are dropped as well.
  } {
    ::xo::db::sql::acs_object_type drop_type \
        -object_type ${:object_type} \
        -cascade_p [expr {$cascade ? "t" : "f"}]
  }

  ::xo::db::Class instproc db_slots {} {

    array set :db_slot [list]
    array set :db_constraints [list]
    #
    # First get all ::xo::db::Attribute slots and check later,
    # if we have to add the id_column automatically.
    #
    # :log "--setting db_slot all=[:info slots]"
    foreach att [:info slots] {
      #:log "--checking $att [$att istype ::xo::db::Attribute] [$att info class]"
      if {![$att istype ::xo::db::Attribute]} continue
      set :db_slot([$att name]) $att
      :collect_constraints $att
    }
    if {[self] ne "::xo::db::Object"} {
      if {[info exists :id_column] && ![info exists :db_slot(${:id_column})]} {
        # create automatically the slot for the id column
        :slots [subst {
          ::xo::db::Attribute create ${:id_column} \
              -pretty_name "ID" \
              -datatype integer \
              -create_acs_attribute false
        }]
        set :db_slot(${:id_column}) [self]::slot::${:id_column}
      }
    }
    #:log "--setting db_slot of [self] to [array names _db_slot]"
  }

  # read attribute constraints and store them so they can be added
  # after plain table creation
  ::xo::db::Class instproc collect_constraints {att} {
    set attname [$att name]
    # Index is always created after table creation, so it is always ok
    # to collect this...
    if {[$att exists index]} {
      lappend :db_constraints($attname) [list index [$att set index]]
    }
    # ...in all other cases, when column doesn not exist will be
    # created properly. No need to collect constraints.
    if {[::xo::db::require exists_column ${:table_name} $attname]} {
      if {[$att exists unique] && [$att set unique]} {
        lappend :db_constraints($attname) unique
      }
      if {[$att exists not_null] && [$att set not_null]} {
        lappend :db_constraints($attname) not_null
      }
      if {![string is space [$att set references]]} {
        lappend :db_constraints($attname) [list references [$att set references]]
      }
      if {[$att exists default]} {
        lappend :db_constraints($attname) [list default [$att set default]]
      }
    }
  }

  ::xo::db::Class instproc table_definition {} {
    array set column_specs [list]
    #
    # iterate over the slots and collect the column_specs for table generation
    #
    foreach {slot_name slot} [array get :db_slot] {
      if {![$slot create_table_attribute]} continue
      set column_name [$slot column_name]
      set column_specs($column_name) \
          [$slot column_spec -id_column [expr {$column_name eq ${:id_column}}]]
    }

    # Requires collected constraints on object's table.
    ::xo::db::Class instproc require_constraints {} {
      set table_name [:table_name]
      foreach col [array names :db_constraints] {
        foreach constr [set :db_constraints($col)] {
          set type  [lindex $constr 0]
          set value [join [lrange $constr 1 end]]
          switch -- $type {
            "unique" {
              ::xo::db::require unique \
                  -table $table_name -col $col
            }
            "index" {
              set value [expr {[string is true $value] ? "" : $value}]
              ::xo::db::require index -using $value \
                  -table $table_name -col $col
            }
            "not_null" {
              ::xo::db::require not_null \
                  -table $table_name -col $col
            }
            "references" {
              ::xo::db::require references \
                  -table $table_name -col $col \
                  -ref $value
            }
            "default" {
              ::xo::db::require default \
                  -table $table_name -col $col \
                  -value $value
            }
          }
        }
      }
    }

    if {[array size column_specs] > 0} {
      if {${:table_name} eq ""} {error "no table_name specified"}
      if {${:id_column} eq ""}  {error "no id_column specified"}
      if {![info exists column_specs(${:id_column})]} {
        error "no ::xo::db::Attribute slot for id_column '${:id_column}' specified"
      }
      set table_specs [list]
      foreach {att spec} [array get column_specs] {lappend table_specs $att $spec}
      set table_definition $table_specs
    } else {
      set table_definition ""
    }
    # :log table_definition=$table_definition
    return $table_definition
  }

  ::xo::db::Class instproc mk_update_method {} {
    set updates [list]
    set vars [list]
    foreach {slot_name slot} [array get :db_slot] {
      $slot instvar name column_name
      if {$column_name ne [:id_column]} {
        lappend updates "$column_name = :$name"
        lappend vars $name
      }
    }
    if {[llength $updates] == 0} return
    :instproc update {} [subst {
      ::xo::dc transaction {
        next
        :instvar object_id $vars
        ::xo::dc dml update_[:table_name] {update [:table_name]
          set [join $updates ,] where [:id_column] = :object_id
        }
      }
    }]
  }

  ::xo::db::Class instproc mk_insert_method {} {
    # create method 'insert' for the application class
    # The caller (e.g. method new) should care about db_transaction
    :instproc insert {} {
      set __table_name [[self class] table_name]
      set __id [[self class] id_column]
      set :$__id ${:object_id}
      :log "ID insert in $__table_name, id = $__id = [set :$__id]"
      next
      foreach {__slot_name __slot} [[self class] array get db_slot] {
        if {[info exists :$__slot_name]} {
          set $__slot_name [set :$__slot_name]
          lappend __vars $__slot_name
          lappend __atts [$__slot column_name]
        }
      }
      ::xo::dc dml insert_$__table_name "insert into $__table_name
        ([join $__atts ,]) values (:[join $__vars ,:])"
    }
  }

  ::xo::db::Class ad_instproc check_table_atts {} {
    Check table_name and id_column and set meaningful
    defaults, if these attributes are not provided.
  } {
    :check_default_values
    set table_name_error_tail ""
    set id_column_error_tail ""

    if {![info exists :sql_package_name]} {
      set :sql_package_name [self]
      #:log "-- sql_package_name of [self] is '${:sql_package_name}'"
    }
    if {[string length ${:sql_package_name}] > 30} {
      error "SQL package_name '${:sql_package_name}' can be maximal 30 characters long!\
        Please specify a shorter sql_package_name in the class definition."
    }
    if {${:sql_package_name} eq ""} {
      error "Cannot determine SQL package_name. Please specify it explicitly!"
    }

    if {![info exists :table_name]} {
      set tail [namespace tail [self]]
      regexp {^::([^:]+)::} [self] _ head
      :table_name [string tolower ${head}_$tail]
      #:log "-- table_name of [self] is '[:table_name]'"
      set table_name_error_tail ", or use different namespaces/class names"
    }

    if {![info exists :id_column]} {
      set :id_column [string tolower [namespace tail [self]]]_id
      set id_column_error_tail ", or use different class names"
      #:log "-- created id_column '[:id_column]'"
    }

    if {![regexp {^[[:alpha:]_][[:alnum:]_]*$} [:table_name]]} {
      error "Table name '[:table_name]' is unsafe in SQL: \
    Please specify a different table_name$table_name_error_tail."
    }

    if {[string length [:table_name]] > 30} {
      error "SQL table_name '[:table_name]' can be maximal 30 characters long!\
        Please specify a shorter table_name in the class definition."
    }

    if {![regexp {^[[:alpha:]_][[:alnum:]_]*$} [:id_column]]} {
      error "Name for id_column '[:id_column]' is unsafe in SQL: \
        Please specify a different id_column$id_column_error_tail"
    }
  }

  ::xo::db::Class instproc check_default_values {} {
    if {![info exists :pretty_name]}   {set :pretty_name [namespace tail [self]]}
    if {![info exists :pretty_plural]} {set :pretty_plural ${:pretty_name}}
  }

  ::xo::db::Class instproc init {} {
    if {![::xo::db::Class object_type_exists_in_db -object_type [:object_type]]} {
      :create_object_type
    }
    :init_type_hierarchy
    :check_table_atts
    :db_slots

    if {[:with_table]} {
      set table_definition [:table_definition]
      if {$table_definition ne ""} {
        ::xo::db::require table [:table_name] $table_definition
        :require_constraints
      }
      :mk_update_method
      :mk_insert_method
    }
    next
  }

  ::xo::db::Class instproc get_context {package_id_var user_id_var ip_var} {
    :upvar \
        $package_id_var package_id \
        $user_id_var user_id \
        $ip_var ip

    if {![info exists package_id]} {
      if {[nsf::is object ::xo::cc]} {
        set package_id    [::xo::cc package_id]
      } elseif {[ns_conn isconnected]} {
        set package_id    [ad_conn package_id]
      } else {
        set package_id ""
      }
    }
    if {![info exists user_id]} {
      if {[nsf::is object ::xo::cc]} {
        set user_id    [::xo::cc user_id]
      } elseif {[ns_conn isconnected]} {
        set user_id    [ad_conn user_id]
      } else {
        set user_id 0
      }
    }
    if {![info exists ip]} {
      if {[ns_conn isconnected]} {
        set ip [ad_conn peeraddr]
      } else {
        set ip [ns_info address]
      }
    }
  }

  ::xo::db::Class instproc new_acs_object {
    -package_id
    -creation_user
    -creation_ip
    {-context_id ""}
    {object_title ""}
  } {
    :get_context package_id creation_user creation_ip

    set id [::xo::db::sql::acs_object new \
                -object_type [::xo::db::Class class_to_object_type [self]] \
                -title $object_title \
                -package_id $package_id \
                -creation_user $creation_user \
                -creation_ip $creation_ip \
                -context_id $context_id \
                -security_inherit_p [:security_inherit_p]]
    return $id
  }

  ::xo::db::Class instproc initialize_acs_object {obj id} {
    #
    # This method is called, whenever a new (fresh) object with
    # a new object_id is created.
    #
    $obj set object_id $id
    # construct the same object_title as acs_object.new() does
    $obj set object_title "[:pretty_name] $id"
    #$obj set object_type [:object_type]
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
    :get_context package_id creation_user creation_ip
    ::xo::dc transaction {
      set id [:new_acs_object \
                  -package_id $package_id \
                  -creation_user $creation_user \
                  -creation_ip $creation_ip \
                  ""]
      #[self class] set during_fetch 1
      ad_try {
        :create ::$id {*}$args
      } on error {errorMsg} {
        ad_log error "create fails: $errorMsg"
      }
      #[self class] unset during_fetch
      :initialize_acs_object ::$id $id
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
    {-as_ordered_composite:boolean true}
    {-object_class "::xotcl::Object"}
    {-named_objects:boolean false}
    {-object_named_after ""}
    {-destroy_on_cleanup:boolean true}
    {-ignore_missing_package_ids:boolean false}
    {-initialize true}
  } {

    Retrieve multiple objects from the database using the given SQL
    query and create XOTcl objects from the tuples.

    @param sql The SQL query to retrieve tuples. Note that if the SQL
    query only returns a restricted set of attributes, the objects
    will be only partially instantiated.

    @param as_ordered_composite return an ordered composite object
    preserving the order. If the flag is false, one has to use "info
    instances" to access the resulted objects.

    @param object_class specifies the XOTcl class, for which instances
    are created.

    @param named_objects If this flag is true, the value of the
    id_column is used for the name of the created objects (object will
    be named e.g. ::13738).  Otherwise, objects are created with the
    XOTcl "new" method to avoid object name clashes.

    @param destroy_on_cleanup If this flag is true, the objects (and
    ordered composite) will be automatically destroyed on cleaup
    (typically after the request was processed).

    @param initialize can be used to avoid full initialization, when a
    large series of objects is loaded. Per default, these objects are
    initialized via initialize_loaded_object, when the are of type
    ::xo::db::Object } {

    if {$object_class eq ""} {set object_class [self]}
    if {$sql eq ""} {set sql [:instance_select_query]}
    if {$as_ordered_composite} {
      set __result [::xo::OrderedComposite new]
      if {$destroy_on_cleanup} {$__result destroy_on_cleanup}
    } else {
      set __result [list]
    }
    if {$named_objects} {
      if {$object_named_after eq ""} {
        set object_named_after [:id_column]
      }
    }

    set sets [uplevel [list ::xo::dc sets -dbn $dbn [self proc] $sql]]
    foreach selection $sets {
      if {$named_objects} {
        set object_name ::[ns_set get $selection $object_named_after]
        if {[nsf::is object $object_name]} {
          set o $object_name
        } else {
          set o [$object_class create $object_name]
        }
      } else {
        set o [$object_class new]
      }
      if {$as_ordered_composite} {
        $__result add $o
      } else {
        if {$destroy_on_cleanup} {
          $o destroy_on_cleanup
        }
        lappend __result $o
      }
      #foreach {att val} [ns_set array $selection] {$o set $att $val}
      $o mset [ns_set array $selection]

      if {[$o exists object_type]} {
        #
        # Set the object type if it looks like managed from XOTcl.
        #
        set object_type [$o set object_type]
        if {[string match "::*" $object_type]} {
          $o class $object_type
        }
      }
      if {$initialize && [$o istype ::xo::db::Object]} {
        if {![$o exists package_id]} {
          if {[$o exists object_package_id]} {
            $o set package_id [$o set object_package_id]
          } elseif {!$ignore_missing_package_ids} {
            ns_log warning "[namespace tail [$o info class]] $o has no package_id and no object_package_id"
          }
        }
        ad_try {
          $o initialize_loaded_object
        } on error {errorMsg} {
          ns_log error "$o initialize_loaded_object => [$o info vars] -> $errorMsg"
        }
      }
      #:log "--DB more = $continue [$o serialize]"
    }

    return $__result
  }

  ::xo::db::Class instproc fetch_query {id} {
    set tables [list]
    set attributes [list]
    set id_column [:id_column]
    set join_expressions [list "[:table_name].$id_column = $id"]
    foreach cl [list [self] {*}[:info heritage]] {
      #if {$cl eq "::xo::db::Object"} break
      if {$cl eq "::xotcl::Object"} break
      set tn [$cl table_name]
      if {$tn  ne ""} {
        lappend tables $tn
        #:log "--db_slots of $cl = [$cl array get db_slot]"
        foreach {slot_name slot} [$cl array get db_slot] {
          # avoid duplicate output names
          set name [$slot name]
          if {![info exists names($name)]} {
            lappend attributes [$slot attribute_reference $tn]
          }
          set names($name) 1
        }
        if {$cl ne [self]} {
          lappend join_expressions "$tn.[$cl id_column] = [:table_name].$id_column"
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
    @param select_attributes attributes for the SQL query to be retrieved.
    if no attributes are specified, all attributes are retrieved.
    @param orderby for ordering the solution set
    @param where_clause clause for restricting the answer set
    @param count return the query for counting the solutions
    @return SQL query
  } {
    set tables [list]
    set id_column [:id_column]

    if {$count} {
      set select_attributes "count(*)"
      set orderby ""         ;# no need to order when we count
      set page_number  ""    ;# no pagination when count is used
    }

    set all_attributes [expr {$select_attributes eq ""}]
    set join_expressions [list]
    foreach cl [list [self] {*}[:info heritage]] {
      #if {$cl eq "::xo::db::Object"} break
      if {$cl eq "::xotcl::Object"} break
      set tn [$cl table_name]

      if {$tn  ne ""} {
        lappend tables $tn
        if {$all_attributes} {
          foreach {slot_name slot} [$cl array get db_slot] {
            # avoid duplicate output names
            set name [$slot name]
            if {![info exists names($name)]} {
              lappend select_attributes [$slot attribute_reference $tn]
            }
            set names($name) 1
          }
        }
        if {$cl ne [self]} {
          lappend join_expressions "$tn.[$cl id_column] = [:table_name].$id_column"
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

    set sql [::xo::dc select \
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
    {-initialize true}
  } {
    Returns a set (ordered composite) of the answer tuples of
    an 'instance_select_query' with the same attributes. Note that
    the returned objects might by partially instantiated.

    @return ordered composite
  } {
    set s [:instantiate_objects \
               -object_class [self] \
               -sql [:instance_select_query \
                         -select_attributes $select_attributes \
                         -from_clause $from_clause \
                         -where_clause $where_clause \
                         -orderby $orderby \
                         -page_size $page_size \
                         -page_number $page_number \
                        ] \
               -initialize $initialize]
    return $s
  }
  ##############

  ::xo::db::Class create ::xo::db::Object \
      -superclass ::xotcl::Object \
      -object_type "acs_object" \
      -pretty_name "Object" \
      -pretty_plural "Objects" \
      -table_name "acs_objects" -id_column "object_id"

  ::xo::db::Object instproc insert {} {:log no-insert;}

  ::xo::db::Object ad_instproc update {
    -package_id -modifying_user -context_id
  } {
    Update the current object in the database
  } {
    set object_id ${:object_id}
    if {![info exists package_id] && [info exists :package_id]} {
      set package_id ${:package_id}
    }
    if {![info exists context_id]} {
      set context_id [expr {[info exists :context_id] ? ${:context_id} : ""}]
    }
    [:info class] get_context package_id modifying_user modifying_ip
    set title ${:object_title}
    ::xo::dc dml update_object {
      update acs_objects set
         title          = :title,
         modifying_user = :modifying_user,
         modifying_ip   = :modifying_ip,
         context_id     = :context_id
      where object_id = :object_id
    }
    # Make sure object memory image reflects db values
    foreach att {modifying_user modifying_ip context_id} {
      set :${att} [set $att]
    }
    set :last_modified [::xo::dc get_value -prepare integer \
                            get_last_modified {
      select last_modified from acs_objects
      where object_id = :object_id
    }]
  }

  ::xo::db::Object ad_instproc delete {} {
    Delete the object from the database and from memory
  } {
    ::xo::db::sql::acs_object delete -object_id ${:object_id}
    :destroy
  }

  ::xo::db::Object ad_instproc save {-package_id -modifying_user -context_id} {
    Save the current object in the database
  } {
    set cmd [list :update]
    if {[info exists package_id]} {lappend cmd -package_id $package_id}
    if {[info exists modifying_user]} {lappend cmd -modifying_user $modifying_user}
    if {[info exists context_id]} {lappend cmd -context_id $context_id}
    {*}$cmd
  }

  ::xo::db::Object ad_instproc save_new {
    -package_id -creation_user -creation_ip -context_id
  } {
    Save the XOTcl Object with a fresh acs_object
    in the database.

    @return new object id
  } {
    if {![info exists package_id] && [info exists :package_id]} {
      set package_id ${:package_id}
    }
    if {![info exists context_id]} {
      set context_id [expr {[info exists :context_id] ? ${:context_id} : ""}]
    }
    [:info class] get_context package_id creation_user creation_ip
    ::xo::dc transaction {
      set id [[:info class] new_acs_object \
                  -package_id $package_id \
                  -creation_user $creation_user \
                  -creation_ip $creation_ip \
                  -context_id $context_id \
                  ""]
      [:info class] initialize_acs_object [self] $id
      :insert
    }
    return $id
  }

  ::xo::db::Object instproc initialize_loaded_object {} {
    #
    # This method is to be called, after an existing
    # object is fetched from the database.
    #
    # empty body, to be refined
  }


  ##############
  ::xotcl::MetaSlot create ::xo::db::Attribute \
      -superclass {::xo::Attribute} \
      -parameter {
        {sqltype}
        {column_name}
        {references ""}
        {min_n_values 1}
        {max_n_values 1}
        {create_acs_attribute true}
        {create_table_attribute true}
        {not_null}
        {unique}
        {index}
      }

  ::xo::db::Attribute instproc create_attribute {} {
    if {![:create_acs_attribute]} return

    set column_name ${:column_name}
    set object_type [${:domain} object_type]
    #ns_log notice "::xo::db::Attribute create_attribute $object_type $column_name epoch [ns_ictl epoch] [array get ::db_state_default]"

    if {[::xo::dc get_value check_att {select 0 from acs_attributes where
      attribute_name = :column_name and object_type = :object_type} 1]} {

      if {![::xo::db::Class object_type_exists_in_db -object_type $object_type]} {
        ${:domain} create_object_type
      }

      ::xo::db::sql::acs_attribute create_attribute \
          -object_type    $object_type \
          -attribute_name $column_name \
          -datatype       ${:datatype} \
          -pretty_name    ${:pretty_name} \
          -min_n_values   ${:min_n_values} \
          -max_n_values   ${:max_n_values}
      #:save
    }
  }

  ::xo::db::Attribute instproc attribute_reference {tn} {
    if {${:column_name} ne ${:name}} {
      return "$tn.${:column_name} AS ${:name}"
    } else {
      return "$tn.${:name}"
    }
  }

  ::xo::db::Attribute instproc column_spec {{-id_column false}} {
    set table_name [${:domain} table_name]
    set column_spec ""
    append column_spec " " [::xo::dc map_datatype ${:sqltype}]
    #
    # Default
    #
    if {[info exists :default]} {
      append column_spec " DEFAULT '${:default}' "
    }
    #
    # References
    #
    if {[info exists :references] && ${:references} ne ""} {
      append column_spec " REFERENCES ${:references}"
    } elseif {$id_column} {
      set sc [${:domain} info superclass]
      if {![$sc istype ::xo::db::Class]} {set sc ::xo::db::Object}
      append column_spec " REFERENCES [$sc table_name]([$sc id_column])\
        ON DELETE CASCADE "
    }
    #
    # Unique and Not NULL
    #
    if {[info exists :unique]}   {append column_spec " UNIQUE "  }
    if {[info exists :not_null]} {append column_spec " NOT NULL "}
    #
    # Primary key
    #
    if {$id_column} {
      # add automatically a constraint for the id_column
      append column_spec " PRIMARY KEY "
    }
    append column_spec [::xo::dc datatype_constraint ${:sqltype} $table_name ${:name}]
    return $column_spec
  }

  ::xo::db::Attribute instproc init {} {
    next    ;# do first ordinary slot initialization
    if {![info exists :sqltype]}     {set :sqltype     ${:datatype}}
    if {![info exists :column_name]} {set :column_name ${:name}}

    :create_attribute
  }

  ##############
  ::xotcl::MetaSlot create ::xo::db::CrAttribute \
      -superclass {::xo::db::Attribute}

  ::xo::db::CrAttribute instproc create_attribute {} {
    # do nothing, if create_acs_attribute is set to false
    if {![:create_acs_attribute]} return

    set column_name ${:column_name}
    set object_type [${:domain} object_type]

    if {$object_type eq "content_folder"} {
      # content_folder does NOT allow one to use create_attribute etc.
      return
    }

    #:log "check attribute $column_name object_type=$object_type, domain=${:domain}"
    if {[::xo::dc get_value check_att {select 0 from acs_attributes where
      attribute_name = :column_name and object_type = :object_type} 1]} {

      if {![::xo::db::Class object_type_exists_in_db -object_type $object_type]} {
        ${:domain} create_object_type
      }

      ::xo::db::sql::content_type create_attribute \
          -content_type   $object_type \
          -attribute_name $column_name \
          -datatype       ${:datatype} \
          -pretty_name    ${:pretty_name} \
          -column_spec    [:column_spec]
    }
  }


  ##############
  ::xo::db::Object slots {
    ::xo::db::Attribute create object_id    -pretty_name "Object ID" -datatype integer
    #::xo::db::Attribute create object_type  -pretty_name "Object Type"
    ::xo::db::Attribute create object_title \
        -pretty_name "Object Title" \
        -column_name title
    ::xo::db::Attribute create context_id \
        -pretty_name "Context ID" \
        -datatype integer \
        -column_name context_id
    ::xo::db::Attribute create security_inherit_p \
        -pretty_name "Security Inherit" \
        -datatype boolean \
        -column_name security_inherit_p
    ::xo::db::Attribute create package_id \
        -pretty_name "Package ID" \
        -datatype integer \
        -column_name package_id
    ::xo::db::Attribute create creation_user \
        -pretty_name "Creation User" \
        -datatype integer \
        -column_name creation_user
    ::xo::db::Attribute create creation_date \
        -pretty_name "Creation Date" \
        -datatype timestamp \
        -column_name creation_date
    ::xo::db::Attribute create creation_ip \
        -pretty_name "Creation IP Address" \
        -column_name creation_ip
    ::xo::db::Attribute create last_modified \
        -pretty_name "Last Modified On" \
        -datatype timestamp \
        -column_name last_modified
    ::xo::db::Attribute create modifying_user \
        -pretty_name "Modifying User" \
        -datatype integer \
        -column_name modifying_user
    ::xo::db::Attribute create modifying_ip \
        -pretty_name "Modifying IP Address" \
        -column_name modifying_ip
  }
  ::xo::db::Object db_slots
  ##############

  ##############
  # Handling temporary tables in PostgreSQL and Oracle via a common interface
  ##############

  ::xotcl::Class create ::xo::db::temp_table -parameter {name query vars}
  ::xo::db::temp_table instproc init {} {
    #
    # The cleanup order is - at least under AOLserver 4.01 - hard to get right.
    # When destroy_on_cleanup is executed, there might be already some global
    # data for the database interaction gone.... So, destroy these objects
    # by hand for now.
    # :destroy_on_cleanup
    #
    # The "GLOBAL" keyword on temp tables is deprecated (and currently
    # ignored) on PostgreSQL. Not sure, what's the state on various
    # Oracle versions. Therefore, we keep the comment for now.
    #

    if {![info exists :name]} {
      #
      # If there is no temp table name provided, create a name
      # avoiding name clashes. We assume, that the set of variables is
      # unique for such temp tables.
      #
      set :name "temp_table_[ns_md5 $vars]"
    }

    #
    # PRESERVE ROWS means that the data will be available until the
    # end of the SQL session..
    #
    set sql_create "CREATE temporary table ${:name} on commit PRESERVE ROWS as "

    # When the table exists already, simply insert into it ...
    if {[::xo::db::require exists_table ${:name}]} {
      ::xo::dc dml . "insert into ${:name} ([:vars]) (${:query})"
    } else {
      # ... otherwise, create the table with the data in one step
      ::xo::dc dml get_n_most_recent_contributions $sql_create${:query}
    }
  }
  ::xo::db::temp_table instproc destroy {} {
    # A session spans multiple connections in OpenACS.
    # We want to get rid the data when we are done.
    ::xo::dc dml truncate_temp_table "truncate table ${:name}"
    next
  }

  ##############
  ad_proc tcl_date {timestamp tz_var {secfrac_var ""}} {
    Convert the timestamp (coming from the database) into a format, which
    can be passed to Tcl's "clock scan".
  } {
    upvar $tz_var tz
    if {$secfrac_var ne ""} {
      upvar $secfrac_var secfrac
    }
    set tz 00
    set secfrac 0
    # Oracle style format like 2008-08-25 (no TZ, no sec frac)
    if {![regexp {^([0-9]+-[0-9]+-[0-9]+)$} $timestamp _ timestamp]} {
      # PostgreSQL type ANSI format secfrac and TZ
      if {![regexp {^([^.]+)[.]([0-9]*)([+-][0-9]*)$} $timestamp _ timestamp secfrac tz]} {
        # no TZ
        if {![regexp {^([^.]+)[.]([0-9]+)$} $timestamp _ timestamp secfrac]} {
          regexp {^([^.]+)([+-][0-9]*)$} $timestamp _ timestamp tz
        }
      }
    }
    return $timestamp
  }

  ad_proc list_to_values {
    list
    {type text}
  } {

    Convert a Tcl list into a quoted SQL VALUES expression

    Example:

    <pre>% list_to_values {1 2 3 4 5}
    (VALUES (1), (2), (3), (4), (5))</pre>

  } {
    set result {}
    foreach e $list {
      lappend result "([ns_dbquotevalue $e $type])"
    }
    return "(VALUES [join $result ,])"
  }
}

::xo::library source_dependent

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
