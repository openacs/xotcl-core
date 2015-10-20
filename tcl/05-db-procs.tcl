ad_library {
  XOTcl API for low level db abstraction

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
  # runtime, and multiple database context can be established,
  # although there is no high level support to connect to multiple
  # different OpenACS databases at the same time.
  #
  ##########################################################################

  #
  # Backend language specific (SQL Dialects)
  #
  ::xotcl::Class create ::xo::db::SQL
  ::xo::db::SQL abstract instproc map_datatype {type} 
  ::xo::db::SQL abstract instproc select {type} 
  ::xo::db::SQL abstract instproc date_trunc {type} 
  ::xo::db::SQL abstract instproc date_trunc_expression {type}

  #
  # generic (fallback) methods
  #
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
      ltree     { set type [expr {[my has_ltree] ? "ltree" : "text" }] }
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
    ns_cache eval xotcl_object_cache [self]::has_ltree {
      if {[my get_value check_ltree "select count(*) from pg_proc where proname = 'ltree_in'"] > 0} {
        return 1
      }
      return 0
    }
  }
  ::xo::db::postgresql instproc has_hstore {} {
    ns_cache eval xotcl_object_cache [self]::has_hstore {
      if {[my get_value check_ltree "select count(*) from pg_proc where proname = 'hstore_in'"] > 0} {
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
      ns_log debug "db_nextval: sequence($sequence) is not a real sequence.  perhaps it uses the view hack."
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

  ::xo::db::oracle instproc map_datatype {type} {
    switch -- $type {
      string    { set type varchar2(1000) }
      text      { set type varchar2(4000) }
      long_text { set type clob }
      boolean   { set type char(1) }
      ltree     { set type varchar2(1000) }
    }
    return $type
  }
  ::xo::db::oracle instproc datatype_constraint {type table att} {
    set constraint ""
    switch -- $type {
      boolean {
        set cname [my mk_sql_constraint_name $table $att _ck]
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
  ::xo::db::oracle instproc date_trunc {field date} {
    return "to_char(trunc($date,'$field'), 'YYYY-MM-DD HH24:MI:SS')"
  }
  ::xo::db::oracle instproc date_trunc_expression {field date date_string} {
    if {![string match :* $date_string]} {set date_string "'$date_string'"}
    return "trunc($date,'$field') = trunc(to_date($date_string,'YYYY-MM-DD'),'$field')"
  }
  ::xo::db::oracle instproc mk_sql_constraint_name {table att suffix} {
    #
    # Constaint names are limited in oracle to 30 characters;
    # Postgres has no such limits. Therefore, we use different
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

  ::xotcl::Class create ::xo::db::Driver
  ::xo::db::Driver abstract instproc sets           {{-dbn ""} {-bind ""} qn sql} 
  ::xo::db::Driver abstract instproc 0or1row        {{-dbn ""} {-bind ""} qn sql}
  ::xo::db::Driver abstract instproc 1row           {{-dbn ""} {-bind ""} qn sql}
  ::xo::db::Driver abstract instproc list_of_lists  {{-dbn ""} {-bind ""} qn sql}
  ::xo::db::Driver abstract instproc list           {{-dbn ""} {-bind ""} qn sql}
  ::xo::db::Driver abstract instproc dml            {{-dbn ""} {-bind ""} qn sql}
  ::xo::db::Driver abstract instproc foreach        {{-dbn ""} {-bind ""} qn sql script}
  ::xo::db::Driver abstract instproc transaction    {{-dbn ""} script args}
  ::xo::db::Driver abstract instproc ds {onOff}

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
    my uplevel 2 [list subst $sql]
  }


  ##########################################################################
  #
  # DBI support
  #
  ::xo::db::DBI instproc profile {onOff} {
    if {$onOff} {
      my mixin ::xo::db::DBI::Profile
    } else {
      if {[my info mixin] ne ""} {my mixin ""}
    }
  }

  ::xo::db::DBI instproc sets {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [my uplevel [list dbi_rows -result sets {*}$bindOpt -- $sql]]
  }

  #
  # foreach based on "dbi_rows + results avlists"
  #
  ::xo::db::DBI instproc foreach {{-dbn ""} {-bind ""} qn sql body} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set avlists [my uplevel [list dbi_rows -result avlists {*}$bindOpt -- $sql]]
    foreach avlist $avlists {
      foreach {a v} $avlist {my uplevel [list set $a $v]}
      my uplevel $body
    }
  }
  #
  # foreach based on "dbi_eval"
  #
  #::xo::db::DBI instproc foreach {{-dbn ""} {-bind ""} qn sql body} {
  #  if {$sql eq ""} {set sql [my get_sql $qn]}
  #  if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
  #  my uplevel [list dbi_foreach $sql $body]
  #}

  ::xo::db::DBI instproc 0or1row {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [my uplevel [list ::dbi_0or1row {*}$bindOpt $sql]]
  }
  ::xo::db::DBI instproc 1row {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [my uplevel [list ::dbi_1row {*}$bindOpt $sql]]
  }
  ::xo::db::DBI instproc list_of_lists {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [my uplevel [list ::dbi_rows -result lists -max 1000000 {*}$bindOpt -- $sql]]
  }
  ::xo::db::DBI instproc list {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set flat [my uplevel [list ::dbi_rows -columns __columns {*}$bindOpt -- $sql]]
    if {[my uplevel {llength $__columns}] > 1} {error "query is returing more than one column"}
    return $flat
  }
  ::xo::db::DBI instproc dml {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    return [my uplevel [list ::dbi_dml {*}$bindOpt -- $sql]]
  }
  ::xo::db::DBI instproc transaction {{-dbn ""} script args} {
    if {$args ne ""} {
      lassign $args op on_error_code
      set result ""
      if {$op ne "on_error"} {error "only 'on_error' as argument after script allowed"}
      if {[catch {
        set result [my uplevel [list ::dbi_eval -transaction committed $script]]
      }]} {
        my uplevel $on_error_code
      }
      return $result
    } else {
      return [my uplevel [list ::dbi_eval -transaction committed $script]]
    }
  }

  ::xo::db::DBI instproc get_value {{-dbn ""} qn sql {default ""}} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    set answers [my uplevel [list ::dbi_rows -result sets -max 1 $sql]]
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
      if {$sql eq ""} {set sql [my get_sql $qn]}
      set start_time [expr {[clock clicks -microseconds]/1000.0}]
      set result [next]
      ds_add db $dbn [my ds_map [self proc]] $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
      return $result
    }
  }

  #
  # foreach based on "dbi_rows + results avlists"
  #
  ::xo::db::DBI::Profile instproc foreach {{-dbn ""} {-bind ""} qn sql body} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set start_time [expr {[clock clicks -microseconds]/1000.0}]
    set avlists [my uplevel [list dbi_rows -result avlists {*}$bindOpt -- $sql]]
    ds_add db $dbn "exec foreach" $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
    foreach avlist $avlists {
      foreach {a v} $avlist {my uplevel [list set $a $v]}
      my uplevel $body
    }
  }

  #
  # foreach based on "dbi_foreach"
  #
  #::xo::db::DBI::Profile instproc foreach {{-dbn ""} {-bind ""} qn sql body} {
  #  if {$sql eq ""} {set sql [my get_sql $qn]}
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
  # :uplevel (the mixin should be transparant). Without "ns_cache eval"
  # things look fine.
  ::xo::db::DBI::Profile instproc 1row {{-dbn ""} {-bind ""} qn sql} {
    set start_time [expr {[clock clicks -microseconds]/1000.0}]
    set result [my uplevel [list ::dbi_1row $sql]]
    ds_add db $dbn [my ds_map [self proc]] $qn $sql $start_time [expr {[clock clicks -microseconds]/1000.0}] 0 ""
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
    return [my uplevel [list ::db_transaction -dbn $dbn $script {*}$args]]
  }

  ::xo::db::DB instproc sets {{-dbn ""} {-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    db_with_handle -dbn $dbn db {
      set result [list]
      set answers [uplevel [list ns_pg_bind select $db {*}$bindOpt $sql]]
      while { [::db_getrow $db $answers] } {
        lappend result [ns_set copy $answers]
      }
    }
    return $result
  }

  ::xo::db::DB instproc foreach {{-dbn ""} {-bind ""} qn sql body} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    set qn [uplevel [list [self] qn $qn]]
    uplevel [list ::db_foreach -dbn $dbn $qn $sql $body {*}$bindOpt]
  }

  ::xo::db::DB instproc exec_0or1row {{-bind ""} sql} {
    # Helper, used from several postgres-spefic one-tuple queries
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    ::db_with_handle h {
      return [uplevel [list ns_pg_bind 0or1row $h {*}$bindOpt $sql]]
    }
  }

  ::xo::db::DB instproc 0or1row {{-dbn ""} {-bind ""} qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_0or1row [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }
  ::xo::db::DB instproc 1row {{-dbn ""} {-bind ""} qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_1row [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }
  ::xo::db::DB instproc dml {{-dbn ""} {-bind ""} qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_dml [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
    return [db_resultrows]
  }
  ::xo::db::DB instproc get_value {{-dbn ""} {-bind ""} qn sql {default ""}} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_string [uplevel [list [self] qn $qn]] $sql -default $default {*}$bindOpt]
  }
  ::xo::db::DB instproc list_of_lists {{-bind ""} qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_list_of_lists [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }
  ::xo::db::DB instproc list {{-bind ""} qn sql} {
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    uplevel [list ::db_list [uplevel [list [self] qn $qn]] $sql {*}$bindOpt]
  }

  proc ::xo::db::pg_0or1row {sql} {
    ns_log notice "::xo::db::pg_0or1row decprecated"
    ::db_with_handle h {
      return [uplevel [list ns_pg_bind 0or1row $h {*}$bindOpt $sql]]
    }
  }

  #
  # The view-insert operation is an operation inserting into a view as eg. in
  #
  #    insert into xowiki_form_pagei (...) values (...)
  #
  # Depending on the version of the CR and the driver, we need
  # different calls to the driver. The default operation is "dml", but
  # in the DB-postgres combo, we need 0or1row.
  # Provide the appropriate db_* call for the view update. Earlier
  # versions up to 5.3.0d1 used db_dml, newer versions (since around
  # july 2006) have to use 0or1row, when the patch for deadlocks
  # and duplicate items was applied...
  #
  apm_version_get -package_key acs-content-repository -array info
  array get info
  if {[apm_version_names_compare $info(version_name) 5.3.0d1] >= 1} {
    ::xo::db::DB-postgresql instproc insert-view-operation {} { return 0or1row }
  }
  array unset info

  # the default insert-view operation
  ::xo::db::Driver instproc insert-view-operation {} { return dml }


  #
  # DB driver functions, optimized for PostgreSQL
  #
  ::xo::db::DB-postgresql instproc 0or1row {{-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    set answers [uplevel [list [self] exec_0or1row -bind $bind $sql]]
    if {$answers ne ""} {
      foreach {att val} [ns_set array $answers] { uplevel [list set $att $val] }
      ns_set free $answers
      return 1
    }
    return 0
  }
  ::xo::db::DB-postgresql instproc 1row {{-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    set answers [uplevel [list [self] exec_0or1row -bind $bind $sql]]
    if {$answers ne ""} {
      foreach {att val} [ns_set array $answers] { uplevel [list set $att $val] }
      ns_set free $answers
      return 1
    }
    error "query $sql did not return an answer"
  }
  ::xo::db::DB-postgresql instproc get_value {{-bind ""} qn sql {default ""}} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    set answers [uplevel [list [self] exec_0or1row -bind $bind $sql]]
    if {$answers ne ""} {
      set result [ns_set value $answers 0]
      ns_set free $answers
      return $result
    }
    return $default
  }
  ::xo::db::DB-postgresql instproc list_of_lists {{-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    db_with_handle db {
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
  ::xo::db::DB-postgresql instproc list {{-bind ""} qn sql} {
    if {$sql eq ""} {set sql [my get_sql $qn]}
    if {$bind ne ""} {set bindOpt [list -bind $bind]} {set bindOpt ""}
    db_with_handle db {
      set result [list]
      set answers [uplevel [list ns_pg_bind select $db {*}$bindOpt $sql]]
      while { [::db_getrow $db $answers] } {
        lappend result [ns_set value $answers 0]
      }
      ns_set free $answers
    }
    return $result
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
    ::xo::db::$driver-$sqlDialect create ::xo::dc
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
  # already. This change makes the object-cache-init.tcl
  # obsolete.
  #
  # Unfortunately, ns_cache has no command to check, whether
  # a cache exists, so we use the little catch below to check.
  #
  if {[catch {ns_cache flush xotcl_object_cache NOTHING}]} {
    ns_log notice "xotcl-core: creating xotcl-object caches"

    ns_cache create xotcl_object_cache \
        -size [parameter::get_from_package_key \
                   -package_key xotcl-core \
                   -parameter XOTclObjectCacheSize \
                   -default 400000]

    ns_cache create xotcl_object_type_cache \
        -size [parameter::get_from_package_key \
                   -package_key xotcl-core \
                   -parameter XOTclObjectTypeCacheSize \
                   -default 10000]
  }



  ad_proc -deprecated has_ltree {} {
    Check, whether ltree is available (postgres only)
  } {
    ::xo::dc has_ltree
  }

  ad_proc -deprecated has_hstore {} {
    Check, whether hstore is available (postgres only)
  } {
    ::xo::dc has_hstore
  }


  #
  # The object require provides an interface to create certain
  # resources in case they are not created already.
  #
  
  # Installations with acs-kernel prior to 5.8.1a6 (or later, before running upgrade script) 
  # won't have these procs. We define them here if missing to avoid breaking running instances during transition.
  if {![::xotcl::Class isobject "::xo::db::sql::util"]} {
      ::xotcl::Class create ::xo::db::sql::util 
  }
  if {[::xo::db::sql::util info commands table_exists] eq ""} {
    ::xo::db::sql::util ad_proc table_exists {-name:required} {Transitional method} {
      set query [expr {[db_driverkey ""] eq "oracle" ? 
	{select 1 from user_tables  where table_name = :name} : 
	{select 1 from pg_class where relname = :name and pg_table_is_visible(oid)}}]
      ::xo::dc 0or1row query $query
    }
  }
  if {[::xo::db::sql::util info commands view_exists] eq ""} {
    ::xo::db::sql::util ad_proc view_exists {-name:required} {Transitional method} {
      set query [expr {[db_driverkey ""] eq "oracle" ? 
	{select 1 from user_views   where view_name  = :name} : 
	{select 1 from pg_views     where viewname   = :name}}]
      ::xo::dc 0or1row query $query
    }
  }
  if {[::xo::db::sql::util info commands index_exists] eq ""} {
    ::xo::db::sql::util ad_proc index_exists {-name:required} {Transitional method} {
      set query [expr {[db_driverkey ""] eq "oracle" ? 
	{select 1 from user_indexes where index_name = :name} : 
	{select 1 from pg_indexes   where indexname  = :name}}]
      ::xo::dc 0or1row query $query
    }
  }
  if {[::xo::db::sql::util info commands table_column_exists] eq ""} {
    ::xo::db::sql::util ad_proc table_column_exists {-t_name:required -c_name:required} {Transitional method} {
      set query [expr {[db_driverkey ""] eq "oracle" ? 
	{select 1 from user_tab_columns where table_name = :t_name and column_name = :c_name} : 
	{select 1 from information_schema.columns where table_name = :t_name and column_name = :c_name}}]
      ::xo::dc 0or1row query $query
    }
  }
  ###
  
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
	-t_name $table_name \
	-c_name $column_name
  }

  require proc table {name definition {populate ""}} {
    #my log "==== require table $name exists: [my exists_table $name]\n$definition"
    if {![my exists_table $name]} {
      set lines {}
      foreach col [dict keys $definition] {lappend lines "$col [dict get $definition $col]"}
      set definition [join $lines ",\n"]
      # my log "--table $name does not exist, creating with definition: $definition"
      ::xo::dc dml create-table-$name "create table $name ($definition)"
      if {$populate ne ""} {
        ::xo::dc dml populate-table-$name $populate
      }
    } else {
      # The table exists already. Check the colums, whether we have to
      # add columns. We do not alter attribute types, and we do not
      # delete columns.
      foreach col [dict keys $definition] {
        if {![my exists_column $name $col]} {
          ns_log notice "xodb: adding column <alter table $name add column $col [dict get $definition $col]>"
          ::xo::dc dml alter-table-$name \
              "alter table $name add column $col [dict get $definition $col]"
        }
      }
    }
  }

  require proc view {name definition} {
    if {[db_driverkey ""] eq "oracle"} {set name [string toupper $name]}
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

  require proc package {package_key} {
    if {![my exists required_package($package_key)]} {
      foreach path [apm_get_package_files \
                        -package_key $package_key \
                        -file_types tcl_procs] {
        # Use apm_source instead of source to prevent double
        # sourcing by the apm_loader (temporary solution, double
        # sourcing should no happen)
        uplevel #1 apm_source "[acs_root_dir]/packages/$package_key/$path"
      }
      my set required_package($package_key) 1
    }
  }

  require ad_proc function_args {
    -kernel_older_than
    -package_key_and_version_older_than
    -check_function 
    sql_file 
  } {
    Load the sql file, if the the kernel is older than the 
    specified version, and the version of the specified package is older,
    and the check_function does not exist in function_args.
    <p>
    Sample usage: <tt>
    ::xo::db::require function_args \<br>
    &nbsp;&nbsp;-kernel_older_than 5.5.0 \<br>
    &nbsp;&nbsp;-older_than_package_key_and_version "xowiki 0.50" \<br>
    &nbsp;&nbsp;-check_function "acs_object_type__create_type" \<br>
    &nbsp;&nbsp;[acs_package_root_dir xotcl-request-broker]/patches/funcs-1.sql</tt>
  } {
    if {[db_driverkey ""] eq "postgresql"} {
      # only necessary with postgres
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

      if {[file readable $sql_file]} {
        my log "Sourcing '$sql_file'"
        db_source_sql_file $sql_file
        ::xo::db::Class create_all_functions
        return 1
      } else {
        my log "Could not source '$sql_file'"
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
    return [ns_cache eval xotcl_object_type_cache $id {
      ::xo::dc 1row get_class "select object_type from acs_objects where object_id=:id"
      return $object_type
    }]
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
    $r db_1row get_instance [$class fetch_query $id]
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
    # some table_names and id_columns in acs_object_types are unfortunately upper case, 
    # so we have to convert to lower case here....
    ::xo::dc 1row fetch_class {
      select object_type, supertype, pretty_name, lower(id_column) as id_column, lower(table_name) as table_name
      from acs_object_types where object_type = :object_type
    }
    set classname [my object_type_to_class $object_type]
    if {![my isclass $classname]} {
      # the XOTcl class does not exist, we create it
      #my log "--db create class $classname superclass $supertype"
      ::xo::db::Class create $classname \
          -superclass [my object_type_to_class $supertype] \
          -object_type $object_type \
          -supertype $supertype \
          -pretty_name $pretty_name \
          -id_column $id_column \
          -table_name $table_name \
          -sql_package_name [namespace tail $classname] \
          -noinit
    } else {
      #my log "--db we have a class $classname"
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
      if {[my exists exclude_attribute($table_name,$attribute_name)]} {
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
    if {[catch {$classname slots $slots} errorMsg]} {
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
    # Load defintions in one step from function args; only for
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
    # Note, that we can as well get the type in future versions.
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
          regexp {^[^a-zA-Z]+([a-zA-Z0-9_]+)\s} $line _ fq_name
          if {![info exists fq_name]} {
            ns_log notice "--***** Could not retrieve argument name for $proname\
            argument $n from line '$line' in $prosrc'"
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
  # The generation of the code interface code is driver specifc, since
  # e.q. dbi supports option "-autonull", which simplified the
  # interface code significantly
  #

  #
  #  DBI interface method generation (with autonull):
  #

  ::xo::db::DBI instproc generate_psql {package_name object_name} {
    set function_args [my get_function_args $package_name $object_name]
    set function_args [my fix_function_args $function_args $package_name $object_name]
    set sql_info [my sql_arg_info $function_args $package_name $object_name]
    #ns_log notice "-- select ${package_name}__${object_name}($psql_args)"
    dict set sql_info sql [subst { select ${package_name}__${object_name}([dict get $sql_info psql_args]) }]
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
  #  DB and Postgres interface method generation (no autonull):
  #

  ::xo::db::DB-postgresql instproc generate_psql {package_name object_name} {
    set function_args [my get_function_args $package_name $object_name]
    set function_args [my fix_function_args $function_args $package_name $object_name]
    set sql_info [my sql_arg_info $function_args $package_name $object_name]
    #ns_log notice "-- select ${package_name}__${object_name} ($psql_args)"
    set sql [subst {
      select ${package_name}__${object_name}([dict get $sql_info psql_args])
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

    set function_args [my get_function_args $package_name $object_name]
    set function_args [my fix_function_args $function_args $package_name $object_name]
    set sql_info [my sql_info $function_args $package_name $object_name]

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
        #my log "sql=$sql, sql_command=$sql_cmd"
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
    # The current implementation should work on postgres and oracle (not tested)
    # but will not work, when a single OpenACS instance want to talk to 
    # postgres and oracle simultaneously. Not sure, how important this is...
    #
    if {$object_name eq "set"} {
      my log "We cannot handle object_name = '$object_name' in this version"  
      return
    }
    #
    # Object names have the form of e.g. ::xo::db::apm_parameter.
    # Therefore, we use the namspace tail as sql_package_name.
    #
    set package_name  [my sql_package_name [namespace tail [self]]]
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
    # appended. we have to added it here to avoid complains. xotcl 2.0
    # should find better ways to handle contain or the new invocation.
    if {$object_name eq "new"} {lappend nonposarg_list -childof}
    #my log "-- define $object_name $nonposarg_list"

    #ns_log notice final=[dict get $sql_info body]
    my ad_proc $object_name $nonposarg_list {Automatically generated method} [dict get $sql_info body]
  }

  ::xo::db::Class instproc unknown {m args} {
    error "Error: unknown database method '$m' for [self]"
  }
  
  ::xo::db::Class proc create_all_functions {} {

    foreach item [::xo::dc get_all_package_functions] {
      lassign $item package_name object_name

      if {[string match "*TRG" [string toupper $object_name]]} {
        # no need to provide interfae to trigger functions
        continue
      }
      
      set class_name ::xo::db::sql::[string tolower $package_name] 
      if {![my isobject $class_name]} {
        ::xo::db::Class create $class_name
      } elseif {![$class_name istype ::xo::db::Class]} {
        #
        # The methods of ::xo::db::sql::util like "table_exists" fall
        # into this category. Make sure, that we do not create new
        # objects via the next command.
        #
        continue
      }
      $class_name dbproc_nonposargs [string tolower $object_name]
    }
  }
  
  ::xo::db::Class proc class_to_object_type {name} {
    if {[my isclass $name]} {
      if {[$name exists object_type]} {
        # The specified class has an object_type defined; return it
        return [$name object_type]
      }
      if {![$name istype ::xo::db::Object]} {
        # The specified class is not subclass of ::xo::db::Object.
        # return acs_object in your desparation.
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
      my set object_type_key [::xo::dc list get_tree_sortkey {
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
    return [::xo::dc list get_object_types \
                [my object_types_query -subtypes_first $subtypes_first]]
  }

  ::xo::db::Class ad_instproc create_object_type {} {
    Create an acs object_type for the current XOTcl class
  } {
    my instvar object_type supertype pretty_name pretty_plural \
        table_name id_column name_method abstract_p

    my check_default_values
    my check_table_atts

    # The default supertype is acs_object. If the supertype
    # was not changed (still acs_object), we map the superclass 
    # to the object_type to obtain the ACS supertype.
    if {$supertype eq "acs_object"} {
      set supertype [::xo::db::Class class_to_object_type [my info superclass]]
    }

    ::xo::db::sql::acs_object_type create_type \
        -object_type $object_type \
        -supertype $supertype \
        -pretty_name $pretty_name \
        -pretty_plural $pretty_plural \
        -table_name $table_name \
        -id_column $id_column \
        -abstract_p $abstract_p \
        -name_method $name_method \
        -package_name [my sql_package_name]
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
    #my log "--setting db_slot all=[my info slots]"
    foreach att [my info slots] {
      #my log "--checking $att [$att istype ::xo::db::Attribute] [$att info class]"
      if {![$att istype ::xo::db::Attribute]} continue
      set db_slot([$att name]) $att
    }
    if {[self] ne "::xo::db::Object"} {
      if {[my exists id_column] && ![info exists db_slot($id_column)]} {
        # create automatically the slot for the id column
        my slots [subst {
          ::xo::db::Attribute create $id_column \
              -pretty_name "ID" \
              -datatype integer \
              -create_acs_attribute false
        }]
        set db_slot($id_column) [self]::slot::$id_column
      }
    }
    #my log "--setting db_slot of [self] to [array names db_slot]"
  }

  ::xo::db::Class instproc table_definition {} {
    my instvar id_column table_name db_slot
    array set column_specs [list]
    #
    # iterate over the slots and collect the column_specs for table generation
    #
    foreach {slot_name slot} [my array get db_slot] {
      if {![$slot create_table_attribute]} continue
      set column_name [$slot column_name]
      set column_specs($column_name) \
          [$slot column_spec -id_column [expr {$column_name eq $id_column}]]
    }

    if {[array size column_specs] > 0} {
      if {$table_name eq ""} {error "no table_name specified"}
      if {$id_column eq ""}  {error "no id_column specified"}
      if {![info exists column_specs($id_column)]} {
        error "no ::xo::db::Attribute slot for id_column '$id_column' specified"
      }
      set table_specs [list]
      foreach {att spec} [array get column_specs] {lappend table_specs $att $spec}
      set table_definition $table_specs
    } else {
      set table_definition ""
    }
    # my log table_definition=$table_definition
    return $table_definition
  }

  ::xo::db::Class instproc mk_update_method {} {
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
    my instproc update {} [subst {
      ::xo::dc transaction {
        next
        my instvar object_id $vars
        ::xo::dc dml update_[my table_name] {update [my table_name]
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
      ::xo::dc dml insert_$__table_name "insert into $__table_name
        ([join $__atts ,]) values (:[join $__vars ,:])"
    }
  }
  
  ::xo::db::Class ad_instproc check_table_atts {} {
    Check table_name and id_column and set meaningful
    defaults, if these attributes are not provided.
  } {
    my check_default_values
    set table_name_error_tail ""
    set id_column_error_tail ""
    my instvar sql_package_name

    if {![my exists sql_package_name]} {
      set sql_package_name [self]
      #my log "-- sql_package_name of [self] is '$sql_package_name'"
    }
    if {[string length $sql_package_name] > 30} {
      error "SQL package_name '$sql_package_name' can be maximal 30 characters long!\
        Please specify a shorter sql_package_name in the class definition."
    }
    if {$sql_package_name eq ""} {
      error "Cannot determine SQL package_name. Please specify it explicitely!"
    }

    if {![my exists table_name]} {
      set tail [namespace tail [self]]
      regexp {^::([^:]+)::} [self] _ head
      my table_name [string tolower ${head}_$tail]
      #my log "-- table_name of [self] is '[my table_name]'"
      set table_name_error_tail ", or use different namespaces/class names"
    }

    if {![my exists id_column]} {
      my set id_column [string tolower [namespace tail [self]]]_id
      set id_column_error_tail ", or use different class names"
      #my log "-- created id_column '[my id_column]'"
    }

    if {![regexp {^[[:alpha:]_][[:alnum:]_]*$} [my table_name]]} {
      error "Table name '[my table_name]' is unsafe in SQL: \
    Please specify a different table_name$table_name_error_tail." 
    }

    if {[string length [my table_name]] > 30} {
      error "SQL table_name '[my table_name]' can be maximal 30 characters long!\
        Please specify a shorter table_name in the class definition."
    }

    if {![regexp {^[[:alpha:]_][[:alnum:]_]*$} [my id_column]]} {
      error "Name for id_column '[my id_column]' is unsafe in SQL: \
        Please specify a different id_column$id_column_error_tail" 
    }
  }

  ::xo::db::Class instproc check_default_values {} {
    my instvar pretty_name pretty_plural
    if {![info exists pretty_name]}   {set pretty_name [namespace tail [self]]}
    if {![info exists pretty_plural]} {set pretty_plural $pretty_name}
  }

  ::xo::db::Class instproc init {} {

    if {![::xo::db::Class object_type_exists_in_db -object_type [my object_type]]} {
      my create_object_type
    }
    my init_type_hierarchy
    my check_table_atts
    my db_slots

    if {[my with_table]} {
      set table_definition [my table_definition]
      if {$table_definition ne ""} {
        ::xo::db::require table [my table_name] $table_definition
      }
      
      my mk_update_method
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
      if {[info commands ::xo::cc] ne ""} {
        set package_id    [::xo::cc package_id]
      } elseif {[ns_conn isconnected]} {
        set package_id    [ad_conn package_id]
      } else {
        set package_id ""
      }
    }
    if {![info exists user_id]} {
      if {[info commands ::xo::cc] ne ""} {
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
    #
    # This method is called, whenever a new (fresh) object with
    # a new object_id is created.
    #
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
    ::xo::dc transaction {
      set id [my new_acs_object \
                  -package_id $package_id \
                  -creation_user $creation_user \
                  -creation_ip $creation_ip \
                  ""]
      #[self class] set during_fetch 1
      if {[catch {my create ::$id {*}$args} errorMsg]} {
        ad_log error $errorMsg
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
    query only returns a restricted set of attributes, the objects will
    be only partially instantiated.

    @param as_ordered_composite return an ordered composite object
    preserving the order. If the flag is false, one has to use
    "info instances" to access the resulted objects.

    @param object_class specifies the XOTcl class, for which instances
    are created.

    @param named_objects If this flag is true, the value of the id_column is used
    for the name of the created objects (object will be named e.g. ::13738). 
    Otherwise, objects are created with the XOTcl "new" method to avoid object name clashes.

    @param destroy_on_cleanup If this flag is true, the objects (and ordered composite) 
      will be automatically destroyed on cleaup (typically                                                                                      after the request was processed).

    @param initialize can be used to avoid full initialization, when
    a large series of of objects is loaded. Per default, these objects
    are initialized via initialize_loaded_object, when the are 
    of type ::xo::db::Object
  } {

    if {$object_class eq ""} {set object_class [self]}
    if {$sql eq ""} {set sql [my instance_select_query]}
    if {$as_ordered_composite} {
      set __result [::xo::OrderedComposite new]
      if {$destroy_on_cleanup} {$__result destroy_on_cleanup}
    } else {
      set __result [list]
    }
    if {$named_objects} {
      if {$object_named_after eq ""} {
        set object_named_after [my id_column]
      }
    }

    set sets [uplevel [list ::xo::dc sets -dbn $dbn [self proc] $sql]]
    foreach selection $sets {
      if {$named_objects} {
        set object_name ::[ns_set get $selection $object_named_after]
        if {[info commands $object_name] eq ""} {
          set o [$object_class create $object_name]
        } else {
          set o $object_name
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
      foreach {att val} [ns_set array $selection] {$o set $att $val}
      if {[$o exists object_type]} {
        # set the object type if it looks like managed from XOTcl
        if {[string match "::*" [set ot [$o set object_type]] ]} {
          $o class $ot
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
        if {[catch {$o initialize_loaded_object} errorMsg]} {
          ns_log error "$o initialize_loaded_object => [$o info vars] -> $errorMsg"
        }
      }
      #my log "--DB more = $continue [$o serialize]" 
    }

    return $__result
  }
  
  ::xo::db::Class instproc fetch_query {id} {
    set tables [list]
    set attributes [list]
    set id_column [my id_column]
    set join_expressions [list "[my table_name].$id_column = $id"]
    foreach cl [concat [self] [my info heritage]] {
      #if {$cl eq "::xo::db::Object"} break
      if {$cl eq "::xotcl::Object"} break
      set tn [$cl table_name]
      if {$tn  ne ""} {
        lappend tables $tn
        #my log "--db_slots of $cl = [$cl array get db_slot]"
        foreach {slot_name slot} [$cl array get db_slot] {
          # avoid duplicate output names
          set name [$slot name]
          if {![info exists names($name)]} {
            lappend attributes [$slot attribute_reference $tn]
          }
          set names($name) 1
        }
        if {$cl ne [self]} {
          lappend join_expressions "$tn.[$cl id_column] = [my table_name].$id_column"
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
            # avoid duplicate output names
            set name [$slot name]
            if {![info exists names($name)]} {
              lappend select_attributes [$slot attribute_reference $tn]
            }
            set names($name) 1
          }
        }
        if {$cl ne [self]} {
          lappend join_expressions "$tn.[$cl id_column] = [my table_name].$id_column"
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
    an 'instance_select_query' with the same attributes. Note, that
    the returned objects might by partially instantiated.

    @return ordered composite
  } {
    set s [my instantiate_objects \
               -object_class [self] \
               -sql [my instance_select_query \
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

  ::xo::db::Object instproc insert {} {my log no-insert;}

  ::xo::db::Object ad_instproc update {-package_id -modifying_user} {
    Update the current object in the database
  } {
    my instvar object_id
    if {![info exists package_id] && [my exists package_id]} {
      set package_id [my package_id]
    }
    [my info class] get_context package_id modifying_user modifying_ip
    ::xo::dc dml update_object {update acs_objects 
      set modifying_user = :modifying_user, modifying_ip = :modifying_ip
      where object_id = :object_id}
  }

  ::xo::db::Object ad_instproc delete {} {
    Delete the object from the database and from memory
  } {
    ::xo::db::sql::acs_object delete -object_id [my set object_id]
    my destroy
  }

  ::xo::db::Object ad_instproc save {-package_id -modifying_user} {
    Save the current object in the database
  } {
    set cmd [list my update]
    if {[info exists package_id]} {lappend cmd -package_id $package_id}
    if {[info exists modifying_user]} {lappend cmd -modifying_user $modifying_user}
    {*}$cmd
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
    ::xo::dc transaction {
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
      }

  ::xo::db::Attribute instproc create_attribute {} {
    if {![my create_acs_attribute]} return

    my instvar datatype pretty_name min_n_values max_n_values domain column_name
    set object_type [$domain object_type]
    if {[::xo::dc get_value check_att {select 0 from acs_attributes where 
      attribute_name = :column_name and object_type = :object_type} 1]} {

      if {![::xo::db::Class object_type_exists_in_db -object_type $object_type]} {
        $domain create_object_type
      }

      ::xo::db::sql::acs_attribute create_attribute \
          -object_type $object_type \
          -attribute_name $column_name \
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
    append column_spec " " [::xo::dc map_datatype $sqltype]
    if {[info exists default]} {append column_spec " DEFAULT '$default'" }
    #
    # References
    #
    if {[info exists references] && $references ne ""} {
      append column_spec " REFERENCES $references" 
    } elseif {$id_column} {
      set sc [[my domain] info superclass]
      if {![$sc istype ::xo::db::Class]} {set sc ::xo::db::Object}
      append column_spec " REFERENCES [$sc table_name]([$sc id_column])\
        ON DELETE CASCADE"
    }
    #
    # Constraints
    #
    set table_name [[my domain] table_name]
    if {$id_column} {
      # add automatically a constraint for the id_column
      set cname [::xo::dc mk_sql_constraint_name $table_name $name pk]
      append column_spec "\n\tCONSTRAINT $cname PRIMARY KEY"
    }
    append column_spec " " [::xo::dc datatype_constraint $sqltype $table_name $name]
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
  ::xotcl::MetaSlot create ::xo::db::CrAttribute \
      -superclass {::xo::db::Attribute}

  ::xo::db::CrAttribute instproc create_attribute {} {
    # do nothing, if create_acs_attribute is set to false
    if {![my create_acs_attribute]} return

    my instvar name column_name datatype pretty_name domain
    set object_type [$domain object_type]

    if {$object_type eq "content_folder"} {
      # content_folder does NOT allow to use create_attribute etc.
      return
    }

    #my log "check attribute $column_name ot=$object_type, domain=$domain"
    if {[::xo::dc get_value check_att {select 0 from acs_attributes where 
      attribute_name = :column_name and object_type = :object_type} 1]} {
      
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
    ::xo::db::Attribute create object_id    -pretty_name "Object ID" -datatype integer
    #::xo::db::Attribute create object_type  -pretty_name "Object Type"
    ::xo::db::Attribute create object_title -pretty_name "Object Title" -column_name title
  }
  ::xo::db::Object db_slots
  ##############

  ##############
  # Handling temporary tables in PostgreSQL and Oracle via a common interface
  ##############
  
  ::xotcl::Class create ::xo::db::temp_table -parameter {name query vars}
  ::xo::db::temp_table instproc init {} {
    # The cleanup order is - at least under aolserver 4.01 - hard to get right.
    # When destroy_on_cleanup is executed, ther might be already some global
    # data for the database interaction gone.... So, destroy these objects 
    # by hand for now.
    # my destroy_on_cleanup
    
    # PRESERVE ROWS means that the data will be available until the end of the SQL session
    set sql_create "CREATE global temporary table [my name] on commit PRESERVE ROWS as "
    
    # When the table exists already, simply insert into it ...
    if {[::xo::db::require exists_table [my name]]} {
      ::xo::dc dml . "insert into [my name] ([my vars]) ([my query])"
    } else {
      # ... otherwise, create the table with the data in one step
      ::xo::dc dml get_n_most_recent_contributions $sql_create[my query]
    }
  }
  ::xo::db::temp_table instproc destroy {} {
    # A session spans multiple connections in OpenACS.
    # We want to get rid the data when we are done.
    ::xo::dc dml truncate_temp_table "truncate table [my name]"
    next
  }

  ##############
  ad_proc tcl_date {timestamp tz_var} {
    Convert the time stamp (coming from the database) into a format, which
    can be passed to Tcl's "clock scan".
  } {
    upvar $tz_var tz
    set tz 00
    # Oracle style format like 2008-08-25 (no TZ)
    if {![regexp {^([0-9]+-[0-9]+-[0-9]+)$} $timestamp _ timestamp]} {
      # PostgreSQL type ANSI format
      if {![regexp {^([^.]+)[.][0-9]*([+-][0-9]*)$} $timestamp _ timestamp tz]} {
        regexp {^([^.]+)([+-][0-9]*)$} $timestamp _ timestamp tz
      }
    }
    return $timestamp
  }
}


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
