namespace eval ::xo::db {

  ::xotcl::Object require
  require proc table {name definition} {
    if {![db_0or1row check-$name \
              "select 1 from pg_tables where tablename = '$name'"]} {
      db_dml create-$name "create table $name($definition)"
    }
  }

  require proc view {name definition} {
    if {![db_0or1row check-$name \
              "select 1 from pg_views where viewname = '$name'"]} {
      db_dml create-$name "create view $name AS $definition"
    }
  }

  require proc index {-table -col {-using ""} {-unique false}} {
    set colpart $col
    regsub -all ", *" $colpart _ colpart
    set suffix [expr {$unique ? "un_idx" : "idx"}]
    set uniquepart [expr {$unique ? "UNIQUE" : ""}]
    set name ${table}_${colpart}_$suffix
    if {![db_0or1row check_${name} \
          "select 1 from pg_indexes where indexname = '$name'"]} {
      set using [expr {$using ne "" ? "using $using" : ""}]
      db_dml create-$name \
          "create $uniquepart index $name ON $table $using ($col)"
    }
  }

  proc has_ltree {} {
    ns_cache eval xotcl_object_cache ::xo::has_ltree {
      if {[catch {db_1row check_ltree "select * from pg_proc where proname = 'ltree_in'"}]} {
        return 0
      }
      return 1
    }
  }

}

