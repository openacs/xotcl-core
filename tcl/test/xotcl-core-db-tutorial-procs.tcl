ad_library {
  Test the availability of xotcl
}


aa_register_case -cats {
  api smoke db
} -procs {
  "::xo::db::Class instproc new_persistent_object"
  "::xo::db::Class proc exists_in_db"
  "::xo::db::Class proc get_instance_from_db"

  "::xo::db::Object instproc delete"
  "::xo::db::Class proc object_type_to_class"
  "::xo::db::DB instproc transaction"
  "::xo::db::DB-postgresql instproc get_value"
  "::xo::db::Class proc get_object_type"
  "::throttle proc check"

} xotcl_core_tutorial_1 {
  Basic test cases based on the XOTcl core tutorial,
  test case set (1): create/fetch/delete/destroy objects
} {

  aa_run_with_teardown -rollback -test_code {
    #
    # 1) Create new ACS Objects, destroy it in memory,
    #    load it from the database, delete it in the database.
    #

    set o [::xo::db::Object new_persistent_object]
    aa_true "'$o' looks like a valid object name" [regexp {^::\d+$} $o]

    set id [$o object_id]
    aa_true "'$o' has a matching object_id" [regexp "^::$id\$" $o]
    aa_true "$o is an abject" [nsf::is object $o]

    $o destroy
    aa_false "$o is not an abject anymore" [nsf::is object $o]

    # Load the persisted object from the DB
    set o [::xo::db::Class get_instance_from_db -id $id]
    aa_true "$o is again an abject" [nsf::is object $o]

    # Check, of object exists in the DB
    aa_true "$o exists in the DB" [::xo::db::Class exists_in_db -id $id]

    # Delete the object in the DB
    $o delete
    aa_false "$o does not exist in the DB anymore" [::xo::db::Class exists_in_db -id $id]
  }
}

aa_register_case -cats {
  api smoke db
} -procs {
  "::xo::db::Class proc exists_in_db"
  "::xo::db::Class proc get_instance_from_db"
  "::xo::db::Class proc object_type_exists_in_db"

  " Class ::xo::db::Attribute"
  " Class ::xo::db::Class"
  "::xo::db::Class instproc new_persistent_object"
  "::xo::db::Class instproc create_object_type"
  "::xo::db::Object instproc save"
  "::acs::Cache instproc eval"
  "::xo::ConnectionContext instproc user_id"
  "::xo::Context instproc package_id"
  "::xo::db::Class proc class_to_object_type"
  "::xo::db::Class proc get_table_name"
  "::xo::db::Class proc object_type_to_class"
  "::xo::db::DB instproc sets"
  "::xo::db::DB instproc transaction"
  "::xo::db::DB-postgresql instproc dml"
  "::xo::db::DB-postgresql instproc generate_psql"
  "::xo::db::DB-postgresql instproc get_value"
  "::xo::db::DB-postgresql instproc list"
  "::xo::db::DB-postgresql instproc list_of_lists"
  "::xo::db::SQL instproc datatype_constraint"
  "::xo::db::postgresql instproc get_all_package_functions"
  "::xo::db::postgresql instproc map_datatype"
  "::xo::db::postgresql instproc select"
  "::xo::db::require proc default"
  "::xo::db::require proc exists_column"
  "::xo::db::require proc index"
  "::xo::db::require proc not_null"
  "::xo::db::require proc references"
  "::xo::db::require proc table"
  "::xo::db::require proc unique"
  "::xo::db::sql::acs_attribute proc create_attribute"
  "::xo::db::sql::acs_object proc delete"
  "::xo::db::sql::acs_object proc new"
  "::xo::db::sql::acs_object_type proc create_type"
  "::xo::db::sql::acs_object_type proc drop_type"
  "::xo::db::Class instproc check_table_atts"
  "::xo::db::Object instproc update"
  "::xo::db::Class proc get_object_type"
  "::throttle proc check"
} xotcl_core_tutorial_2 {
  Basic test cases based on the XOTcl core tutorial,
  test case set (2): Create new types from XOTcl objects
} {

  aa_run_with_teardown -rollback -test_code {

    ############################################################
    #
    # 2) Create new ACS Object Types, ACS Attributes and
    #    SQL Tables from XOTcl Classes with slot definitions.
    #
    # Create a new ACS Object type and an XOTcl class named ::demo::Person.

    aa_false "Does the ACS Object type ::demo::Person exist in the database" \
        [::xo::db::Class object_type_exists_in_db -object_type ::demo::Person]

    # We create a new XOTcl Class '::demo::Person'.
    # By defining this class, the database layer takes care
    # of creating the ACS Object Type and the necessary table via SQL.

    # The persistent attributes (stored in the database) are defined
    # as slots of type ::xo::db::Attribute.

    set cl [::xo::db::Class create ::demo::Person  -superclass ::xo::db::Object  -slots {
      ::xo::db::Attribute create name -column_name pname
      ::xo::db::Attribute create age -default 0 -datatype integer
      ::xo::db::Attribute create projects -default {} -multivalued true
    }]

    aa_equals "created class has name ::demo::Person" $cl "::demo::Person"
    aa_true "the object_type ::demo::Person exists" \
        [::xo::db::Class object_type_exists_in_db -object_type ::demo::Person]

    aa_equals "the SQL attributes are slot names" \
        [lsort [::demo::Person array names db_slot]] \
        {age name person_id projects}

    #
    # Create a new instance of ::demo::Person with name 'Gustaf'
    #
    # The method 'new_persistent_object' of a database class (instance of ::xo::db::Class)
    # creates an ACS Object with a fresh id in the database and
    # creates as well an XOTcl object in memory

    set p [::demo::Person new_persistent_object -name Gustaf -age 105]

    aa_true "'$p' looks like a valid object name" [regexp {^::\d+$} $p]

    aa_true "object $p exists in memory" [nsf::is object $p]
    set id [$p object_id]
    aa_true "bject $p exists in the db"  [::xo::db::Class exists_in_db -id $id]

    # modify some attributes of the XOTcl object
    set new_age [$p incr age]

    # save the modified object data in the database
    $p save

    # deleting XOTcl object $p in memory
    $p destroy

    aa_true "check, if object $p exists in the database" \
        [::xo::db::Class exists_in_db -id $id]

    # fetch person again from database:
    set p [::xo::db::Class get_instance_from_db -id $id]

    # get the age from the instance
    set age [$p age]
    aa_true "age equals the modified age" {$age eq $new_age}

    #
    # Now, we create a subclass of ::demo::Person called ::demo::Employee
    # which has a few more attributes. Again, we define an XOTcl class
    # ::demo::Employee which creates the ACS Object Type, the ACS
    # attributes and the table, if necessary.

    aa_false "Does the ACS Object type ::demo::Employee exist in the database" \
        [::xo::db::Class object_type_exists_in_db -object_type ::demo::Employee]

    set cl [::xo::db::Class create ::demo::Employee  \
                -superclass ::demo::Person  \
                -table_name demo_employee  \
                -id_column employee_id  \
                -slots {
                  ::xo::db::Attribute create salary -datatype integer
                  ::xo::db::Attribute create dept_nr -datatype integer -default "0"
                }]
    ::demo::Employee

    aa_equals "created class has name ::demo::Employee" $cl "::demo::Employee"
    aa_true "the object_type ::demo::Employee exists" \
        [::xo::db::Class object_type_exists_in_db -object_type ::demo::Employee]

    aa_equals "the SQL attributes are slot names" \
        [lsort [::demo::Employee array names db_slot]] \
        {dept_nr employee_id salary}
  }
}

aa_register_case -cats {
  api smoke db
} -procs {
  "::xo::db::Class proc get_class_from_db"

  "::xo::db::DB-postgresql instproc 1row"
  "::xo::db::DB-postgresql instproc list_of_lists"
  "::xo::db::Class instproc check_table_atts"
} xotcl_core_tutorial_3 {
  Basic test cases based on the XOTcl core tutorial,
  test case set (3): create classes from DB
} {

  aa_run_with_teardown -rollback -test_code {

    ############################################################
    # 3) Create XOTcl classes from existing ACS Object Types
    #    and ACS Attributes based on the definitions in the
    #    database

    set cl [::xo::db::Class get_class_from_db -object_type party]
    aa_equals "fetched class is named ::xo::db::party" "::xo::db::party" $cl

    # XOTcl class ::xo::db::party created (superclass ::xo::db::Object)
    # SQL attributes:
    aa_equals "the SQL attributes are slot names" \
        [lsort [$cl array names db_slot]] \
        {email party_id url}


    set cl [::xo::db::Class get_class_from_db -object_type person]
    aa_equals "fetched class is named ::xo::db::person" "::xo::db::person" $cl

    set attributes [lsort [$cl array names db_slot]]
    aa_equals "the SQL attributes $attributes are in slot names" \
        {1 1 1} \
        [lmap a {first_names last_name person_id} {
          expr {$a in $attributes}
        }]
  }
}


aa_register_case -cats {
  api smoke db
} -procs {
  "::xo::db::Class proc object_type_exists_in_db"
  "::xo::db::CrClass proc lookup"

  "::cr_check_mime_type"
  "::cr_create_content_file"
  "::xo::db::Class instproc check_table_atts"
  "::xo::db::CrClass instproc create_object_type"
  "::xo::db::CrClass instproc folder_type"
  "::xo::db::CrClass instproc get_instance_from_db"
  "::xo::db::CrItem instproc save"
  "::xo::db::CrItem instproc save_new"
  "::xo::db::DB instproc transaction"
  "::xo::db::DB-postgresql instproc dml"
  "::xo::db::DB-postgresql instproc get_value"
  "::xo::db::DB-postgresql instproc insert-view-operation"
  "::xo::db::DB-postgresql instproc row_lock"
  "::xo::db::postgresql instproc nextval"
  "::xo::db::sql::acs_object proc delete"
  "::xo::db::sql::content_item proc new"
  "::xo::db::sql::content_item proc set_live_revision"
  "::xo::db::sql::content_type proc create_type"

} xotcl_core_tutorial_4 {
  Basic test cases based on the XOTcl core tutorial,
  test case set (4): extending CR with application class
} {

  aa_run_with_teardown -rollback -test_code {
    ############################################################
    # 4) Create new application classes by sub-typing the
    # Content Repository, adding additional attributes
    #
    # We create a subclass of ::xo::db::CrItem called ::demo::Page
    # which has a few more attributes. Actually, this class is very
    # similar to ::xowiki::Page. Again, we define an XOTcl class
    # ::demo::Page which creates the ACS Object Type, the ACS
    # attributes and the table, if necessary.

    aa_false "Does the ACS Object type ::demo::Page exist in the database" \
        [::xo::db::Class object_type_exists_in_db -object_type ::demo::Page]

     set cl [::xo::db::CrClass create ::demo::Page  \
                -superclass ::xo::db::CrItem \
                -pretty_name "demo Page"  \
                -mime_type text/html \
                -slots {
                  ::xo::db::CrAttribute create creator
                }]
    aa_equals "created class is named ::demo::Page" "::demo::Page" $cl

    # Create a page object in memory.
    set i [::demo::Page new  \
               -name "page0" \
               -title "Joke of the Month"  \
               -creator "GN"  \
               -text "Three cannibals meet in a NYC subway station..."  ]

    # Save as a new item under default parent_id (-100), allocates fresh item_id
    set id [$i save_new]
    aa_true "the new id is larger than 10" {$id > 10}

    set item_id [$i item_id]
    aa_true "the returned id was the item_id" {$id == $item_id}

    set creator [$i creator]
    aa_true "the creator in the object is $creator" {$creator == "GN"}

    aa_log "i: <pre>[$i serialize]</pre>"

    # Destroy object in memory
    $i destroy

    # Fetch item per item_id from the database
    set o [::demo::Page get_instance_from_db -item_id $item_id]
    aa_true "the fetched object ($o) has the same item_id as before ($item_id)" {[$o item_id] eq $item_id}

    aa_log "o: <pre>[$o serialize]</pre>"
    set creator [$o creator]
    aa_true "the fetched creator is $creator" {$creator == "GN"}

    #
    # Lookup page from CR by name. In general, we do not know, of
    # which type a page with a certain name is, therefore, we use
    # ::xo::db::CrClass as interface.
    #
    set r [::xo::db::CrClass lookup -name page0]
    aa_true "lookup returned the item_id" {$r eq $item_id}

    # Modify the object.
    $o set title "Kilroy was here"

    # Save the object with a new revision.
    $o save
    $o destroy

    #
    # Fetch the object again from the DB and compare the title,
    # whether it is the new one.
    #
    set o [::demo::Page get_instance_from_db -item_id $item_id]
    aa_true "we fetched an object with the new title" {
      [$o title] eq "Kilroy was here"
    }
  }
}

# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
