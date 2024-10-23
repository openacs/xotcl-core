ad_library {
  XOTcl API for the API browser, defines the methods
  ad_proc (for object specific methods),
  ad_instproc (for traditional methods) and
  ad_doc (for documenting classes). Syntax for the methods
  ad_proc and ad_instproc is like oacs ad_proc, ad_doc
  receives one argument, similar to ad_library.

  @author Gustaf Neumann
  @creation-date 2005-05-13
  @cvs-id $Id$
}

::nx::Object create ::xo::api {
  #
  # General interface to obtain information from XOTcl/NX objects and
  # classes for the API browser.
  #

  array set :methodLabel {
    1-instproc "method"
    1-proc "object method"
    1-forward "object forward"
    0-instproc "instproc"
    0-proc "proc"
    0-forward "forward"
    0-Class "Class"
    0-Object "Object"
  }

  #
  # Support functions for the OpenACS API browser
  #
  :public object method method_label { -kind:switch proc_spec } {
    #
    # Return a user-friendly label for methods and objects
    #
    switch [llength $proc_spec] {
      1 {}
      3 {lassign $proc_spec obj methodType method; set scope ""}
      4 {lassign $proc_spec scope obj methodType method}
      default {
        ns_log notice "Unexpected format <$proc_spec> consists of [llength $proc_spec] parts"
      }
    }
    if {[info exists method]} {
      set isObject [:scope_eval $scope ::nsf::is object $obj]
      if {$isObject} {
        set isNx [:scope_eval $scope \
                      ::nsf::directdispatch $obj ::nsf::methods::object::info::hastype ::nx::Class]
        if {$kind} {
          set result [set :methodLabel($isNx-$methodType)]
        } else {
          set result "$obj [set :methodLabel($isNx-$methodType)] $method"
        }
        return $result
      }
    }
    return $proc_spec
  }

  :public object method debug_widget { proc_spec } {
    #
    # Return HTML code for a debug switch that lets an admin turn
    # debugging of functions and methods on and off. This
    # functionality is only allowed to site-wide admins.
    #
    if {![acs_user::site_wide_admin_p]
        || [info commands ::nsf::method::property] eq ""
        || $::nsf::version < 2.1
      } {
      return ""
    }
    switch [llength $proc_spec] {
      1 {lassign [list "" ::nx::Object nsfproc $proc_spec] scope obj methodType method
        if {![string match ::* $method]} {
          set method ::$method
        }
        #
        # In case $proc_spec is a cmd, it has to be a nsfproc
        #
        if {[nsf::cmd::info type $method] ne "nsfproc"} {
          return ""
        }
      }
      3 {lassign $proc_spec obj methodType method; set scope ""}
      4 {lassign $proc_spec scope obj methodType method}
      default {
        ns_log notice "[self] debug_widget: Unexpected format <$proc_spec> consists of [llength $proc_spec] parts"
        return ""
      }
    }
    if {$methodType eq "proc"} {
      set modifier "-per-object"
    } elseif {$methodType in {instproc nsfproc}} {
      set modifier ""
    } elseif {$methodType eq "Class"} {
      return ""
    } else {
      ns_log warning "[self] debug_widget unexpected method type <$methodType>"
      set modifier ""
    }
    set isObject [:scope_eval $scope ::nsf::is object $obj]
    if {!$isObject} {
      return ""
    }
    set debug_p [:scope_eval $scope ::nsf::method::property $obj {*}$modifier $method debug]

    #
    # Increment global form_id
    #
    set form_id "form-[incr ::__form_id]"

    #
    # Add the JavaScript function only once, which will toggle the
    # debug state in the background (template::add_script would add
    # it multiple times).
    #
    if {$::__form_id eq "1"} {

      template::add_body_script -script {
        function ajax_submit(form) {
          var xhr = new XMLHttpRequest();
          xhr.open('POST', '/xotcl/admin/toggle-debug', true);
          xhr.onreadystatechange = function() {
            if (this.readyState == 4) {
              if (this.status != 200) {
                alert('AJAX submit unexpected response: ' + this.status);
              }
            }
          }
          xhr.send(new FormData(form));
        };
      }
    }

    #
    # Add the required js and CSS. We use here bootstrap + titatoggle,
    # and assume, we have bootstrap3 installed
    #
    #template::head::add_css -href urn:ad:css:bootstrap3
    #template::head::add_javascript -src urn:ad:js:bootstrap3

    template::head::add_css -href "/resources/xotcl-core/titatoggle/titatoggle-dist.css"
    #
    # Return an HTML snippet with a form and the computed form-ID
    #
    if {$debug_p} {set state checked} {set state ""}
    set html [subst {
      <form id="$form_id" class="form" method="POST" action="/xotcl/admin/toggle-debug">
      <div class="checkbox checkbox-slider--b-flat">
      <label class="checkbox-inline">
      <input class="debug form-control" id="$form_id-control" name="debug" type="checkbox" $state><span>Debug</span>
      <input name="proc_spec" type="hidden" value="$proc_spec">
      <input name="return_url" type="hidden" value="[ns_quotehtml [ad_return_url]]">
      </label>
      </div>
      </form>
    }]

    template::add_body_script -script [subst {
      document.getElementById('$form_id-control').addEventListener('click', function (event) {
        ajax_submit(this.form);
      });
    }]

    return $html
  }

  :public object method method_link {obj kind method} {
    #
    # Return a link for the method if possible. If no proc-doc is
    # available, return just plain text.
    #
    set kind [string trimright $kind s]
    set proc_index [::xo::api proc_index "" $obj $kind $method]
    if {[nsv_exists api_proc_doc $proc_index]} {
      return "<a href='/api-doc/proc-view?proc=[ns_urlencode $proc_index]'>$method</a>"
    } else {
      if {[::xo::getObjectProperty $obj $kind $method] eq ""} {
        return $method<SUP>C</SUP>
      } else {
        return $method
      }
    }
  }

  :public object method scope_eval {scope args} {
    #
    # When the scope is not empty, evaluate the command in the
    # specified scope (thread)
    #
    if {$scope eq ""} {
      {*}$args
    } else {
      $scope do {*}$args
    }
  }

  :public object method isclass {scope obj} {
    #
    # Check, whether the passed in obj is a class
    #
    :scope_eval $scope xo::getObjectProperty $obj isclass
  }

  :public object method isobject {scope obj} {
    #
    # Check, whether the passed in obj is an object
    #
    :scope_eval $scope xo::getObjectProperty $obj isobject
  }

  :public object method scope {} {
    #
    # Return the scope of the object. When executed in an XOTcl
    # thread; the body won't be accessible by default without the
    # explicit scope.
    #
    # The purpose of this proc is to document objects and classes that
    # live only in a certain thread.
    #
    if {[info exists ::xotcl::currentThread]} {
      #
      # We are in an XOTcl thread
      #
      return $::xotcl::currentThread
    }
    return ""
  }

  :public object method scope_from_object_reference {scope_var object_var} {
    #
    # Parse the object reference and return the scope from it.
    #
    upvar $scope_var scope $object_var object
    set scope ""
    regexp {^(.+) do (.+)$} $object match scope object
  }

  :public object method scope_from_proc_index {proc_index} {
    #
    # Parse the proc_index and return the scope from it.
    #
    set scope ""
    regexp {^(.+) .+ (inst)?proc (.+)$} $proc_index match scope
    return $scope
  }

  :public object method object_from_proc_index {proc_index} {
    #
    # Parse the proc_index and return the scope from it.
    #
    set object ""
    if {[regexp { *([^ ].+) (inst)?proc (.+)$} $proc_index . object]
        || [regexp { (Class|Object) (.+)$} $proc_index . what object]
      } {
    }
    return $object
  }
  
  :public object method script_name {-obj scope} {
    #
    # Determine name of the current "script" as displayed by "Defined
    # in" in the API browser. Define different sources available in
    # different situatons.
    #
    # @param  obj class name for identifying the source filename
    # @param scope either empty or thread name
    # @return path starting with the "packages" directory
    #
    set script [info script]
    if {$script eq "" || [file tail $script] eq "procdoc-init.tcl"} {
      set script ""
      if {$script eq "" && [info exists obj] && [nsv_get proc_source_file " Class $obj" script]} {
        #ns_log notice "INIT script_name of $obj from proc_source_file => <$script>"
      }
      if {$script eq "" && [info exists obj]} {
        set class [$obj info class]
        if {[nsv_get proc_source_file " Class $class" script]} {
          #ns_log notice "INIT script_name of $obj via $class from proc_source_file => <$script>"
        }
      }
      if {$script eq "" && [info exists ::xotcl::currentScript]} {
        set script $::xotcl::currentScript
      }
      set root_dir $::acs::rootdir
      set root_length [string length $root_dir]
      if { $root_dir eq [string range $script 0 $root_length-1]} {
        set script [string range $script $root_length+1 end]
      }
    }
    return $script
  }

  :public object method object_link {{-noimg:boolean off} scope obj} {
    #
    # Return a link for the object.
    #
    set link "<a href='[ns_quotehtml [:object_url $scope $obj]]'>"
    if {$noimg} {
      return "$link$obj</a>"
    } else {
      return "$obj$link<img src='/resources/acs-subsite/ZoomIn16.gif' alt='\[i\]' border='0'></a>"
    }
  }

  :public object method object_url {{-show_source 0} {-show_methods 1} scope obj} {
    #
    # Return a link for the object in the object browser (show-object)
    #
    set isObject [:scope_eval $scope ::nsf::is object $obj]
    if {$isObject} {
      set object [:scope_eval $scope namespace origin $obj]
      return [export_vars -base /xotcl/show-object {object show_source show_methods}]
    } else {
      return .
    }
  }

  :public object method object_index {scope obj} {
    #
    # Return a canonical index string for the object
    #
    set kind [expr {[:isclass $scope $obj] ? "Class" : "Object"}]
    return "$scope $kind $obj"
  }

  :public object method proc_index {scope obj instproc proc_name} {
    #
    # Return a canonical index string for the specified method
    #
    if {$scope eq ""} {
      return [list [string trimleft $obj :] $instproc $proc_name]
    } else {
      return [list $scope $obj $instproc $proc_name]
    }
  }

  :public object method source_to_html {{-width 100} string} {
    #
    # Helper proc to preserve indentation in source-code to HTML
    # conversion.
    #
    set lines [list]
    foreach l [split $string \n] {
      while {[string length $l] > $width} {
        set pos [string last " \{" $l $width]
        if {$pos>10} {
          lappend lines "[string range $l 0 $pos-1] \\"
          set l "      [string range $l $pos end]"
        } else {
          # search for a match right of the target
          set pos [string first " \{" $l $width]
          if {$pos > 10} {
            lappend lines "[string range $l 0 $pos-1] \\"
            set l "      [string range $l $pos end]"
          } else {
            # last resort try to split around spaces
            set pos [string last " " $l $width]
            if {$pos > 10} {
              lappend lines "[string range $l 0 $pos-1] \\"
              set l "      [string range $l $pos end]"
            } else {
              break
            }
          }
        }
      }
      lappend lines $l
    }
    set string [join $lines \n]
    set html [ns_quotehtml $string]
    regsub -all -- {(\n[\t ]*)(\#[^\n]*)} $html \\1<it>\\2</it> html
    return "<pre class='code'>$html</pre>"
  }

  :public object method get_doc_block {text {restVar ""}} {
    #
    # Get the (first) documentation block of the provided text (which
    # might be e.g. the body of a method).
    #
    set lines [split $text \n]
    set docBlock ""
    set i 0
    set nrLines [llength $lines]
    while {[string is space [lindex $lines $i]] && $i < $nrLines} {incr i}
    while {$i < $nrLines} {
      set line [string trim [lindex $lines $i]]
      incr i
      if {[string index $line 0] ne "#"} break
      append docBlock [string range $line 1 end] \n
    }
    if {$restVar ne ""} {
      upvar $restVar rest
      set rest [join [lrange $lines $i-1 end] \n]
    }
    #ns_log notice "=================== get_doc_block RETURNS <$docBlock>"
    return $docBlock
  }

  :public object method update_object_doc {scope obj doc_string} {
    #
    # Update the API browser nsvs with information about the provided
    # object.
    #
    # If no doc string is provided, try to get it from the object
    # definition.
    #

    if {$doc_string eq ""} {
      set doc_string [:get_doc_block [:get_init_block $scope $obj]]
    }

    ad_parse_documentation_string $doc_string doc_elements
    #
    # Initialize dictionary with default values and update it with the
    # information from parsing the doc string.
    #
    set doc [dict create \
                 param "" \
                 protection public \
                 varargs_p false \
                 deprecated_p false \
                 warn_p false \
                 script [::xo::api script_name -obj $obj $scope] \
                ]
    set doc [dict replace $doc {*}[array get doc_elements]]

    #
    # TODO: add actual parameters to flags and defaults (also required, ...)
    #
    set switches {}; set flags {}
    foreach l [dict get $doc param] {
      if {[regexp {^([^ ]+)\s} $l . word]} {
        lappend switches $word
        lappend flags $word ""
      }
    }
    set proc_index [::xo::api object_index $scope $obj]
    set doc [dict replace $doc \
                 default_values "" \
                 switches0 $switches \
                 switches1 "" \
                 positionals "" \
                 flags $flags \
                ]
    #ns_log notice "proc_index <$proc_index> -> $doc"
    if {![nsv_exists api_proc_doc $proc_index]} {
      nsv_lappend api_proc_doc_scripts [dict get $doc script] $proc_index
    }
    nsv_set api_proc_doc $proc_index $doc
    nsv_set api_library_doc $proc_index $doc

    set file_index [dict get $doc script]
    if {[nsv_exists api_library_doc $file_index]} {
      array set elements [nsv_get api_library_doc $file_index]
    }
    set oldDoc [expr {[info exists elements(main)] ? \
                          [lindex $elements(main) 0] : ""}]
    set prefix "This file defines the following Objects and Classes"
    set entry [::xo::api object_link $scope $obj]
    if {![string match "*$prefix*" $oldDoc]} {
      append oldDoc "<p>$prefix: $entry"
    } else {
      append oldDoc ", $entry"
    }
    set elements(main) [list $oldDoc]
    #ns_log notice "elements = [array get elements]"
    nsv_set api_library_doc $file_index [array get elements]

    if {[::nsf::dispatch $obj ::nsf::methods::object::info::hastype ::nx::Class]} {
      #
      # nx classes
      #
      foreach protection {public protected private} {
        foreach m [::nsf::dispatch $obj ::nsf::methods::class::info::methods \
                       -path -callprotection $protection -type scripted] {
          set docBlock [:get_doc_block \
                            [::nsf::dispatch $obj ::nsf::methods::class::info::method body $m]]
          ::xo::api update_method_doc \
              -protection $protection \
              -deprecated=false \
              -debug=false \
              $scope $obj \
              inst $m $docBlock
        }
      }
    }
    if {[::nsf::dispatch $obj ::nsf::methods::object::info::hastype ::nx::Object]} {
      #
      # nx objects
      #
      foreach protection {public protected private} {
        foreach m [::nsf::dispatch $obj ::nsf::methods::object::info::methods \
                       -callprotection $protection -type scripted] {
          set docBlock [:get_doc_block \
                            [::nsf::dispatch $obj ::nsf::methods::object::info::method body $m]]
          ::xo::api update_method_doc \
              -protection $protection \
              -deprecated=false \
              -debug=false \
              $scope $obj \
              "" $m $docBlock
        }
      }
    }

  }

  :public object method update_method_doc {
    {-protection "public"}
    {-deprecated:switch false}
    {-debug:switch false}
    {-warn:switch false}
    scope obj inst proc_name
    docString
  } {
    #
    # Obtain a doc-string for a method, convert it and add it to the
    # proc-doc.
    #
    set methodType [::xo::getObjectProperty $obj ${inst}methodtype $proc_name]
    set varargs_p [expr {$methodType eq "scripted"
                         && "args" in [::xo::getObjectProperty $obj ${inst}args $proc_name]}]

    set doc [dict create \
                 param "" \
                 protection $protection \
                 varargs_p $varargs_p \
                 deprecated_p $deprecated \
                 warn_p false \
                 script [::xo::api script_name -obj $obj $scope] \
                 main "" \
                 flags "" \
                 switches0 "" \
                 switches1 "" \
                ]

    if {$docString ne ""} {
      ad_parse_documentation_string $docString doc_elements
      set doc [dict replace $doc {*}[array get doc_elements]]
    }

    if {$methodType ne "scripted"} {
      dict set doc default_values {}
      dict set doc positionals {}
    } else {
      set defaults [list]
      foreach a [::xo::getObjectProperty $obj ${inst}args $proc_name] {
        if {[::xo::getObjectProperty $obj ${inst}argdefault $proc_name $a d]} {
          lappend defaults $a $d
        }
      }

      foreach def [::xo::getObjectProperty $obj ${inst}methodparameter $proc_name] {
        lassign $def f default
        set pair [split [lindex $f 0 0] :]
        lassign $pair flaggedName flags
        if {[string index $flaggedName 0] eq "-"} {
          set isFlag 1
          set name [string range $flaggedName 1 end]
        } else {
          set isFlag 0
          set name $flaggedName
        }
        if {$isFlag} {
          dict lappend doc switches0 $name
          dict lappend doc flags $name [split $flags ,]
          #:log "default_value $proc_name: $sw -> '[lindex $f 1]' <$pair/$f>"
          if {$flags eq "switch" && $default eq ""} {
            set default "false"
          }
        } else {
          dict lappend doc flags $name [split $flags ,]
        }
        #:log "default_value $proc_name: $sw -> 'default' <$pair/$f>"
        if {[llength $def] > 1} {
          lappend defaults $name $default
        }
      }
      dict set doc default_values $defaults
      dict set doc positionals [::xo::getObjectProperty $obj ${inst}args $proc_name]
    }

    # argument documentation finished
    set proc_index [::xo::api proc_index $scope $obj ${inst}proc $proc_name]
    if {![nsv_exists api_proc_doc $proc_index]} {
      nsv_lappend api_proc_doc_scripts [dict get $doc script] $proc_index
    }
    #ns_log notice "SETTING api_proc_doc '$proc_index' <$doc>"
    nsv_set api_proc_doc $proc_index $doc
  }


  :public object method get_init_block {scope obj} {
    #
    # Get the init block of an object/class or return empty
    #
    if {[:scope_eval $scope ::nsf::var::exists $obj __cmd(__initblock)]} {
      return [:scope_eval $scope ::nsf::var::set $obj __cmd(__initblock)]
    }
    return ""
  }

  :public object method get_object_source {scope obj} {
    #
    # Return the full object definition
    #
    if {![nsf::is object $obj]} {
      ns_log warning "[self] get_object_source: argument passed as obj is not an object: $obj"
      return ""
    }
    set init_block [:get_init_block $scope $obj]
    if {$init_block ne ""} {
      set dummy [:get_doc_block $init_block body]
      return $body
    } else {
      return [:scope_eval $scope $obj serialize]
    }
  }

  :public object method get_method_source {scope obj prefix method} {
    #
    # Return the full method definition.
    #
    :scope_eval $scope ::Serializer methodSerialize $obj $method $prefix
  }

  :public object method get_method_body {scope obj prefix method} {
    #
    # Return the method body on object (when "prefix" is empty) or
    # class (when "prefix" is "inst").
    #
    :scope_eval $scope ::nsf::dispatch $obj \
        ::nsf::methods::[expr {$prefix eq "inst" ? "class" : "object"}]::info::method \
        body $method
  }

  :public object method update_nx_docs {{objects ""}} {
    #
    # Update for the provided (or all) nx::Object instances the
    # internal documentation structures.
    #
    if {[llength $objects] == 0} {
      set objects [nx::Object info instances -closure]
    }

    foreach o $objects {
      #
      # Check general per-object documentation.
      #
      if {[string match ::nx::* $o]} continue
      ::xo::api update_object_doc "" $o ""
    }
  }

  :public object method get_proc_definition_flags {debug deprecated} {
    #
    # Helper for version compatibility
    #
    # @return flags for proc definition
    #
    if {$::nsf::version < 2.1} {
      return ""
    }
    return [list -debug=$debug -deprecated=$deprecated]
  }

  :public object method get_returns_spec {returns} {
    #
    # Helper for version compatibility
    #
    # @return flags for -returns flag
    #
    if {$::nsf::version < 2.1} {
      set result ""
    } elseif {$returns ne ""} {
      set result [list -returns $returns]
    } else {
      set result ""
    }
    return $result
  }
}

# ---------------------------------------------------------------------
#
# Per default, the content of the "::xotcl" and "::nx" namespace is
# not serialized; so when extending the base classes of the object
# system, one has to these methods explizitely to the export list for
# the serializer.
#
# The following extensions of the base classes are defined here:
#
::Serializer exportMethods {
  ::xotcl::Object instproc ad_proc
  ::xotcl::Object instproc ad_forward
  ::xotcl::Class  instproc ad_instproc
  ::xotcl::Class  instproc ad_instforward
  ::xotcl::Object instproc ad_doc
}

::xotcl::Object instproc ad_proc {
  {-private:switch false}
  {-deprecated:switch false}
  {-warn:switch false}
  {-debug:switch false}
  proc_name
  arguments:parameter,0..*
  {-returns ""}
  doc
  body
} {
  set flags [::xo::api get_proc_definition_flags $debug $deprecated]
  set returnSpec [::xo::api get_returns_spec $returns]
  uplevel [list [self] proc {*}$flags $proc_name $arguments {*}$returnSpec $body]
  ::xo::api update_method_doc \
      -protection [expr {$private ? "private" : "public"}] \
      -deprecated=$deprecated \
      -debug=$debug \
      [::xo::api scope] [self] \
      "" $proc_name $doc
}

::xotcl::Class instproc ad_instproc {
  {-private:switch false}
  {-deprecated:switch false}
  {-warn:switch false}
  {-debug:switch false}
  proc_name
  arguments:parameter,0..*
  {-returns ""}
  doc
  body
} {
  set flags [::xo::api get_proc_definition_flags $debug $deprecated]
  set returnSpec [::xo::api get_returns_spec $returns]
  uplevel [list [self] instproc {*}$flags $proc_name $arguments {*}$returnSpec $body]
  ::xo::api update_method_doc \
      -protection [expr {$private ? "private" : "public"}] \
      -deprecated=$deprecated \
      -debug=$debug \
      [::xo::api scope] [self] \
      inst $proc_name $doc
}

::xotcl::Object instproc ad_forward {
  {-private:switch false}
  {-deprecated:switch false}
  {-warn:switch false}
  {-debug:switch false}
  method_name
  doc
  args
} {
    set flags [::xo::api get_proc_definition_flags $debug $deprecated]
    uplevel [self] forward {*}$flags $method_name $args
    ::xo::api update_method_doc \
        -protection [expr {$private ? "private" : "public"}] \
        -deprecated=$deprecated \
        -debug=$debug \
        [::xo::api scope] [self] \
        "" $method_name $doc
  }

::xotcl::Class instproc ad_instforward {
  {-private:switch false}
  {-deprecated:switch false}
  {-warn:switch false}
  {-debug:switch false}
  method_name
  doc
  args
} {
    set flags [::xo::api get_proc_definition_flags $debug $deprecated]
    uplevel [self] instforward {*}$flags $method_name $args
    ::xo::api update_method_doc \
      -protection [expr {$private ? "private" : "public"}] \
      -deprecated=$deprecated \
      -debug=$debug \
      [::xo::api scope] [self] \
      "inst" $method_name $doc
  }

::xotcl::Object instproc ad_doc {doc_string} {
  ::xo::api update_object_doc [::xo::api scope] [self] $doc_string
}

# Class ::Test -ad_doc {
#   Test Class for the documentation of
#   <code>Classes</code>,
#   <code>Objects</code>,
#   <code>instprocs</code>, and
#   <code>procs</code>.
#   @author Gustaf Neumann
#   @cvs-id $Id$
# }
# ::Test ad_proc my-class-specific-proc {x y} {
#   This is a proc of Class Test merely for testing purposes...
#   @param x First Operand
#   @param y Second Operand
# } {
#   ns_log notice "hello world $x $y"
# }

# ::Test ad_instproc my-method {-id:required} {
#   This is an instproc of Class Test merely for testing purposes...
#   @param id Some Id
# } {
#   ns_log notice "hello world $id"
# }
# ::Test ad_instproc my-method2 {-id:required {-flag:boolean true}} {
#   This is an instproc of Class Test merely for testing purposes...
#   @param id Some Id
#   @param flag Some flag
# } {
#   ns_log notice "hello world $id"
# }
# ::Test ad_instproc -private my-method3 {-id:required {-flag:boolean true} -switch:switch x {y 1}} {
#   This is an instproc of Class Test merely for testing purposes...
#   @param id Some Id
#   @param flag Some flag
#   @param switch Switch to turn on or off depending on default
#   @param x First Operand
#   @param y Second Operand
# } {
#   ns_log notice "hello world $id"
# }

# Class create ::SpecializedTest -superclass ::Test -ad_doc {
#   A Class defined as a subclass of ::Test for testing the
#   documentation stuff...
# }

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
