::xo::library doc {
  XOTcl HTML Widget Classes based on tDOM

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @author Neophytos Demetriou (k2pts@phigita.net)
  @creation-date 2005-11-26
  @cvs-id $Id$
}

::Serializer exportMethods {
  ::xotcl::Object instproc asHTML
}

Object instproc asHTML {{-master defaultMaster} -page:switch} {
  ::xo::require_html_procs
  dom createDocument html doc
  set root [$doc documentElement]
  if {!$page} {
    $root appendFromScript {:render}
    set n [$root childNode]
    if {$n eq ""} {
      return ""
    }
    return [$n asHTML]
  } else {
    set slave [$master decorate $root]
    $slave appendFromScript {:render}
    ns_return 200 text/html [$root asHTML]
  }
}


#
# Define Widget classes with localization
#
# Most importantly, we define ::xo::Table, somewhat similar to the classical multirow

namespace eval ::xo {}
namespace eval ::xo::tdom {

  ::xotcl::Class create ::xo::tdom::Class \
      -superclass ::xotcl::Class \
      -parameter {autoimport}

  ::xo::tdom::Class instproc incr_level {{amount 1}} {
    #
    # Keep the nesting level of TdomClass instances during creation.
    # Use a global variable to assure cleanup in case of exceptions.
    #
    set var __tdom_level
    global $var
    if {[info exists $var]} {
      incr $var $amount
    } else {
      set $var 1
    }
  }

  ::xo::tdom::Class instproc unknown args {
    set configurecmds [lrange $args 0 end-1]
    set createcmd [lindex $args end]
    #
    # Keep a stack of nesting levels of ::xo::tdom Objects.
    # The stack is used for building automatically an ordered
    # composite of objects, used e.g. in recursive renderings.
    #
    [self class] instvar stack
    set level [:incr_level]

    #
    # Create a new instance of the current class and configure it.
    #
    #:log "tdom START $level [self], cmd='$configurecmds'"
    set me [:new -destroy_on_cleanup {*}$configurecmds]
    #:log "tdom CREATED $level $me ([$me info class])"

    #
    # If we are not on the topmost level, add the created object
    # to the parent ordered composite.
    #
    set stack($level) $me
    if {$level > 1} {
      set parent $stack([expr {$level - 1}])
      #:log "tdom ADD  $level $me to $parent ([$parent info class])"
      $parent add $me
    }

    #
    # search for autoimports: all commands are executed in the ... currently not needed
    #
    #     set class [$me info class]
    #     foreach cl [concat $class [$class info heritage]] {
    #       :log "tdom EVAL $level ns=[namespace current] autoimport in $cl?[$cl exists autoimport]"
    #       if {[$cl exists autoimport]} {
    #         :log "tdom IMPO [$cl autoimport] into $me"
    #         namespace eval ::xo::tmp [list namespace import -force [$cl autoimport]]
    #       }
    #     }
    #    #:log "tdom CMDS $level [lsort [info commands ::xo::tmp::*]]"

    if {$createcmd ne ""} {
      #
      # perform the subcommand on the caller level to expand (like in tdom)
      # all specified variables in the caller's context
      #
      uplevel $createcmd
    }

    #
    # autorendering means that after creating an ordered composite,
    # the topmost element is automatically rendered. This makes
    # the ::xo::tdom classes behave more like plain tDOM commands.
    #
    #:log "tdom AUTO $level [$me autorender]"

    if {$level == 1 && [$me autorender]} {
      #:log "tdom RNDR $level $me render"
      $me render
    }

    #:log "tdom END  $level [self] me=$me"
    set level [:incr_level -1]
    return $me
  }

  #
  # The tDOM attribute manager makes it syntactically easier to
  # specify a list of attributes for rendering via tDOM.
  #
  ::xotcl::Class create ::xo::tdom::AttributeManager
  ::xo::tdom::AttributeManager ad_instproc get_attributes {
    args
  } {
    Get a list of attribute value pairs
    of instance attributes. It returns only those
    pairs for which a value exists.

    @return flattened list of attribute value pairs
  } {
    set pairs [list]
    foreach attribute $args {
      set l [split $attribute]
      if {[llength $l] > 1} {
        lassign $l attribute HTMLattribute
      } else {
        set HTMLattribute $attribute
      }
      #:msg "${:name} check for $attribute => [info exists :$attribute]"
      if {[info exists :$attribute]} {
        lappend pairs $HTMLattribute [set :$attribute]
      }
    }
    return $pairs
  }
  ::xo::tdom::AttributeManager ad_instproc get_local_attributes {
    args
  } {
    Get a list of attribute value pairs
    of instance attributes. It returns only those
    pairs for which a value exists.

    @return flattened list of attribute value pairs
  } {
    set pairs [list]
    foreach attribute $args {
      set l [split $attribute]
      if {[llength $l] > 1} {
        lassign $l attribute HTMLattribute
      } else {
        set HTMLattribute $attribute
      }
      #:msg "${:name} check for $attribute => [info exists :$attribute]"
      if {[:uplevel [list info exists $attribute]]} {
        lappend pairs $HTMLattribute [:uplevel [list set $attribute]]
      }
    }
    return $pairs
  }

  #
  # ::xo::tdom::Object
  # is the top of the class hierarchies for tDOM objects
  #
  ::xotcl::Class create ::xo::tdom::Object \
      -superclass {::xo::tdom::AttributeManager ::xo::OrderedComposite} \
      -parameter {{autorender true}}

  ::xo::tdom::Object instproc render {} {
    foreach o [:children] { $o render }
  }

  #
  # General of HTML markup CSRF tokens in tDOM contexts
  #
  namespace eval ::html {}
  proc ::html::CSRFToken {} {
    ::if {[::info exists ::__csrf_token]} {
      ::html::input -type hidden -name __csrf_token -value $::__csrf_token {}
    }
  }

}




namespace eval ::xo {
  #
  # Escape provided char in provided string with backslash
  #
  proc backslash_escape {char string} {
    return [string map [list $char \\$char] $string]
  }

  #
  # Localization
  #

  #
  # The following pair of functions implement a crude method for
  # avoiding i16n substitutions. These are necessary, since xowiki
  # provides all its markup finally as "content" that is currently
  # internationalized without distinctions. However, sometimes
  # (e.g. values in forms) should be presented without i18n
  # processing. In such cases, the two functions below can be used to
  # prevent such substitutions.
  #
  proc remove_escapes {text} {
    regsub -all \x01# $text "#" text
    return $text
  }

  proc escape_message_keys {text} {
    regsub -all {(\#[a-zA-Z0-9_:-]+\.[a-zA-Z0-9_:-]+)\#} $text "\\1\x01#" text
    return $text
  }

  #
  # xo::localize function
  #

  set ::xo::acs_lang_url [apm_package_url_from_key acs-lang]admin

  proc localize {text {inline 0}} {
    #ns_log notice "--local $text $inline"
    set obj [uplevel self]
    if {![$obj exists __localizer]} {
      $obj set __localizer [list]
    }
    if {[string first \x02 $text] == -1} {
      return $text
    } else {
      set return_text ""
      if {$inline} {
        # Attempt to move all message keys outside of tags
        while { [regsub -all {(<[^>]*)(\x02\(\x01[^\x01]*\x01\)\x02)([^>]*>)} $text {\2\1\3} text] } {}

        # Attempt to move all message keys outside of <select>...</select> statements
        regsub -all -nocase {(<option\s[^>]*>[^<]*)(\x02\(\x01[^\x01]*\x01\)\x02)([^<]*</option[^>]*>)} $text {\2\1\3} text

        while { [regsub -all -nocase {(<select[^>]*>[^<]*)(\x02\(\x01[^\x01]*\x01\)\x02)} $text {\2\1} text] } {}
      }

      while {[regexp {^([^\x02]*)\x02\(\x01([^\x01]*)\x01\)\x02(.*)$} $text _ \
                  before key text]} {
        append return_text $before
        lassign [split $key .] package_key message_key
        set url [export_vars -base $::xo::acs_lang_url/edit-localized-message {
          {locale {[ad_conn locale]} }
          package_key message_key
          {return_url [ad_return_url]}
        }]
        if {[lang::message::message_exists_p [ad_conn locale] $key]} {
          set type localized
        } elseif { [lang::message::message_exists_p "en_US" $key] } {
          set type us_only
        } else { # message key is missing
          set url [export_vars -base $::xo::acs_lang_url/localized-message-new {
            {locale en_US } package_key message_key
            {return_url [ad_return_url]}
          }]
          set type missing
        }
        if {!$inline} {
          $obj lappend __localizer [::xo::Localizer new -type $type -key $key -url $url]
        } else {
          set l [::xo::Localizer new -type $type -key $key -url $url]
          append return_text [$l asHTML]
        }
      }
      append return_text $text
      return $return_text
    }
  }

  proc render_localizer {} {
    set obj [uplevel self]
    if {[$obj exists __localizer]} {
      foreach l [$obj set __localizer] {
        $l render
        $l destroy
      }
    }
  }

  Class create Localizer -parameter {type key url}

  #Localizer instproc render {} {
  #  html::a -title [:key] -href [:url] {
  #    switch -- [:type] {
  #      localized {set char o; set style "color: green"}
  #      us_only   {set char *; set style "background-color: yellow; color: red;"}
  #      missing   {set char @; set style "background-color: red; color: white;"}
  #    }
  #    html::span -style $style {html::t $char}
  #  }
  #}
  #Localizer instproc render {} {
  #  html::a -title [:key] -href [:url] {
  #    set path /resources/acs-templating/xinha-nightly/plugins/
  #    switch -- [:type] {
  #      localized {set img ImageManager/img/btn_ok.gif}
  #      us_only  {set img Filter/img/ed_filter.gif}
  #      missing  {set img LangMarks/img/en.gif}
  #    }
  #    html::img -alt [:type] -src $path/$img -width 16 -height 16 -border 0
  #  }
  #}
  Localizer instproc render {} {
    switch -- ${:type} {
      localized {set img ImageManager/img/btn_ok.gif}
      us_only  {set img Filter/img/ed_filter.gif}
      missing  {set img LangMarks/img/en.gif}
    }
    html::a -class "acs-lang-${:type}" -title ${:key} -href ${:url} {}
  }

  ## todo : make these checks only in trn mode (additional mixin)

  Class create Drawable \
      -superclass ::xo::tdom::AttributeManager \
      -instproc _ {attr} {
        set :$attr
      } \
      -instproc render_localizer {} {
      }

  Class create TRN-Mode \
      -instproc _ {attr} {
        return [::xo::localize [set :$attr]]
      } \
      -instproc render_localizer {} {
        #:log "-- "
        if {[info exists :__localizer]} {
          foreach l ${:__localizer} {
            $l render
            $l destroy
          }
        }
        set :__localizer [list]
      } \
      -instproc render-data args {
        next
        :render_localizer
      } \
      -instproc render args {
        next
        :render_localizer
      }

  #
  # for the time being, just a proc
  #
  proc get_user_name {uid} {
    set name [expr {[string is integer -strict $uid]
                    ? [person::name -person_id $uid]
                    : ""}]
    if {$name eq ""} {
      set name [_ xotcl-core.nobody]
    }
    return $name
  }
}

namespace eval ::xo {
  #
  # Define an abstract ::xo::Table
  #
  Class create ::xo::Table -superclass OrderedComposite \
      -parameter [expr {[apm_version_names_compare [ad_acs_version] 5.3.0] == 1
                        ? {{no_data  "#xotcl-core.No_Data#"} {renderer TABLE3} name}
                        : {{no_data  "#xotcl-core.No_Data#"} {renderer TABLE2} name}
                      }]

  Table instproc destroy {} {
    #:log "-- "
    foreach c {__bulkactions __actions __columns} {
      #:log "-- namespace eval [self]::$c {namespace forget *}"
      namespace eval [self]::$c {namespace forget *}
    }
    next
  }
  Table instproc actions {cmd} {
    set M [OrderedComposite create [self]::__actions]
    namespace eval $M [list namespace import -force [self class]::*]
    $M contains $cmd
  }
  Table instproc __bulkactions {cmd} {
    set M [OrderedComposite create [self]::__bulkactions]
    namespace eval $M [list namespace import -force [self class]::*]
    $M contains $cmd
  }
  Table instproc columns {cmd} {
    set M [OrderedComposite create [self]::__columns]
    namespace eval $M [list namespace import -force [self class]::*]
    $M contains $cmd
    set slots [list]
    foreach c [$M children] {
      lappend slots {*}[$c get-slots]
    }
    :proc add $slots {
      set __self [::xo::Table::Line new]
      foreach __v [info vars] {$__self set $__v [set $__v]}
      next $__self
    }
  }

  Table ad_instproc column_names {} {

    Return a list of names of the columns of the current table.  These
    names are used to refer to the columns, e.g. in sorting or when
    values are set.

    @return list of names

  } {
    set names {}
    foreach c [[[self]::__columns] children] {
      lappend names [$c name]
    }
    return $names
  }

  Table instproc render_with {renderer trn_mixin} {
    #:log "-- renderer=$renderer"
    set cl [self class]
    :mixin ${cl}::$renderer
    foreach child [$cl info classchildren] {
      #:log "-- $child class [$child info class] "
      set mixinname ${cl}::${renderer}::[namespace tail $child]
      if {[::xotcl::Object isclass $mixinname]} {
        #if {![$child istype ::xo::OrderedComposite::Child]} continue
        $child instmixin $mixinname
        if {$trn_mixin ne ""} {$child instmixin add $trn_mixin}
        #:log "-- $child using instmixin <[$child info instmixin]>"
      } else {
        #:log "-- no mixin $mixinname"
      }
    }
    Table::Line instmixin $trn_mixin
    :init_renderer
  }

  Table instproc write_csv {
    {-delimiter ","}
  } {
    set output ""
    set line [list]
    foreach column [[self]::__columns children] {
      if {[$column exists no_csv]} continue
      set label [$column label]
      if {[regexp {^#([a-zA-Z0-9_:-]+\.[a-zA-Z0-9_:-]+)#$} $label _ message_key]} {
        set label [_ $message_key]
      }
      set value [string map {\" \\\" \n \r} $label]
      lappend line \"$value\"
    }
    append output [join $line $delimiter] \n
    foreach row [:children] {
      set line [list]
      foreach column [[self]::__columns children] {
        if {[$column exists no_csv]} continue
        set value [string map {\" \\\" \n \r} [$row set [$column set name]]]
        lappend line \"$value\"
      }
      append output [join $line $delimiter] \n
    }
    #ns_return 200 text/plain $output
    if {![info exists :name]} {
      set :name "table"
    }
    set fn [xo::backslash_escape \" ${:name}.csv]
    ns_set put [ns_conn outputheaders] Content-Disposition "attachment;filename=\"$fn\""
    ns_return 200 text/csv $output
    ad_script_abort
  }

}

namespace eval ::xo::Table {

  #
  # Define elements of a ::xo::Table
  #

  Class create ::xo::Table::Line \
      -superclass ::xo::Drawable \
      -instproc attlist {name atts {extra ""}} {
        set result [list]
        foreach att $atts {
          set varname $name.$att
          if {[info exists :$varname]} {
            lappend result $att [::xo::localize [set :$varname]]
          }
        }
        foreach {att val} $extra {lappend result $att $val}
        return $result
      }

  Class create ::xo::Table::Action \
      -superclass ::xo::OrderedComposite::Child \
      -parameter {label url {tooltip {}} {confirm_message {}}}
  #-proc destroy {} {
  #   :log "-- DESTROY "
  #      show_stack
  #      next
  #    }

  Class create ::xo::Table::Field \
      -superclass ::xo::OrderedComposite::Child \
      -parameter {
        label
        {html {}}
        {orderby ""}
        name
        {richtext false}
        no_csv
        {CSSclass ""}
        {hide 0}
      } \
      -instproc init {} {
        set :name [namespace tail [self]]
      } \
      -instproc get-slots {} {
        set slots [list -${:name}]
        foreach subfield {richtext CSSclass} {
          lappend slots [list -${:name}.$subfield ""]
        }
        return $slots
      }

  Class create ::xo::Table::BulkAction \
      -superclass ::xo::OrderedComposite::Child \
      -parameter {name id {html {}} {hide 0}} \
      -instproc actions {cmd} {
        #:init
        set grandParent [[:info parent] info parent]
        if {![info exists :name]} {set :name [namespace tail [self]]}
        #set M [::xo::OrderedComposite create ${grandParent}::__bulkactions]
        set M [::xo::OrderedComposite create ${grandParent}::__bulkactions -noinit]
        namespace eval $M {namespace import -force ::xo::Table::*}
        $M contains $cmd
        $M set __belongs_to [self]
        $M set __identifier ${:name}
      } \
      -instproc get-slots {} {
        ;
      }

  Class create ::xo::Table::AnchorField \
      -superclass ::xo::Table::Field \
      -instproc get-slots {} {
        set slots [list -${:name}]
        foreach subfield {href title CSSclass} {
          lappend slots [list -${:name}.$subfield ""]
        }
        return $slots
      }

  Class create ::xo::Table::HiddenField \
      -superclass ::xo::Table::Field \
      -instproc get-slots {} {
        return [list -${:name}]
      }

  Class create ::xo::Table::ImageField \
      -parameter {src width height border title alt} \
      -superclass ::xo::Table::Field \
      -instproc get-slots {} {
        set slots [list -${:name}]
        lappend slots [list -${:name}.src ${:src}]
        lappend slots [list -${:name}.CSSclass ${:CSSclass}]
        foreach att {width height border title alt} {
          if {[info exists :$att]} {
            lappend slots [list -${:name}.$att [:$att]]
          } else {
            lappend slots [list -${:name}.$att]
          }
        }
        return $slots
      }

  Class create ::xo::Table::ImageAnchorField \
      -superclass ::xo::Table::ImageField \
      -instproc get-slots {} {
        return [concat [next]  -${:name}.href ""]
      }

  Class create ::xo::Table::ImageField_EditIcon \
      -superclass ImageAnchorField -parameter {
        {src /resources/acs-subsite/Edit16.gif} {width 16} {height 16} {border 0}
        {title "[_ xotcl-core.edit_item]"} {alt "edit"}
      }

  Class create ::xo::Table::ImageField_AddIcon \
      -superclass ImageAnchorField -parameter {
        {src /resources/acs-subsite/Add16.gif} {width 16} {height 16} {border 0}
        {title "[_ xotcl-core.add_item]"} {alt "add"}
      }

  Class create ::xo::Table::ImageField_ViewIcon \
      -superclass ImageAnchorField -parameter {
        {src /resources/acs-subsite/Zoom16.gif} {width 16} {height 16} {border 0}
        {title "[_ xotcl-core.view_item]"} {alt "view"}
      }
  Class create ::xo::Table::ImageField_DeleteIcon \
      -superclass ImageAnchorField -parameter {
        {src /resources/acs-subsite/Delete16.gif} {width 16} {height 16} {border 0}
        {title "[_ xotcl-core.delete_item]"} {alt "delete"}
      }
}

namespace eval ::xo::Table {
  #
  # Export ::xo::Table elements
  #
  namespace export Field AnchorField HiddenField Action ImageField ImageAnchorField \
      ImageField_EditIcon ImageField_ViewIcon ImageField_DeleteIcon ImageField_AddIcon \
      BulkAction
}


namespace eval ::xo::Table {
  #
  # Class for rendering ::xo::Table as the html TABLE
  #
  Class create TABLE \
      -superclass ::xo::Drawable \
      -instproc init_renderer {} {
        #:log "--"
        set :__rowcount 0
        set :css.table-class list
        set :css.tr.even-class list-even
        set :css.tr.odd-class list-odd
      }

  TABLE instproc render-actions {} {
    html::tr -class list-button-bar  {
      set cols [llength [[self]::__columns children]]
      html::td -colspan $cols -class list-button-bar {
        set children [[self]::__actions children]
        set last [lindex $children end]
        foreach o $children {
          $o render
          if {$o ne $last} {
            html::t -disableOutputEscaping "&middot;"
          }
        }
      }
    }
  }

  TABLE instproc render-bulkactions {} {
    set bulkactions [[self]::__bulkactions children]
    html::div -class "list-button-bar-bottom" {
      html::t "#xotcl-core.Bulk_actions#:"
      set bulkaction_container [[lindex $bulkactions 0] set __parent]
      set name [$bulkaction_container set __identifier]

      html::ul -class compact {
        foreach ba $bulkactions {
          set id [::xowiki::Includelet html_id $ba]
          html::li {
            html::a -title [$ba tooltip] -id $id -class button -href # \
                {
                  html::t [$ba label]
                }
          }
          set script [subst {
            acs_ListBulkActionClick("$name","[$ba url]");
          }]
          if {[$ba confirm_message] ne ""} {
            set script [subst {
              if (confirm('[$ba confirm_message]')) {
                $script
              }
            }]
          }
          template::add_event_listener \
              -id $id \
              -preventdefault=false \
              -script $script
        }
      }
    }
  }

  TABLE instproc render-body {} {
    html::tr -class list-header {
      foreach o [[self]::__columns children] {
        $o render
      }
    }
    set children [:children]
    if {[llength $children] == 0} {
      html::tr {html::td { html::t ${:no_data}}}
    } else {
      foreach line [:children] {
        #:log "--LINE vars=[:info vars] cL: [[self class] info vars] r=[:renderer]"
        html::tr -class [expr {[incr :__rowcount]%2 ? ${:css.tr.odd-class} : ${:css.tr.even-class}}] {
          foreach field [[self]::__columns children] {
            set CSSclass [list "list" {*}[$field CSSclass]]
            html::td  [concat [list class $CSSclass] [$field html]] {
              $field render-data $line
            }
          }
        }
      }
    }
  }

  TABLE instproc render {} {
    if {![nsf::is object [self]::__actions]} {:actions {}}
    if {![nsf::is object [self]::__bulkactions]} {:bulkactions {}}
    set bulkactions [[self]::__bulkactions children]
    if {[llength $bulkactions] == 0} {
      html::table -class ${:css.table-class} {
        :render-actions
        :render-body
      }
    } else {
      set name [[self]::__bulkactions set __identifier]
      html::form -name $name -method POST {
        html::table -class ${:css.table-class} {
          :render-actions
          :render-body
        }
        :render-bulkactions
      }
    }
  }

  #
  # Define renderer for elements of a Table
  #
  # ::xo:Table requires the elements to have the methods render and render-data
  #

  Class create TABLE::Action \
      -superclass ::xo::Drawable \
      -instproc render {} {
        html::a -class button -title [:_ tooltip] -href [:url] {
          html::t [:_ label]
        }
        #:log "-- "
      }
  #-proc destroy {} {
  #  :log "-- DESTROY"
  #  show_stack
  #  next
  #}

  Class create TABLE::Field -superclass ::xo::Drawable
  TABLE::Field instproc render-data {line} {
    $line instvar [list ${:name}.richtext richtext]
    if {![info exists richtext] || $richtext eq ""} {
      set richtext [:richtext]
    }
    if {$richtext} {
      html::t -disableOutputEscaping [$line set ${:name}]
    } else {
      html::t [$line set ${:name}]
    }
  }

  TABLE::Field instproc render {} {
    html::th [concat [list class list] [:html]] {
      if {${:orderby} eq ""} {
        html::t [:_ label]
      } else {
        :renderSortLabels
      }
      :render_localizer ;# run this before th is closed
    }
  }

  TABLE::Field instproc renderSortLabels {} {
    set field ${:orderby}
    set lvl [template::adp_level]
    if {$lvl ne ""} {
      upvar #$lvl orderby orderby
    }
    if {![info exists orderby]} {set orderby ""}
    set new_orderby $orderby
    if {$orderby eq "$field,desc"} {
      set new_orderby $field,asc
      set title [_ xotcl-core.Sort_by_this_column_ascending]
      set img /resources/acs-templating/sort-ascending.png
    } elseif {$orderby eq "$field,asc"} {
      set new_orderby $field,desc
      set title [_ xotcl-core.Sort_by_this_column_descending]
      set img /resources/acs-templating/sort-descending.png
    } else {
      set new_orderby $field,asc
      set title [_ xotcl-core.Sort_by_this_column]
      set img /resources/acs-templating/sort-neither.png
    }
    set query [list [list orderby $new_orderby]]
    if {[catch {set actual_query [ns_conn query]}]} {
      set actual_query ""
    }
    foreach pair [split $actual_query &] {
      lassign [split $pair =] key value
      if {$key in {"orderby" "__csrf_token"}} continue
      lappend query [list [ns_urldecode $key] [ns_urldecode $value]]
    }
    set href [export_vars -base [ad_conn url] $query]
    html::a -href $href -title $title {
      html::t [:_ label]
      html::img -src $img -alt ""
    }
  }

  # TODO: title for anchors
  Class create TABLE::AnchorField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
        if {[$line exists ${:name}.href]
            && [set href [$line set ${:name}.href]] ne ""
          } {
          # use the CSS class rather from the Field than not the line
          :instvar CSSclass
          $line instvar [list ${:name}.title title]
          html::a [:get_local_attributes href title {CSSclass class}] {
            return [next]
          }
        }
        next
      }

  Class create TABLE::HiddenField \
      -instproc render {} {;} \
      -instproc render-data {line} {;}


  Class create TABLE::ImageField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
        $line instvar [list ${:name}.CSSclass CSSclass]
        html::a [:get_local_attributes href {style "border-bottom: none;"} {CSSclass class}] {
          html::img [$line attlist ${:name} {src width height border title alt}] {}
        }
        $line render_localizer
      }

  Class create TABLE::ImageAnchorField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
        if {[$line exists ${:name}.href]
            && [set href [$line set ${:name}.href]] ne ""
          } {
          #if {$line exists ${:name}.CSSclass} {set CSSclass [$line set ${:name}.CSSclass]}
          $line instvar [list ${:name}.CSSclass CSSclass]
          html::a [:get_local_attributes href {style "border-bottom: none;"} {CSSclass class}] {
            html::img [$line attlist ${:name} {src width height border title alt}] {}
          }
          $line render_localizer
        }
      }

  Class create TABLE::BulkAction -superclass ::xo::Drawable
  TABLE::BulkAction instproc render {} {
    set name ${:name}
    #:msg [:serialize]
    html::th -class list {
      html::input -type checkbox -name __bulkaction -id __bulkaction \
          -title "Mark/Unmark all rows"
      ::html::CSRFToken
    }
    template::add_body_script -script [subst {
      document.getElementById('__bulkaction').addEventListener('click', function (event) {
        acs_ListCheckAll('$name', this.checked);
      }, false);
    }]
  }

  TABLE::BulkAction instproc render-data {line} {
    #:msg [:serialize]
    set name ${:name}
    set value [$line set [:id]]
    html::input -type checkbox -name $name -value $value \
        -id "$name---[string map {/ _} $value]" \
        -title "Mark/Unmark this row"
  }

  Class create TABLE2 \
      -superclass TABLE \
      -instproc render-actions {} {
        set actions [[self]::__actions children]
        if {[llength $actions] > 0} {
          html::div -class "actions" -style "float: left;" {
            html::ul -style "list-style:none; padding: 10px;" {
              foreach o $actions { html::li -class "button" {$o render} }
            }
          }
        }
      } \
      -instproc render {} {
        if {![nsf::is object [self]::__actions]} {:actions {}}
        if {![nsf::is object [self]::__bulkactions]} {:__bulkactions {}}
        set bulkactions [[self]::__bulkactions children]
        html::div  {
          :render-actions
          if {$bulkactions eq ""} {
            html::div -class table {
              html::table -class ${:css.table-class} {:render-body}
            }
          } else {
            set name [[self]::__bulkactions set __identifier]
            html::form -name $name -action "" {
              html::div -class table {
                html::table -class ${:css.table-class} {:render-body}
                :render-bulkactions
              }
            }
          }
        }
      }


  Class create TABLE2::Action -superclass TABLE::Action
  Class create TABLE2::Field -superclass TABLE::Field
  Class create TABLE2::AnchorField -superclass TABLE::AnchorField
  Class create TABLE2::HiddenField -superclass TABLE::HiddenField
  Class create TABLE2::ImageField -superclass TABLE::ImageField
  Class create TABLE2::ImageAnchorField -superclass TABLE::ImageAnchorField
  Class create TABLE2::BulkAction -superclass TABLE::BulkAction

  Class create TABLE3 \
      -superclass TABLE2 \
      -instproc init_renderer {} {
        next
        set :css.table-class list-table
        set :css.tr.even-class even
        set :css.tr.odd-class odd
      }

  Class create TABLE3::Action -superclass TABLE::Action
  Class create TABLE3::Field -superclass TABLE::Field
  Class create TABLE3::AnchorField -superclass TABLE::AnchorField
  Class create TABLE3::HiddenField -superclass TABLE::HiddenField
  Class create TABLE3::ImageField -superclass TABLE::ImageField
  Class create TABLE3::ImageAnchorField -superclass TABLE::ImageAnchorField
  Class create TABLE3::BulkAction -superclass TABLE::BulkAction
}

Class create TableWidget \
    -superclass ::xo::Table \
    -instproc init {} {
      set trn_mixin [expr {[lang::util::translator_mode_p] ?"::xo::TRN-Mode" : ""}]
      :render_with [:renderer] $trn_mixin
      next
    }



#
# Pure List widget
#

Class create ListWidget -superclass ::xo::OrderedComposite -instproc render {} {
  html::ul -class plainlist {
    foreach o [:children] {
      html::li {
        $o render
      }
    }
  }
}


#
# Define two Master templates, an empty one and one page master
#

Object create defaultMaster -proc decorate {node} {
  $node appendFromScript {
    set slave [tmpl::div]
  }
  return $slave
}

Object create pageMaster -proc decorate {node} {
  $node appendFromScript {
    html::div -class defaultMasterClass {
      #html::t "hello header"
      set slave [tmpl::body]
      #html::t "hello footer"
    }
  }
  return $slave
}


namespace eval ::xo {
  #
  # xo::Page: Templating and CSS
  #
  Class create Page
  Page proc requireCSS {{-order 1} name} {
    set ::_xo_need_css($name) [expr {[array size ::_xo_need_css] + 1000 * $order}]
  }
  Page proc requireStyle {{-order 1} s} {
    set ::_xo_need_style($s) [expr {[array size ::_xo_need_style] + 1000 * $order}]
  }
  Page proc requireJS  name {
    if {![info exists ::_xo_need_js($name)]} {lappend ::_xo_js_order $name}
    set ::_xo_need_js($name)  1
  }
  Page proc requireLink {-rel -type -title -href} {
    template::head::add_link -rel $rel -href $href -type $type -title $title
  }
  Page proc set_property {name element value} {
    set ::xo_property_${name}($element) $value
  }
  Page proc get_property {name} {
    if {[array exists ::xo_property_${name}]} {
      return [array get ::xo_property_${name}]
    }
    return [list]
  }
  Page proc sort_keys_by_value {{-comparison integer} {-direction increasing} pairs} {
    set result [list]
    set a [list]
    foreach {key value} $pairs {
      lappend a [list $key $value]
    }
    foreach pair [lsort -index 1 -$comparison -$direction $a] {
      lappend result [lindex $pair 0]
    }
    return $result
  }

  Page proc header_stuff {} {

    foreach style [:sort_keys_by_value [array get ::_xo_need_style]] {
      template::head::add_style -style $style
    }
    set count 10
    foreach file [:sort_keys_by_value [array get ::_xo_need_css]] {
      template::head::add_css -href $file -media all -order [incr count]
    }
    if {[info exists ::_xo_js_order]} {
      set statements ""
      set order 10
      foreach file $::_xo_js_order {
        if {[string match "*;*" $file]} {
          # it is not a file, but some JavaScript statements
          #append statements [string map {< "&lt;" > "&gt;"} $file] \n
          append statements $file \n
        } else {
          template::head::add_script -src $file -type text/javascript -order [incr order]
        }
      }
      if {$statements ne ""} {
        template::head::add_script -script $statements -type text/javascript -order [incr order]
      }
    }
    return ""
  }
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
