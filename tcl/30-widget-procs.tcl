::xo::library doc {
  XOTcl HTML Widget Classes based on tdom

  @author Gustaf Neumann (neumann@wu-wien.ac.at)
  @author Neophytos Demetriou (k2pts@phigita.net)
  @creation-date 2005-11-26
  @cvs-id $Id$
}

::Serializer exportMethods {
  ::xotcl::Object instproc asHTML
}

Object instproc asHTML {{-master defaultMaster} -page:switch} {
  require_html_procs
  dom createDocument html doc
  set root [$doc documentElement]
  if {!$page} {
    $root appendFromScript {my render}
    set n [$root childNode]
    if {$n eq ""} {
      return ""
    }
    return [$n asHTML]
  } else {
    set slave [$master decorate $root]
    $slave appendFromScript {my render}
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
    # Use a global variable to assure cleanup in case of execptions.
    #
    set var __tdom_level
    global $var
    if {[info exists $var]} {
      incr $var $amount
    } else {
      set $var 1
    }
  }

  #Object create ::xo::tmp
  ::xo::tdom::Class instproc unknown args {
    set configurecmds [lrange $args 0 end-1]
    set createcmd [lindex $args end]
    #
    # Keep a stack of nesting levels of ::xo::tdom Objects.
    # The stack is used for building automatically an ordered
    # composite of objects, used e.g. in recursive renderings.
    #
    [self class] instvar stack
    set level [my incr_level]

    #
    # Create a new instance of the current class and configure it.
    #
    #my log "tdom START $level [self], cmd='$configurecmds'"
    set me [eval my new -destroy_on_cleanup $configurecmds]
    #my log "tdom CREATED $level $me ([$me info class])"

    #
    # If we are not on the topmost level, add the created object
    # to the parent ordered composite.
    #
    set stack($level) $me
    if {$level > 1} {
      set parent $stack([expr {$level - 1}])
      #my log "tdom ADD  $level $me to $parent ([$parent info class])"
      $parent add $me
    }

    #
    # search for autoimports: all commands are executed in the ... currently not needed
    #
#     set class [$me info class]
#     foreach cl [concat $class [$class info heritage]] {
#       my log "tdom EVAL $level ns=[namespace current] autoimport in $cl?[$cl exists autoimport]"
#       if {[$cl exists autoimport]} {
#         my log "tdom IMPO [$cl autoimport] into $me"
#         namespace eval ::xo::tmp [list namespace import -force [$cl autoimport]]
#       }
#     }
#    #my log "tdom CMDS $level [lsort [info commands ::xo::tmp::*]]"

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
    # the ::xo::tdom classes behave more like plain tdom commands.
    #
    #my log "tdom AUTO $level [$me autorender]"

    if {$level == 1 && [$me autorender]} {
      #my log "tdom RNDR $level $me render"
      $me render
    }

    #my log "tdom END  $level [self] me=$me"
    set level [my incr_level -1]
    return $me
  }

  #
  # The tdom attribute manager makes it syntactically easier to
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
        foreach {attribute HTMLattribute} $l break
      } else {
        set HTMLattribute $attribute
      }
      #my msg "[my name] check for $attribute => [my exists $attribute]"
      if {[my exists $attribute]} {
        lappend pairs $HTMLattribute [my set $attribute]
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
        foreach {attribute HTMLattribute} $l break
      } else {
        set HTMLattribute $attribute
      }
      #my msg "[my name] check for $attribute => [my exists $attribute]"
      if {[my uplevel info exists $attribute]} {
        lappend pairs $HTMLattribute [my uplevel set $attribute]
      }
    }
    return $pairs
  }
  
  #
  # ::xo::tdom::Object
  # is the top of the class hierarchies for tdom objects
  #
  ::xotcl::Class create ::xo::tdom::Object \
      -superclass {::xo::tdom::AttributeManager ::xo::OrderedComposite} \
      -parameter {{autorender true}}

  ::xo::tdom::Object instproc render {} {
    foreach o [my children] { $o render }
  }

}




namespace eval ::xo {
  #
  # Localization
  #

  set ::xo::acs_lang_url [apm_package_url_from_key acs-lang]admin

  proc localize {text {inline 0}} {
    #ns_log notice "--local $text $inline"
    set obj [uplevel self]
    if {![$obj exists __localizer]} {
      $obj set __localizer [list]
    }
    if {[string first \x002 $text] == -1} {
      return $text
    } else {
      set return_text ""
      if {$inline} {
        # Attempt to move all message keys outside of tags
        while { [regsub -all {(<[^>]*)(\x002\(\x001[^\x001]*\x001\)\x002)([^>]*>)} $text {\2\1\3} text] } {}
        
        # Attempt to move all message keys outside of <select>...</select> statements
        regsub -all -nocase {(<option\s[^>]*>[^<]*)(\x002\(\x001[^\x001]*\x001\)\x002)([^<]*</option[^>]*>)} $text {\2\1\3} text
        
        while { [regsub -all -nocase {(<select[^>]*>[^<]*)(\x002\(\x001[^\x001]*\x001\)\x002)} $text {\2\1} text] } {}
      }

      while {[regexp {^([^\x002]*)\x002\(\x001([^\x001]*)\x001\)\x002(.*)$} $text _ \
                  before key text]} {
	append return_text $before
	foreach {package_key message_key} [split $key .] break
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

  Class Localizer -parameter {type key url}

  Localizer instproc render {} {
    html::a -title [my key] -href [my url] {
      switch -- [my type] {
	localized {set char o; set style "color: green"}
        us_only   {set char *; set style "background-color: yellow; color: red;"}
        missing   {set char @; set style "background-color: red; color: white;"}
      }
      html::span -style $style {html::t $char}
    }
  }
  Localizer instproc render {} {
     html::a -title [my key] -href [my url] {
       set path /resources/acs-templating/xinha-nightly/plugins/
       switch -- [my type] {
	 localized {set img ImageManager/img/btn_ok.gif}
         us_only  {set img Filter/img/ed_filter.gif}
         missing  {set img LangMarks/img/en.gif}
       }
       html::img -alt [my type] -src $path/$img -width 16 -height 16 -border 0
     }
   }

  ## todo : make these checks only in trn mode (additional mixin)
  
  Class Drawable \
      -superclass ::xo::tdom::AttributeManager \
      -instproc _ {attr} {
	my set $attr
      } \
      -instproc render_localizer {} {
      }

  Class TRN-Mode \
      -instproc _ {attr} {
	return [::xo::localize [my set $attr]]
      } \
      -instproc render_localizer {} {
	#my log "-- "
	if {[my exists __localizer]} {
	  foreach l [my set __localizer] {
	    $l render
	    $l destroy
	  }
	}
	my set __localizer [list]
      } \
      -instproc render-data args {
	next
	my render_localizer
      } \
      -instproc render args {
	next
	my render_localizer
      }
  
  #
  # for the time being, just a proc
  #
  proc get_user_name {uid} {
    if {$uid ne "" && $uid != 0} {
      if {[catch {acs_user::get -user_id $uid -array user}]} {
        # we saw some strange cases, where after a regression,
        # a user_id was present, which was already deleted...
        return nobody
      }
      return "$user(first_names) $user(last_name)"
    } else {
      return nobody
    }
  }

  #
  # define an abstract table
  #
  Class Table -superclass OrderedComposite \
      -parameter [expr {[apm_version_names_compare [ad_acs_version] 5.3.0] == 1 ? 
			{{no_data  "No Data"} {renderer TABLE3} name} :
			{{no_data  "No Data"} {renderer TABLE2} name} 
		      }]
  
  Table instproc destroy {} {
    #my log "-- "
    foreach c {__bulkactions __actions __columns} {
      #my log "-- namespace eval [self]::$c {namespace forget *}"
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
      eval lappend slots [$c get-slots]
    }
    my proc add $slots {
      set __self [::xo::Table::Line new]
      foreach __v [info vars] {$__self set $__v [set $__v]}
      next $__self
    }
  }

  Table instproc render_with {renderer trn_mixin} {
    #my log "-- renderer=$renderer"
    set cl [self class]
    [self] mixin ${cl}::$renderer 
    foreach child [$cl info classchildren] {
      #my log "-- $child class [$child info class] "
      set mixinname ${cl}::${renderer}::[namespace tail $child]
      if {[::xotcl::Object isclass $mixinname]} {
        #if {![$child istype ::xo::OrderedComposite::Child]} continue
	$child instmixin $mixinname
	if {$trn_mixin ne ""} {$child instmixin add $trn_mixin}
	#my log "-- $child using instmixin <[$child info instmixin]>"
      } else {
	#my log "-- no mixin $mixinname"
      }
    }
    Table::Line instmixin $trn_mixin
    my init_renderer
  }

  Table instproc write_csv {} {
    set output ""
    set line [list]
    foreach column [[self]::__columns children] {
      if {[$column exists no_csv]} continue
      set label [$column label]
      if {[regexp {^#(.*)#$} $label _ message_key]} {
        set label [_ $message_key]
      }
      set value [string map {\" \\\" \n \r)} $label]
      lappend line \"$value\"
    }
    append output [join $line ,] \n
    foreach row [my children] {
      set line [list]
      foreach column [[self]::__columns children] {
        if {[$column exists no_csv]} continue
	set value [string map {\" \\\" \n \r} [$row set [$column set name]]]
	lappend line \"$value\"
      }
      append output [join $line ,] \n
    }
    #ns_return 200 text/plain $output
    my instvar name
    if {![my exists name]} {set name "table"}
    ns_set put [ns_conn outputheaders] Content-Disposition "attachment;filename=$name.csv"
    ns_return 200 text/csv $output
  }

  Class create Table::Line \
      -superclass ::xo::Drawable \
      -instproc attlist {name atts {extra ""}} {
	set result [list] 
	foreach att $atts {
	  set varname $name.$att
	  if {[my exists $varname]} {
	    lappend result $att [::xo::localize [my set $varname]]
	  }
	}
	foreach {att val} $extra {lappend result $att $val}
	return $result
      }
  

  #
  # Define elements of a Table
  #
  namespace eval ::xo::Table {
    Class Action \
	-superclass ::xo::OrderedComposite::Child \
	-parameter {label url {tooltip {}}} 
    #-proc destroy {} {
    #   my log "-- DESTROY "
    #	  show_stack
    #	  next
    #	}

    Class Field \
	-superclass ::xo::OrderedComposite::Child \
	-parameter {label {html {}} {orderby ""} name {richtext false} no_csv {CSSclass ""}} \
	-instproc init {} {
	  my set name [namespace tail [self]]
	} \
	-instproc get-slots {} {
	  set slots [list -[my name]]
	  foreach subfield {richtext CSSclass} {
	    lappend slots [list -[my name].$subfield ""]
	  }
          return $slots
	}

    Class BulkAction \
	-superclass ::xo::OrderedComposite::Child \
	-parameter {name id {html {}}} \
        -instproc actions {cmd} {
          #my init
          set grandParent [[my info parent] info parent]
          if {![my exists name]} {my set name [namespace tail [self]]}
          #set M [::xo::OrderedComposite create ${grandParent}::__bulkactions]
          set M [::xo::OrderedComposite create ${grandParent}::__bulkactions -noinit]
          namespace eval $M {namespace import -force ::xo::Table::*}
          $M contains $cmd
          $M set __belongs_to [self]
          $M set __identifier [my set name]
        } \
        -instproc get-slots {} {
          ;
        }

    Class AnchorField \
	-superclass ::xo::Table::Field \
	-instproc get-slots {} {
	  set slots [list -[my name]]
	  foreach subfield {href title CSSclass} {
	    lappend slots [list -[my name].$subfield ""]
	  }
	  return $slots
	}

    Class HiddenField \
	-superclass ::xo::Table::Field \
	-instproc get-slots {} {
	  return [list -[my name]]
	}

    Class ImageField \
	-parameter {src width height border title alt} \
	-superclass ::xo::Table::Field \
	-instproc get-slots {} {
	  set slots [list -[my name]]
	  lappend slots [list -[my name].src [my src]]
	  lappend slots [list -[my name].CSSclass [my CSSclass]]
	  foreach att {width height border title alt} {
	    if {[my exists $att]} {
	      lappend slots [list -[my name].$att [my $att]]
	    } else {
	      lappend slots [list -[my name].$att]
	    }
	  }
	  return $slots
	}

    Class ImageAnchorField \
	-superclass ::xo::Table::ImageField \
	-instproc get-slots {} {
          return [concat [next]  -[my name].href ""]
	}

    Class ImageField_EditIcon \
	-superclass ImageAnchorField -parameter {
	  {src /resources/acs-subsite/Edit16.gif} {width 16} {height 16} {border 0} 
	  {title "[_ xotcl-core.edit_item]"} {alt "edit"}
	}
    
    Class ImageField_AddIcon \
	-superclass ImageAnchorField -parameter {
	  {src /resources/acs-subsite/Add16.gif} {width 16} {height 16} {border 0} 
	  {title "[_ xotcl-core.add_item]"} {alt "add"}
	}

    Class ImageField_ViewIcon \
	-superclass ImageAnchorField -parameter {
	  {src /resources/acs-subsite/Zoom16.gif} {width 16} {height 16} {border 0} 
	  {title "[_ xotcl-core.view_item]"} {alt "view"}
	}
    Class ImageField_DeleteIcon \
	-superclass ImageAnchorField -parameter {
	  {src /resources/acs-subsite/Delete16.gif} {width 16} {height 16} {border 0} 
	  {title "[_ xotcl-core.delete_item]"} {alt "delete"}
	}
    
    # export table elements
    namespace export Field AnchorField HiddenField Action ImageField ImageAnchorField \
	ImageField_EditIcon ImageField_ViewIcon ImageField_DeleteIcon ImageField_AddIcon \
        BulkAction
  }
  
}


namespace eval ::xo::Table {
  #
  # Class for rendering ::xo::Table as the html TABLE
  #
  Class TABLE \
      -superclass ::xo::Drawable \
      -instproc init_renderer {} {
	#my log "--"
	my set __rowcount 0
        my set css.table-class list
        my set css.tr.even-class list-even
        my set css.tr.odd-class list-odd
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
      html::t "Bulk-Actions:"
      set bulkaction_container [[lindex $bulkactions 0] set __parent]
      set name [$bulkaction_container set __identifier]

      html::ul -class compact {
        foreach ba $bulkactions {
          html::li {
            html::a -title [$ba tooltip] -class button -href # \
                -onclick "acs_ListBulkActionClick('$name','[$ba url]'); return false;" \
                {
                  html::t [$ba label]
                }
          }
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
    set children [my children]
    if {[llength $children] == 0} {
      html::tr {html::td { html::t [my set no_data]}}
    } else {
      foreach line [my children] {
        #my log "--LINE vars=[my info vars] cL: [[self class] info vars] r=[my renderer]"
	html::tr -class [expr {[my incr __rowcount]%2 ? 
                               [my set css.tr.odd-class] : 
                               [my set css.tr.even-class] }] {
	  foreach field [[self]::__columns children] {
	    html::td  [concat [list class list] [$field html]] { 
	      $field render-data $line
	    }
	  }
	}
      }
    }
  }
  
  TABLE instproc render {} {
    if {![my isobject [self]::__actions]} {my actions {}}
    if {![my isobject [self]::__bulkactions]} {my bulkactions {}}
    set bulkactions [[self]::__bulkactions children]
    if {$bulkactions eq ""} {
      html::table -class [my set css.table-class] {
        my render-actions
        my render-body
      }
    } else {
      set name [[self]::__bulkactions set __identifier]
      html::form -name $name -method POST { 
        html::table -class [my set css.table-class] {
          my render-actions
          my render-body
        }
        my render-bulkactions
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
	html::a -class button -title [my _ tooltip] -href [my url] { 
	  html::t [my _ label]
	}
	#my log "-- "
      }
  #-proc destroy {} {
  #  my log "-- DESTROY"
  #  show_stack 
  #  next
  #}

  Class create TABLE::Field -superclass ::xo::Drawable 
  TABLE::Field instproc render-data {line} {
    $line instvar [list [my name].richtext richtext]
    if {![info exists richtext] || $richtext eq ""} {
      set richtext [my richtext]
    }
    if {$richtext} {
      html::t -disableOutputEscaping [$line set [my name]]
    } else {
      html::t [$line set [my name]] 
    }
  }

  TABLE::Field instproc render {} {
    html::th [concat [list class list] [my html]] { 
      if {[my set orderby] eq ""} {
	html::t [my _ label]
      } else {
	my renderSortLabels
      }
      my render_localizer ;# run this before th is closed
    }
  }

  TABLE::Field instproc renderSortLabels {} {
    set field [my set orderby]
    set lvl [template::adp_level]
    if {$lvl ne ""} {
      upvar #$lvl orderby orderby
    }
    if {![info exists orderby]} {set orderby ""}
    set new_orderby $orderby
    if {$orderby eq "$field,desc"} {
      set new_orderby $field,asc
      set title "Sort by this column ascending"
      set img /resources/acs-templating/sort-ascending.png
    } elseif {$orderby eq "$field,asc"} {
      set new_orderby $field,desc
      set title "Sort by this column descending"
      set img /resources/acs-templating/sort-descending.png
    } else {
      set new_orderby $field,asc
      set title "Sort by this column"
      set img /resources/acs-templating/sort-neither.png
    }
    set query [list [list orderby $new_orderby]]
    if {[catch {set actual_query [ns_conn query]}]} {
      set actual_query ""
    }
    foreach pair [split $actual_query &] {
      foreach {key value} [split $pair =] break
      if {$key eq "orderby"} continue
      lappend query [list [ns_urldecode $key] [ns_urldecode $value]]
    }
    set href [export_vars -base [ad_conn url] $query]
    html::a -href $href -title $title {
      html::t [my _ label]
      html::img -src $img -alt "" -border 0
    }
  }

  # TODO: title for anchors
  Class create TABLE::AnchorField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
	if {[$line exists [my name].href] && 
	    [set href [$line set [my name].href]] ne ""} {
          # use the CSS class rather from the Field than not the line
          my instvar CSSclass
          $line instvar [list [my name].title title]
          html::a [my get_local_attributes href title {CSSclass class}] {
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
        $line instvar [list [my name].CSSclass CSSclass]
        html::a [my get_local_attributes href {style "border-bottom: none;"} {CSSclass class}] {
          html::img [$line attlist [my name] {src width height border title alt}] {}
        }
        $line render_localizer
      }

  Class create TABLE::ImageAnchorField \
      -superclass TABLE::Field \
      -instproc render-data {line} {
        set href [$line set [my name].href]
        if {$href ne ""} {
          #if {$line exists [my name].CSSclass} {set CSSclass [$line set [my name].CSSclass]}
          $line instvar [list [my name].CSSclass CSSclass]
          html::a [my get_local_attributes href {style "border-bottom: none;"} {CSSclass class}] {
            html::img [$line attlist [my name] {src width height border title alt}] {}
          }
          $line render_localizer
        }
      }

  Class create TABLE::BulkAction -superclass ::xo::Drawable 
  TABLE::BulkAction instproc render {} {
    set name [my name]
    #my msg [my serialize]
    html::th -class list { 
      html::input -type checkbox -name __bulkaction \
          -onclick "acs_ListCheckAll('$name', this.checked)" \
          -title "Mark/Unmark all rows"
    }
  }
  TABLE::BulkAction instproc render-data {line} {
    #my msg [my serialize]
    set name [my name]
    set value [$line set [my id]]
    html::input -type checkbox -name $name -value $value \
        -id "$name,$value" \
        -title "Mark/Unmark this row"
  }

  Class TABLE2 \
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
	if {![my isobject [self]::__actions]} {my actions {}}
	if {![my isobject [self]::__bulkactions]} {my __bulkactions {}}
        set bulkactions [[self]::__bulkactions children]
	html::div  {
	  my render-actions
          if {$bulkactions eq ""} {
            html::div -class table {
              html::table -class [my set css.table-class] {my render-body}
            }
          } else {
            set name [[self]::__bulkactions set __identifier]
            html::form -name $name {
              html::div -class table {
                html::table -class [my set css.table-class] {my render-body}
                my render-bulkactions
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

  Class TABLE3 \
      -superclass TABLE2 \
      -instproc init_renderer {} {
        next 
        my set css.table-class list-table
        my set css.tr.even-class even
        my set css.tr.odd-class odd
      }

  Class create TABLE3::Action -superclass TABLE::Action
  Class create TABLE3::Field -superclass TABLE::Field
  Class create TABLE3::AnchorField -superclass TABLE::AnchorField
  Class create TABLE3::HiddenField -superclass TABLE::HiddenField
  Class create TABLE3::ImageField -superclass TABLE::ImageField
  Class create TABLE3::ImageAnchorField -superclass TABLE::ImageAnchorField
  Class create TABLE3::BulkAction -superclass TABLE::BulkAction
}

Class TableWidget \
    -superclass ::xo::Table \
    -instproc init {} {
      set trn_mixin [expr {[lang::util::translator_mode_p] ?"::xo::TRN-Mode" : ""}]
      my render_with [my renderer] $trn_mixin
      next
    }



#
# Pure List widget
#

Class ListWidget -superclass ::xo::OrderedComposite -instproc render {} {
  html::ul {
    foreach o [my children] {
      html::li {
        $o render
      }
    }
  }
}


#
# Define two Master templates, an empty one and one page master
#

Object defaultMaster -proc decorate {node} {
   $node appendFromScript {
     set slave [tmpl::div]
   }
  return $slave
}

Object pageMaster -proc decorate {node} {
  $node appendFromScript {
    html::div -class defaultMasterClass {
      html::t "hello header"
      set slave [tmpl::body]
      html::t "hello footer"
    }
  }
  return $slave
}


namespace eval ::xo {
  #
  # templating and CSS
  #
  Class create Page
  Page proc requireCSS {{-order 1} name} {
    set ::_xo_need_css($name) [expr {[array size ::_xo_need_css]+1000*$order}]
  }
  Page proc requireStyle {{-order 1} s} {
    set ::_xo_need_style($s) [expr {[array size ::_xo_need_style]+1000*$order}]
  }
  Page proc requireJS  name {
    if {![info exists ::_xo_need_js($name)]} {lappend ::_xo_js_order $name}
    set ::_xo_need_js($name)  1
  }
  Page proc requireLink {-rel -type -title -href} {
    regsub -all ' $title "&apos;" title
    regsub -all ' $href "&apos;" href
    set key "rel='$rel' type='$type' title='$title' href='$href'"
    set ::_xo_need_link($key) 1
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
    set result ""
    foreach link [array names ::_xo_need_link] {
      append result "<link $link>\n"
    }
    foreach style [my sort_keys_by_value [array get ::_xo_need_style]] {
      append result "<style type='text/css'>$style</style>\n"
    }
    foreach file [my sort_keys_by_value [array get ::_xo_need_css]] {
      append result "<link type='text/css' rel='stylesheet' href='$file' media='all' >\n"
    }
    if {[info exists ::_xo_js_order]} {
      set statements ""
      foreach file $::_xo_js_order  {
        if {[string match "*;*" $file]} {
          # it is not a file, but some javascipt statements
          append statements $file \n
        } else {
          append result "<script src='$file' type='text/javascript'></script>\n"
        }
      }
      if {$statements ne ""} {
	append result \n "<script type='text/javascript' >$statements</script>\n"
      }
    }
    return $result
  }
}
::xo::library source_dependent