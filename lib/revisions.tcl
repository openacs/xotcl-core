ad_page_contract {
  display information about revisions of content items

  @author Gustaf Neumann (gustaf.neumann@wu-wien.ac.at)
  @creation-date Oct 23, 2005
  @cvs-id $Id$
} {
  page_id:integer,notnull
  {name ""}
} -properties {
  name:onevalue
  context:onevalue
  page_id:onevalue
  revisions:multirow
  gc_comments:onevalue
}

# check they have read permission on content item
permission::require_permission -object_id $page_id -privilege read

set user_id [ad_conn user_id]
set live_revision_id [content::item::get_live_revision -item_id $page_id]

template::list::create \
    -name revisions \
    -no_data [_ file-storage.lt_There_are_no_versions] \
    -multirow revisions \
    -elements {
      version_number {label "" html {align right}}
      name { label ""
	display_template {
	  <img src='/resources/acs-subsite/Zoom16.gif' \
	      title='View Item' alt='view' \
	      width="16" height="16" border="0">
	}
	sub_class narrow
	link_url_col version_link
      }
      author { label #file-storage.Author#
	display_template {@revisions.author_link;noquote@}
      }
      content_size { label #file-storage.Size# html {align right}
	display_col content_size_pretty
      }
      last_modified_ansi { label #file-storage.Last_Modified#
	display_col last_modified_pretty
      }
      description { label #file-storage.Version_Notes#}
      live_revision { label #xotcl-core.live_revision#
	display_template {
	  <a href='@revisions.live_revision_link@'> \
	  <img src='@revisions.live_revision_icon@' \
	      title='@revisions.live_revision@' alt='@revisions.live_revision@' \
	      width="16" height="16" border="0"></a>
	}
	html {align center}
	sub_class narrow
      }
      version_delete { label "" link_url_col version_delete_link
	display_template {
	  <img src='/resources/acs-subsite/Delete16.gif' \
	      title='Delete Revision' alt='delete' \
	      width="16" height="16" border="0">
	}
	html {align center}
      }
    }

db_multirow -unclobber -extend { 
  author_link last_modified_pretty 
  content_size_pretty version_link version_delete version_delete_link 
  live_revision live_revision_icon live_revision_link
} revisions revisions_info {} {
  set version_number $version_number:
  set last_modified_ansi   [lc_time_system_to_conn $last_modified_ansi]
  set last_modified_pretty [lc_time_fmt $last_modified_ansi "%x %X"]
  if {$content_size < 1024} {
    set content_size_pretty "[lc_numeric $content_size] [_ file-storage.bytes]"
  } else {
    set content_size_pretty "[lc_numeric [format %.2f [expr {$content_size/1024.0}]]] [_ file-storage.kb]"
  }
  
  if {$name eq ""} {set name [_ file-storage.untitled]}
  set live_revision_link [export_vars -base make-live-revision \
			      {page_id name {revision_id $version_id}}]
  set version_delete_link [export_vars -base delete-revision \
			       {page_id name {revision_id $version_id}}]
  set version_link [export_vars -base view {{revision_id $version_id} {item_id $page_id}}]
  if {$version_id != $live_revision_id} {
    set live_revision "Make this Revision Current"
    set live_revision_icon /resources/acs-subsite/radio.gif
  } else {
    set live_revision "Current Live Revision"
    set live_revision_icon /resources/acs-subsite/radiochecked.gif
  }
  set version_delete [_ file-storage.Delete_Version]
  set author_link [acs_community_member_link -user_id $author_id -label $author]
}

