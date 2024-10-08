ad_include_contract {
  display information about revisions of content items

  @author Gustaf Neumann (gustaf.neumann@wu-wien.ac.at)
  @creation-date Oct 23, 2005
  @cvs-id $Id$
} {
  page_id:naturalnum,notnull
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
    -no_data [_ acs-content-repository.No_Revisions] \
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
      author { label #acs-content-repository.Creation_User#
        display_template {@revisions.author_link;noquote@}
      }
      content_size { label #acs-content-repository.Size# html {align right}
        display_col content_size_pretty
      }
      last_modified_ansi { label #acs-content-repository.Last_Modified#
        display_col last_modified_pretty
      }
      description { label #acs-content-repository.Description#}
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
        display_template {<adp:icon name="trash" title="Delete Revision">}
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
  set content_size_pretty  [lc_content_size_pretty -size $content_size]

  if {$name eq ""} {set name [_ acs-kernel.Untitled]}
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
  set version_delete [_ acs-content-repository.Delete_Revision]
  set author_link [acs_community_member_link -user_id $author_id -label $author]
}


# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
