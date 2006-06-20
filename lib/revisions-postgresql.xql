<?xml version="1.0"?>

<queryset>
  <rdbms><type>postgresql</type><version>7.1</version></rdbms>

  <fullquery name="revisions_info">      
    <querytext>
    select  ci.name, n.revision_id as version_id,
      person__name(n.creation_user) as author,
      n.creation_user as author_id,
      to_char(n.last_modified,'YYYY-MM-DD HH24:MI:SS') as last_modified_ansi,
      n.description,
      acs_permission__permission_p(n.revision_id,:user_id,'admin') as admin_p,
      acs_permission__permission_p(n.revision_id,:user_id,'delete') as delete_p,
      char_length(n.data) as content_size,
      content_revision__get_number(n.revision_id) as version_number	
    from cr_revisionsi n, cr_items ci
    where ci.item_id = n.item_id and ci.item_id = :page_id
          and exists (select 1 from acs_object_party_privilege_map m
                      where m.object_id = n.revision_id
                        and m.party_id = :user_id
                        and m.privilege = 'read')
	order by n.revision_id desc
    </querytext>
  </fullquery> 
</queryset>


