::xo::library doc {
    XoTcl - dav procs.  Simple WebDAV implementation to access files in the wiki via the
    WebDAV protocol.

    @creation-date 2010-04-10
    @author Gustaf Neumann
    @cvs-id $Id$
}
package require XOTcl 2

namespace eval ::xo {

  #
  # Subclass ::xo::ProtocolHander for dav (as used by ical)
  #
  Class create ::xo::dav -superclass ProtocolHandler -parameter {
    {url /webdav}
  }

  set davStmlog 1
  set davLog 1

  #
  # Simple WebDAV interface implementation to access files via the
  # WebDAV protocol. No actual StoreManager is defined here. One can
  # be found in the xowiki package. Additional ones must be
  # implemented based on some OpenACS file API (e.g. the file-storage)
  #
  # Architecturally, this WebDAV implementation could be used for
  # multiple Storage managers (stm).
  # Still to do:
  #
  # a) In this version, the storage manager supports only a limited
  #    set of operations on folder.
  #
  #    Features:
  #      - supports wiki files and folders based on formpages,
  #      - drag file from other filessystems into toplevel or subfolder,
  #      - move files between folders/subfolders,
  #      - delete files in folders/subfolders
  #      - rename files in foldersfiles (according to WebDAV
  #        standards a copy and delete)
  #      - macOS Finder operations: Duplicate, Copy work (folders/subfolders)
  #
  #    Limitations:
  #      - macOS Finder operation "compress" does not work
  #        (complains about volume full)
  #
  # b) locking is not implemented, just a few minimal functions are
  #    included
  #
  # Limitations: some level of functionality could be provided only on
  # selected clients due to differences in webDAV standard
  # implementation. This list is currently hardcoded in the code
  # below, but should be put in a parameter in the future.
  #
  # Gustaf Neumann (April 2010)
  #
  Class create WebDAV -superclass ::xo::dav -parameter {
    {stm}
    {url /xodav/}
    {package_id}
    {user_id}
    {package ::xo::Package}
  }

  WebDAV instproc handle_request {} {
    set user_agent [string tolower [ns_set get [ns_conn headers] User-Agent]]
    # TODO: parameter
    # set allowed_ua_patterns [::parameter::get_from_package_key \
    #                              -package_key xotcl-core \
    #                              -parameter SupportedWebDAVClients \
    #                              -default {Cyberduck WinSCP davfs2 neon gvfs}]
    set allowed_ua_patterns "Cyberduck WinSCP davfs2 neon gvfs"
    if {![regexp -nocase [join $allowed_ua_patterns |] $user_agent]} {
      ns_return 404 text/plain "not supported"
    } else {
      next
    }
  }

  WebDAV instproc log {msg} {if {$::xo::davLog} {ns_log notice "dav: $msg"}}

  # The following block can provide pseudo URLs as mount points, using
  # "!" as substitution character.
  #
  # WebDAV instproc get_package_id {} {
  #   set uri ${:uri}
  #   set top [lindex ${:urlv} 1]
  #   if {[string match *!* $top]} {
  #     regsub -all ! $top / top
  #     set uri /$top/[join [lrange ${:urlv} 2 end] /]
  #     [:package] initialize -url $uri
  #     #$package_id set package_url ${:uri}
  #     :log "--[:package] initialize -url $uri (was ${:uri} urlv ${:urlv})"
  #     return $package_id
  #   } else {
  #     next
  #   }
  # }

  WebDAV instproc not_for_us {} {
    # TODO unwire allowed packages
    return [expr {![info exists :package_id] ||
                   [${:package_id} package_key] ni {xowiki xowf file-storage}}]
  }
  #
  # DAV Methods
  #

  WebDAV instproc OPTIONS {} {
    if {[:not_for_us]} {return [next] }
    ns_set put [ns_conn outputheaders] DAV 1,2
    ns_set put [ns_conn outputheaders] DAV <http://apache.org/dav/propset/fs/1>
    # possible methods: OPTIONS,GET,HEAD,DELETE,TRACE,PROPFIND,PROPPATCH,COPY,MOVE,LOCK,UNLOCK
    ns_set put [ns_conn outputheaders] Allow OPTIONS,GET,DELETE,PROPFIND,PROPPATCH,COPY,MOVE
    ns_set put [ns_conn outputheaders] MS-Author-Via DAV
    ns_return 200 text/plain {}
  }

  WebDAV instproc GET {} {
    set r [${:stm} deliver_file -path ${:uri}]
    :log "### GET returned $r"
    switch [dict get $r success] {
      1 { ; }
      0 { ns_return 404 text/plain [dict get $r msg] }
     -1 { ns_return 403 text/plain "forbidden: [dict get $r msg]" }
    }
  }

  WebDAV instproc HEAD {} {
    set r [${:stm} file_properties -path ${:uri}]
    :log "### HEAD returned $r"
    switch [dict get $r success] {
      1 {
        set hdrs [ns_conn outputheaders]

        set p [lindex [dict get $r props] 0]
        ns_set put $hdrs "Last-Modified"  [:tcl_time_to_http_date [dict get $p last_modified]]
        ns_set put $hdrs "Content-Length" [dict get $p content_length]
        ns_return 200 text/plain ""
      }
      0 {ns_return 404 text/plain "File ${:uri} not found"}
     -1 { ns_return 403 text/plain "forbidden: [dict get $r msg]" }
    }
  }

  WebDAV instproc MKCOL {} {
    :log "### MKCOL ${:uri}"
    set r [${:stm} create_folder -path ${:uri}]
    # :log "MKCOL returned $r"
    switch [dict get $r success] {
      1 { ns_return 201 text/plain "created: [dict get $r msg]" }
      2 { ns_return 204 text/plain [dict get $r msg] }
      0 { ns_return 409 text/plain "no such parent: [dict get $r msg]" }
     -1 { ns_return 403 text/plain "forbidden: [dict get $r msg]" }
    }
  }

  WebDAV instproc PUT {} {
    set contentfile [::xo::get_raw_request_body -as_file]
    # Optionally we would be able to disable creation of some OS specific files
    # set fn [lindex [split [string trimright ${:uri} /] /] end]
    # if {$fn in {".DS_Store" "._.DS_Store"}} {
    # set r [dict create success 1 msg "MacOS-specific '$fn' file ignored"]
    # } else {
      :log "### PUT ${:uri} content length [file size $contentfile] file $contentfile"
      set r [${:stm} create_file -path ${:uri} -contentfile $contentfile]
    # }
    :log "### PUT returned $r"
    switch [dict get $r success] {
      1 { ns_return 201 text/plain "created: [dict get $r msg]" }
      2 { ns_return 204 text/plain [dict get $r msg] }
      0 { ns_return 409 text/plain "no such parent: [dict get $r msg]" }
     -1 { ns_return 403 text/plain "forbidden: [dict get $r msg]" }
    }
  }

  WebDAV instproc PROPPATCH {} {
    set content [::xo::get_raw_request_body -as_string]
    :log "### PROPPATCH ${:uri} $content"
    #
    # win7 wants to set the creation and modification times
    #
    #<D:propertyupdate>
    #<D:set>
    #  <D:prop>
    #    <Z:Win32CreationTime>Sat, 29 Jun 2013 19:36:18 GMT</Z:Win32CreationTime>
    #    <Z:Win32LastAccessTime>Sat, 29 Jun 2013 19:36:18 GMT</Z:Win32LastAccessTime>
    #    <Z:Win32LastModifiedTime>Sat, 29 Jun 2013 19:36:18 GMT</Z:Win32LastModifiedTime>
    #    <Z:Win32FileAttributes>00000020</Z:Win32FileAttributes>
    #  </D:prop>
    #</D:set>
    #</D:propertyupdate>
    #
    # For the time being, we ignore this attempts
    #
    set props ""
    dom parse $content doc1
    $doc1 documentElement root1
    foreach n [$root1 selectNodes //D:prop/*] {
      #ns_log notice "FOUND [$n asHTML]"
      lappend props [$n nodeName] "HTTP/1.1 200 OK"
    }
    # :log "### PROPPATCH props $props"
    set response [:multiStatus \
		      [:multiStatusResonse -href ${:uri} -propstats $props]]
    set r {success 1}
    switch [dict get $r success] {
      1 { ns_return 207 text/xml $response }
    }
  }

  WebDAV instproc DELETE {} {
    set r [${:stm} delete_file -path ${:uri}]
    :log "### DELETE returned $r"
    switch [dict get $r success] {
      1 { ns_return 204 text/plain "" }
      0 { ns_return 404 text/plain [dict get $r msg] }
     -1 { ns_return 403 text/plain "forbidden: [dict get $r msg]" }
    }
  }

  WebDAV instproc COPY {} {
    set r [${:stm} copy_file -path ${:uri} -destination ${:destination}]
    :log "### COPY returned $r"
    switch [dict get $r success] {
      1 { ns_return 201 text/plain "created: [dict get $r msg]" }
      2 { ns_return 204 text/plain "" }
      0 { ns_return 403 text/plain [dict get $r msg] }
     -1 { ns_return 403 text/plain "forbidden: [dict get $r msg]" }
    }
  }

  WebDAV instproc MOVE {} {
    set r [${:stm} move_file -path ${:uri} -destination ${:destination}]
    :log "### MOVE returned $r"
    switch [dict get $r success] {
      1 { ns_return 201 text/plain "created: [dict get $r msg]" }
      2 { ns_return 204 text/plain "" }
      0 { ns_return 403 text/plain "forbidden [dict get $r msg]" }
    }
  }

  WebDAV instproc PROPFIND {} {
    if {[:not_for_us]} { return [next] }

    # uri contains the path without the dav prefix (e.g. /xowiki/),
    :log "### PROPFIND uri <${:uri}> [ns_set array [ns_conn headers]]"
    # :log [ns_conn content]

    set depth [ns_set iget [ns_conn headers] Depth]
    if {$depth ne ""} {set depth [list -depth $depth]}
    set r [${:stm} file_properties -path ${:uri} -prefix "" {*}$depth]

    :log "### PROPFIND returned $r"
    switch [dict get $r success] {
      1 {
        set body ""
        foreach p [dict get $r props] {append body [:multiStatusProps -props $p]}
        set result [:multiStatus $body]
	:log "### PROPFIND returns $result"
        ns_return 207 text/xml $result
      }
      0 { ns_return 404 text/xml "Not found"}
     -1 { :multiStatusError "HTTP/1.1 403 Forbidden" }
    }
  }

  WebDAV instproc LOCK {} {
    :log "--LOCK ${:uri}";#"\n[ns_conn content]\n[ns_set array [ns_conn headers]]\n"
    set text [subst {<?xml version="1.0" encoding="utf-8"?>
<D:prop xmlns:D="DAV:">
<D:lockdiscovery>
<D:activelock>
<D:locktype><D:write/></D:locktype>
<D:lockscope><D:exclusive/></D:lockscope>
<D:depth>0</D:depth>
<ns0:owner xmlns:ns0="DAV:">
<ns0:href>http://www.apple.com/webdav_fs/</ns0:href>
</ns0:owner><D:timeout>Second-600</D:timeout>
<D:locktoken><D:href>opaquelocktoken:[:lockToken]</D:href></D:locktoken>
</D:activelock>
</D:lockdiscovery>
</D:prop>
    }]
    ns_return 200 text/xml $text
  }

  WebDAV instproc UNLOCK {} {
    :log "--UNLOCK ${:uri}";#"\n[ns_conn content] \n[ns_set array [ns_conn headers]]\n"
    ns_return 204 text/plain ""
  }


  #
  # some helper methods
  #

  WebDAV instproc init {} {
    # tell storage manager about corresponding dav object
    ${:stm} configure -dav [self]
    next
  }

  WebDAV instproc lockToken {} {
    #
    # generate a token with the same properties as oacs-dav
    #
    set tokenList [list]
    set peer [split [ns_conn peeraddr] .]
    foreach v [list [clock clicks -milliseconds] \
                   [ns_rand 2147483647] \
                   [lindex $peer 0][lindex $peer 1] [lindex $peer 2][lindex $peer 3]] {
      lappend tokenList [format %x $v]
    }
    return [join $tokenList -]
  }

  WebDAV instproc encode {string} {
    set user_agent [string tolower [ns_set get [ns_conn headers] User-Agent]]
    if {[string first "microsoft data access internet publishing" $user_agent] != -1} {
      set string [string map {" " "{{blank}}"} $string]
      set string [ns_urlencode $string]
      set string [string map {"%7b%7bblank%7d%7d" " "} $string]
    } else {
      set string [string map {"&" "&amp;"} $string]
    }
    return $string
  }

  WebDAV instproc multiStatusProps {-props:required} {
    #:log "multiStatusProps $props"
    # we require the fields href, collection, last_modified, content_type
    set r [dict create content_length "" status "HTTP/1.1 200 OK" creationdate ""]
    set r [dict replace $r {*}$props]

    lappend davprops \
	lp1:resourcetype    [expr {[dict get $r collection] ? "<D:collection/>" : ""}] \
	lp1:creationdate    [:tcl_time_to_iso8601 [dict get $r creationdate]] \
	lp1:getlastmodified [:tcl_time_to_http_date [dict get $r last_modified]] \
	g0:getcontentlength [dict get $r content_length] \
	D:supportedlock     {} \
	D:lockdiscovery     {} \
	D:getcontenttype    [dict get $r content_type]

    return [:multiStatusResonse \
		-href [dict get $r href] \
		-propstats [list $davprops [dict get $r status]]]
  }

  #
  # define abstact storage manager
  #

  nx::Class create StorageManager -superclass nx::Class {
    :property dav

    :public method deliver_file    {-path:required} {
      # abstract method to send a file to the user
    }
    :public method create_file     {-path:required -content:required}  {
      # abstract method to create a new file
    }
    :public method create_folder   {-path:required} {
      # abstract method to create a new folder
    }
    :public method delete_file     {-path:required} {
      # abstract method to delete a file
    }
    :public method copy_file       {-path:required -destination:required} {
      # abstract method to copy a file
    }
    :public method move_file       {-path:required -destination:required} {
      # abstract method to move a file
    }
    :public method file_properties {-path:required -prefix {-depth 0}} {
      # abstract method to return properties of a file
    }

    :method log {msg} {if {$::xo::davStmlog} {ns_log notice "dav::stm: $msg"}}
  }

}
::xo::library source_dependent

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
