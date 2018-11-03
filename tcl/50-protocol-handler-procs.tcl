namespace eval ::xo {
  Class create ProtocolHandler -parameter {
    {url}
    {package}
  }

  ProtocolHandler ad_instproc unknown {method args} {
    Return connection information similar to ad_conn
  } {
    :log "--[self class] unknown called with '$method' <$args>"
    switch -- [llength $args] {
      0 {
        if {[info exists :$method]} {
          return ${:method}
        }
        return [ad_conn $method]
      }
      1 {set :method $args}
      default {:log "--[self class] ignoring <$method> <$args>"}
    }
  }

  ProtocolHandler ad_instproc set_user_id {} {
    Set user_id based on authentication header
  } {
    set ah [ns_set get [ns_conn headers] Authorization]
    if {$ah ne ""} {
      # should be something like "Basic 29234k3j49a"
      :debug "auth_check authentication info $ah"
      # get the second bit, the base64 encoded bit
      set up [lindex [split $ah " "] 1]
      # after decoding, it should be user:password; get the username
      lassign [split [ns_uudecode $up] ":"] user password
      set auth [auth::authenticate \
                    -username $user \
                    -authority_id [::auth::get_register_authority] \
                    -password $password]
      :debug "auth $user $password returned $auth"
      if {[dict get $auth auth_status] ne "ok"} {
        set auth [auth::authenticate \
                      -email $user \
                      -password $password]
        if {[dict get $auth auth_status] ne "ok"} {
          :debug "auth status [dict get $auth auth_status]"
          set :user_id 0
          throw {AUTH UNAUTHORIZED {unauthorized}} [dict get $auth auth_status]
        }
      }
      :debug "auth_check user_id='[dict get $auth user_id]'"
      ad_conn -set user_id [dict get $auth user_id]

    } else {
      # no authenticate header, anonymous visitor
      ad_conn -set user_id 0
      ad_conn -set untrusted_user_id 0
    }
    set :user_id [ad_conn user_id]
  }

  ProtocolHandler ad_instproc initialize {} {
    Setup connection object and authenticate user
  } {
    ad_conn -reset
    #
    # Make sure, there is no ::ad_conn(request); otherwise the
    # developer support will add all its output to a single var, which
    # can lead easily to running out of resources in busy sites. When
    # unset, the developer support will create its own id.
    unset -nocomplain ::ad_conn(request)

    set :uri [ns_urldecode [ns_conn url]]
    if {[string length ${:uri}] < [string length ${:url}]} {append :uri /}
    set url_regexp "^${:url}"
    regsub $url_regexp ${:uri} {} :uri
    if {![regexp {^[./]} ${:uri}]} {set :uri /${:uri}}
    #:log "--conn_setup: uri '${:uri}' url='${:url}' con='[ns_conn url]'"
    :set_user_id

    set :method [string toupper [ns_conn method]]
    #:log "--conn_setup: uri '${:uri}' method ${:method}"
    set :urlv [split [string trim ${:uri} "/"] "/"]
    set :user_agent [ns_set iget [ns_conn headers] user-agent]
    set :destination [ns_urldecode [ns_set iget [ns_conn headers] Destination]]
    if {${:destination} ne ""} {
      regsub {https?://[^/]+/} ${:destination} {/} dest
      regsub $url_regexp $dest {} :destination
      if {![regexp {^[./]} ${:destination}]} {set :destination /${:destination}}
    }
    :log "--conn_setup: method ${:method} destination '${:destination}' uri '${:uri}' peer [ns_conn peeraddr]"
  }

  ProtocolHandler ad_instproc preauth { args } {
    Handle authorization. This method is called via ns_filter.
  } {
    #:log "--preauth args=<$args>"

    # Restrict to SSL if required
    if { [security::RestrictLoginToSSLP]  && ![security::secure_conn_p] } {
      ns_returnunauthorized
      return filter_return
    }

    #
    # Set common data for all kind of requests. A possible outcome is
    # that we cannot proceed (authentication failure), so we have
    # to trap such cases.
    try {
      :initialize
    } trap {AUTH UNAUTHORIZED} {errorMsg} {
      ns_returnunauthorized
      return filter_return
    } on error {errorMsg} {
      ns_log error "ProtocolHandler: exception during initialization: $errorMsg"
      return filter_return
    }

    if {${:user_id} == 0} {
      #
      # Check, if we are running under the regression test. For this,
      # the nsv must exist and the peeraddr must be the regression
      # test. If this is all true, accept the user_id if provided.
      #
      if {[nsv_array exists aa_test]
          && [nsv_get aa_test logindata logindata]
          && [ns_conn peeraddr] eq [dict get $logindata peeraddr]
        } {
        #ns_log notice logindata=$logindata
        if {[dict exists $logindata user_id]} {
          ad_conn -set user_id [dict get $logindata user_id]
          ad_conn -set untrusted_user_id [dict get $logindata user_id]
          set :user_id [ad_conn user_id]
        }
      } else {
        # for now, require for every user authentication
        ns_returnunauthorized
        return filter_return
      }
    }

    #:log "--preauth filter_ok"
    return filter_ok
  }

  ProtocolHandler ad_instproc register { } {
    Register the NaviServer/AOLserver filter and traces.
    This method is typically called via *-init.tcl.

    Note that the specified url must not have an entry
    in the site-nodes, otherwise the OpenACS request
    processor performs always the cockie-based authorization.

    To change that, it would be necessary to register the
    filter before the request processor
    (currently, there are no hooks for that).
  } {
    set filter_url ${:url}*
    set url ${:url}/*
    set root [string trimright ${:url} /]
    #
    # Methods defined by RFC 2086 (19.6.1 Additional Request Methods):
    #
    #    LINK UNLINK PATCH
    #
    # Methods defined by RFC 2616:
    #
    #    OPTIONS GET HEAD POST PUT DELETE TRACE CONNECT
    #
    # Methods defined by RF C2518:
    #
    #    PROPFIND PROPPATCH MKCOL COPY MOVE LOCK UNLOCK
    #
    # Methods defined by RFC 3253 (versioning extensions):
    #
    #    VERSION-CONTROL REPORT CHECKOUT CHECKIN UNCHECKOUT
    #    MKWORKSPACE UPDATE LABEL MERGE BASELINE-CONTROL
    #    MKACTIVITY
    #
    # Methods defined by RFC 3648 (ordered collections):
    #
    #    ORDERPATCH
    #
    # Methods defined by RFC 3744 (WebDAV):
    #
    #    ACL REPORT
    #
    # Methods defined by RFC 4437 (redirect reference resources):
    #
    #    MKREDIRECTREF UPDATEREDIRECTREF
    #
    # Methods defined by RFC $791 (CalDAV):
    #
    #    MKCALENDAR
    #
    # Methods defined by RFC 4918 (HTTP Extensions):
    #
    #    COPY LOCK MKCOL MOVE PROPFIND PROPPATCH UNLOCK
    #
    # Methods defined by RFC 5323 (WebDAV SEARCH):
    #
    #    SEARCH
    #
    # Methods defined by RFC 5789:
    #
    #    PATCH
    #
    foreach method {
      GET HEAD PUT POST MKCOL COPY MOVE PROPFIND PROPPATCH
      DELETE LOCK UNLOCK OPTIONS
      REPORT
    } {
       ns_register_filter preauth $method $filter_url [self]
       ns_register_filter preauth $method $root       [self]
       ns_register_proc $method $url  [self] handle_request
       ns_register_proc $method $root [self] handle_request

       #:log "--ns_register_filter preauth $method $filter_url  [self]"
       #:log "--ns_register_filter preauth $method $root  [self]"
       #:log "--ns_register_proc $method $url [self] handle_request"
       #:log "--ns_register_proc $method $root [self] handle_request"
     }
    ns_register_proc OPTIONS  / ::xo::minimalProctocolHandler OPTIONS
    ns_register_proc PROPFIND / ::xo::minimalProctocolHandler PROPFIND
  }

  ProtocolHandler ad_instproc get_package_id {} {
    Initialize the given package and return the package_id
    @return package_id
  } {
    ${:package} initialize -url ${:uri}
    #:log "-- ${:package} initialize -url ${:uri}"
    return $package_id
  }

  ProtocolHandler ad_instproc handle_request { args } {
    Process the incoming HTTP request. This method
    could be overloaded by the application and
    dispatches the HTTP requests.
  } {
    #:log "--handle_request method=${:method} uri=$uri\
         #     userid=${:user_id} -ns_conn query '[ns_conn query]'"
    if {[info exists :package] && ${:uri} ne "/"} {
      # We don't call package-initialize for ${:uri} = "/"
      set :package_id [:get_package_id]
    }
    if {[:procsearch ${:method}] ne ""} {
      :${:method}
    } else {
      ns_return 404 text/plain "not implemented"
    }
  }

  #
  # Formatting methods
  #
  ProtocolHandler instproc tcl_time_to_iso8601 {datetime} {
    # RFC2518 requires this just for creationdate
    if {$datetime eq ""} return ""
    set tcl_time [::xo::db::tcl_date $datetime tz]
    return [clock format [clock scan $tcl_time] -format "%Y-%m-%dT%H:%M:%SZ" -gmt 1]
  }

  ProtocolHandler instproc http_date {seconds} {
    # HTTP-Date as defined in RFC2068#section-3.3.1
    return "[clock format $seconds -format {%a, %d %b %Y %T} -gmt 1] GMT"
  }

  ProtocolHandler instproc tcl_time_to_http_date {datetime} {
    # RFC2518 requires this e.g. for getlastmodified
    if {$datetime eq ""} return ""
    return [:http_date [clock scan [::xo::db::tcl_date $datetime tz]]]
  }

  ProtocolHandler instproc multiStatus {body} {
    append _ {<?xml version="1.0" encoding="utf-8" ?>} \n \
        {<D:multistatus xmlns:D="DAV:">} $body \n </D:multistatus> \n
  }

  ProtocolHandler instproc multiStatusResonse {
    -href:required
    -propstats:required
    {-propstatus true}
  } {
    #:log "multiStatusResonse href $href propstats $propstats"
    append reply \n \
        {<D:response xmlns:lp1="DAV:" xmlns:lp2="http://apache.org/dav/props/" xmlns:g0="DAV:">} \
        "\n<D:href>$href</D:href>\n"
    # The multi-status respons has 2 formats
    # - with <D:propstat> (used in PROPFIND and PROPPATCH)
    # - without <D:propstat> (used in other cases, e.g. DELETE, COPY, MOVE for collections)
    # http://www.webdav.org/specs/rfc4918.html#multi-status.response
    #
    foreach {props status} $propstats {
      if {$propstatus} {
        append reply <D:propstat>\n
        if {[llength $props] > 0} {
          append reply <D:prop>\n
          foreach {name value} $props {
            if {$value ne ""} {
              append reply <$name>$value</$name>\n
            } else {
              append reply <$name/>\n
            }
          }
          append reply </D:prop>\n
        } else {
          append reply <D:prop/>\n
        }
        append reply <D:status>$status</D:status>\n</D:propstat>\n
      } else {
        append reply <D:status>$status</D:status>\n
      }
    }
    append reply </D:response>\n
  }

  ProtocolHandler instproc multiStatusError {status} {
    lappend davprops \
        D:getlastmodified "" \
        D:getcontentlength "" \
        D:creationdate "" \
        D:resourcetype ""
    set r [:multiStatus [:multiStatusResonse \
                             -href [ns_urldecode [ns_conn url]] \
                             -propstats [list $davprops $status]]]
    #:log multiStatusError=$r
    ns_return 207 text/xml $r
  }

  #
  # Some dummy HTTP methods
  #
  ProtocolHandler instproc GET {} {
    :log "--GET method"
    ns_return 200 text/plain GET-${:uri}
  }
  ProtocolHandler instproc PUT {} {
    :log "--PUT method [ns_conn content]"
    ns_return 201 text/plain "received put with content-length [string length [ns_conn content]]"
  }

  ProtocolHandler instproc OPTIONS {} {
    ns_set put [ns_conn outputheaders] Allow OPTIONS
    ns_return 200 text/plain {}
  }

  ProtocolHandler instproc PROPFIND {} {
    #:log "--ProtocolHandler PROPFIND [ns_conn content]"
    # when GET is not supported on this resource, the get* properties are not be sent
    # see http://www.webdav.org/specs/rfc4918.html, 9.1.5
    lappend davprops \
        lp1:resourcetype    <D:collection/> \
        lp1:creationdate    [:tcl_time_to_iso8601 "2013-06-30 01:21:22.648325+02"] \
        D:supportedlock     {} \
        D:lockdiscovery     {}

    ns_return 207 text/xml [:multiStatus \
                                [:multiStatusResonse \
                                     -href ${:uri} \
                                     -propstats [list $davprops "HTTP/1.1 200 OK"]]]
  }

  ::xo::ProtocolHandler create ::xo::minimalProctocolHandler
  ::xo::minimalProctocolHandler proc OPTIONS {args} {
    ns_set put [ns_conn outputheaders] Allow OPTIONS
    ns_return 200 text/plain {}
  }
  ::xo::minimalProctocolHandler proc PROPFIND {args} {
    :multiStatusError "HTTP/1.1 403 Forbidden"
  }
}


#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
