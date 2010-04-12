namespace eval ::xo {
  Class create ProtocolHandler -parameter {
    {url}
    {package}
  }

  ProtocolHandler ad_instproc unknown {method args} {
    Return connection information similar to ad_conn
  } {
    my log "--[self class] unknown called with '$method' <$args>"
    switch -- [llength $args] {
      0 {if {[my exists $method]} {return [my set method]}
        return [ad_conn $method]
      }
      1 {my set method $args}
      default {my log "--[self class] ignoring <$method> <$args>"}
    }
  }

  ProtocolHandler ad_instproc set_user_id {} {
    Set user_id based on authentication header
  } {
    set ah [ns_set get [ns_conn headers] Authorization]
    if {$ah ne ""} {
      # should be something like "Basic 29234k3j49a"
      my debug "auth_check authentication info $ah"
      # get the second bit, the base64 encoded bit
      set up [lindex [split $ah " "] 1]
      # after decoding, it should be user:password; get the username
      set user [lindex [split [ns_uudecode $up] ":"] 0]
      set password [lindex [split [ns_uudecode $up] ":"] 1]
      array set auth [auth::authenticate \
                          -username $user \
                          -authority_id [::auth::get_register_authority] \
                          -password $password]
      my debug "auth $user $password returned [array get auth]"
      if {$auth(auth_status) ne "ok"} {
        array set auth [auth::authenticate \
                            -email $user \
                            -password $password]
        if {$auth(auth_status) ne "ok"} {
          my debug "auth status $auth(auth_status)"
          ns_returnunauthorized
          my set user_id 0
          return 0
        }
      }
      my debug "auth_check user_id='$auth(user_id)'"
      ad_conn -set user_id $auth(user_id)
      
    } else {
      # no authenticate header, anonymous visitor
      ad_conn -set user_id 0
      ad_conn -set untrusted_user_id 0
    }
    my set user_id [ad_conn user_id]
  }

  ProtocolHandler ad_instproc initialize {} {
    Setup connection object and authenticate user
  } {
    my instvar uri method urlv destination
    ad_conn -reset
    set uri [ns_urldecode [ns_conn url]]
    set url_regexp "^[my url]"
    #my log "--conn_setup: uri '$uri' my url='[my url]' con='[ns_conn url]'"
    regsub $url_regexp $uri {} uri
    if {![regexp {^[./]} $uri]} {set uri /$uri}
    my set_user_id

    set method [string toupper [ns_conn method]]
    #my log "--conn_setup: uri '$uri' method $method"
    set urlv [split [string trimright $uri "/"] "/"]
    set destination [ns_urldecode [ns_set iget [ns_conn headers] Destination]]
    if {$destination ne ""} {
      regsub {https?://[^/]+/} $destination {/} dest
      regsub $url_regexp $dest {} destination
      if {![regexp {^[./]} $destination]} {set destination /$destination}
    }
    #my log "--conn_setup: method $method destination '$destination' uri '$uri'"
  }

  ProtocolHandler ad_instproc preauth { args } {
    Handle authorization. This method is called via ns_filter.
  } {
    #my log "--preauth args=<$args>"
    my instvar user_id 
    
    # Restrict to SSL if required
    if { [security::RestrictLoginToSSLP]  && ![security::secure_conn_p] } {
      ns_returnunauthorized
      return filter_return
    }
  
    # set common data for all kind of requests 
    my initialize

    # for now, require for every user authentification
    if {$user_id == 0} {
      ns_returnunauthorized
      return filter_return
    }
    
    #my log "--preauth filter_ok"
    return filter_ok    
  }

  ProtocolHandler ad_instproc register { } {
    Register the the aolserver filter and traces.
    This method is typically called via *-init.tcl.

    Note, that the specified url must not have an entry
    in the site-nodes, otherwise the openacs request 
    processor performs always the cockie-based authorization.

    To change that, it would be necessary to register the
    filter before the request processor (currently, there
    are no hooks for that).
  } {
    set filter_url [my url]*
    set url [my url]/*
    foreach method {
      GET HEAD PUT POST MKCOL COPY MOVE PROPFIND PROPPATCH
      DELETE LOCK UNLOCK OPTIONS
    } {
      ns_register_filter preauth $method $filter_url  [self]
      ns_register_proc $method $url [self] handle_request
      #my log "--ns_register_filter preauth $method $filter_url  [self]"
      #my log "--ns_register_proc $method $url [self] handle_request"
    }
  }

  ProtocolHandler ad_instproc get_package_id {} {
    Initialize the given package and return the package_id
    @return package_id 
  } {
    my instvar uri package
    $package initialize -url $uri
    #my log "--[my package] initialize -url $uri"
    return $package_id
  }

  ProtocolHandler ad_instproc handle_request { args } {
    Process the incoming HTTP request. This method
    could be overloaded by the application and
    dispatches the HTTP requests.
  } {
    my instvar uri method user_id
  
    #my log "--handle_request method=$method uri=$uri\
    # 	userid=$user_id -ns_conn query '[ns_conn query]'"
    if {[my exists package]} {
      my set package_id [my get_package_id]
    }
    if {[my procsearch $method] ne ""} {
      my $method
    } else {
      ns_return 404 text/plain "not implemented"
    }
  }

  #
  # Some dummy HTTP methods
  #
  ProtocolHandler instproc GET {} {
    my log "--GET method"
    ns_return 200 text/plain GET-[my uri]
  }
  ProtocolHandler instproc PUT {} {
    my log "--PUT method [ns_conn content]"
    ns_return 201 text/plain "received put with content-length [string length [ns_conn content]]"
  }
  ProtocolHandler instproc PROPFIND {} {
    my log "--PROPFIND [ns_conn content]"
    ns_return 204 text/xml {<?xml version="1.0" encoding="utf-8" ?>}
  }
}