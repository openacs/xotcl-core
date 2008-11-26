ad_library {
  XOTcl cluster support

  @author Gustaf Neumann
  @creation-date 2007-07-19
  @cvs-id $Id$
}

namespace eval ::xo {

  proc clusterwide args {
    # first, excute the command on the local server
    eval $args
    # then, distribute the command in the cluster
    eval ::xo::Cluster broadcast $args
  }

  proc cache_flush_all {cache pattern} {
    # Provide means to perform a wildcard-based cache flushing on
    # (cluster) machines.
    foreach n [ns_cache names $cache $pattern] {ns_cache flush $cache $n}
  }

  Class Cluster -parameter {host {port 80}}
  Cluster set allowed_host_patterns [list]
  Cluster set url /xotcl-cluster-do 
  Cluster array set allowed_host {
    "127.0.0.1" 1
  }
  # 
  # The allowed commands are of the form 
  #   - command names followed by 
  #   - optional "except patterns"
  #
  Cluster array set allowed_command {
    set "" 
    unset "" 
    nsv_set "" 
    nsv_unset ""
    nsv_incr ""
    bgdelivery ""
    ns_cache "^ns_cache\s+eval"
    xo::cache_flush_all ""
  }
  #
  # Prevent unwanted object generations for unknown 
  # arguments of ::xo::Cluster.
  #
  Cluster proc unknown args {
    error "[self] received unknown method $args"
  }
  #
  # handling the ns_filter methods
  #
  Cluster proc trace args {
    my log ""
    return filter_return
  }
  Cluster proc preauth args {
    my log ""
    my incoming_request
    return filter_return
  }
  Cluster proc postauth args {
    my log ""
    return filter_return
  }
  #
  # handle incoming request issues
  #
  Cluster proc incoming_request {} {
    set cmd [ns_queryget cmd]
    set addr [lindex [ns_set iget [ns_conn headers] x-forwarded-for] end]
    if {$addr eq ""} {set addr [ns_conn peeraddr]}
    #ns_log notice "--cluster got cmd='$cmd' from $addr"
    if {[catch {set result [::xo::Cluster execute [ns_conn peeraddr] $cmd]} errorMsg]} {
      ns_log notice "--cluster error: $errorMsg"
      ns_return 417 text/plain $errorMsg
    } else {
      #ns_log notice "--cluster success $result"
      ns_return 200 text/plain $result
    }
  }

  Cluster proc execute {host cmd} {
    if {![my exists allowed_host($host)]} {
      set ok 0
      foreach g [my set allowed_host_patterns] {
        if {[string match $g $host]} {
          set ok 1
          break
        }
      }
      if {!$ok} {
        error "refuse to execute commands from $host (command: '$cmd')"
      }
    }
    set cmd_name [lindex $cmd 0]
    set key allowed_command($cmd_name)
    #ns_log notice "--cluster $key exists ? [my exists $key]"
    if {[my exists $key]} {
      set except_RE [my set $key]
      #ns_log notice "--cluster [list regexp $except_RE $cmd] -> [regexp $except_RE $cmd]"
      if {$except_RE eq "" || ![regexp $except_RE $cmd]} {
        ns_log notice "--cluster executes command '$cmd' from host $host"
        return [eval $cmd]
      }
    }
    error "command '$cmd' from host $host not allowed"
  }
  #
  # handline outgoing request issues
  #
  Cluster proc broadcast args {
    foreach server [my info instances] {
      eval $server message $args
    }
  }
  Cluster instproc message args {
    my log "--cluster outgoing request to [my host]:[my port] // $args" 
#     set r [::xo::HttpRequest new -volatile \
#                -host [my host] -port [my port] \
#                -path [Cluster set url]?cmd=[ns_urlencode $args]]
#     return [$r set data]

    set r [::xo::AsyncHttpRequest new -volatile \
	       -host [my host] -port [my port] \
	       -path [Cluster set url]?cmd=[ns_urlencode $args]]
  
#     ::bgdelivery do ::xo::AsyncHttpRequest new \
# 	-host [my host] -port [my port] \
#         -path [Cluster set url]?cmd=[ns_urlencode $args] \
# 	-mixin ::xo::AsyncHttpRequest::SimpleListener \
# 	-proc finalize {obj status value} { my destroy }

  }
}