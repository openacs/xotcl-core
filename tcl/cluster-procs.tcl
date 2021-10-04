::xo::library doc {
  XOTcl cluster support (deprecated code, moved into  ::acs::* namespace)

  @author Gustaf Neumann
  @creation-date 2007-07-19
  @cvs-id $Id$
}

namespace eval ::xo {

  ad_proc -deprecated clusterwide args {
    Execute a command on every machine in a cluster.
    The command was moved into the ::acs::* namespace.
  } {
    ::acs::clusterwide {*}$args
  }

  ad_proc -deprecated cache_flush_all {cache pattern} {
    Provide means to perform a wildcard-based cache flushing on
    (cluster) machines.
  } {
    ::acs::cache_flush_all $cache $pattern
  }

  ad_proc -deprecated Cluster args {
    Create a cluster node. The command was moved into the ::acs::*
    namespace.
  } {
    ::acs::Cluster {*}$args
  }
  
}
::xo::library source_dependent

#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
