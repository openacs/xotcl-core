if {[server_cluster_enabled_p]} {
  set my_ip   [ns_config ns/server/[ns_info server]/module/nssock Address]
  set my_port [ns_config ns/server/[ns_info server]/module/nssock port]
  
  foreach host [server_cluster_all_hosts] {
    set port 80
    regexp {^(.*):(.*)} $host _ host port
    if {"$host-$port" eq "$my_ip-$my_port"}  continue
    ::xo::Cluster create CS_${host}_$port -host $host -port $port
  }
  
  foreach ip [ad_parameter -package_id [ad_acs_kernel_id] ClusterAuthorizedIP server-cluster] {
    if {[string first * $ip] > -1} {
      ::xo::Cluster lappend allowed_host_patterns $ip
    } else {
      ::xo::Cluster set allowed_host($ip) 1
    }
  }
  
  ns_register_filter trace GET /xotcl/do ::xo::Cluster
  ad_register_filter -priority 900 preauth GET /xotcl/do ::xo::Cluster
}
