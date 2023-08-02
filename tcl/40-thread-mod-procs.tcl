::xo::library doc {

  Tcl API for Thread management provides some support for threads
  under NaviServer and/or AOLserver.

  This package contains
  essentially two classes THREAD and Proxy.
  <p>
  The class THREAD is used to create, initialize
  and destroy threads and to pass commands to these
  threads. It is designed in a way to create threads
  lazyly such that thread definitions can be included
  in the modules directory of the AOLserver and
  therefore be part of the AOLserver blueprints.
  When an instance of THREAD is created (e.g. t1),
  an init-command is provided. e.g.:
  <pre>
  ::xotcl::THREAD create t1 {
    Class create Counter -parameter {{value 1}}
    Counter instproc ++ {} {incr :value}
    Counter create c1
    Counter create c2
  }
  </pre>
  Commands are sent to the thread via the
  "do" method, which returns the result of the
  command evaluated in the specified thread.
  When the first command is sent to a
  non-initialized thread, such as
  <pre>
  set x [t1 do c1 ++]
  </pre>
  the actual thread is created and the thread
  ID is remembered in a tsv array. When a
  THREAD object is destroyed, the associated
  thread is terminated as well.

  Notice that according to the aol-server behavior it
  is possible to create **persistent threads**
  (when the thread object is created during
   startup and provided to all request threads
   through the blueprint), or to create **volatile
  threads** that are created during a request
  and which are deleted when the thread cleanup
  is called after some timeout. Volatile threads can
  shared as well (when different request-threads
                  create the same-named thread objects) and can
  be used for caching proposes. Flushing the cache
  can be done in the thread's exitHandler.

  The Proxy class can be used to simplify
  the interaction with a thread and to
  hide the fact, that certain classes/objects
  are part of a thread. The following command
  creates a Proxy for an object c1 in thread t1.
  After this, c1 can be used like a local object.
  <pre>
  ::xotcl::THREAD::Proxy c1 -attach t1
  set x [c1 ++]
  </pre>
  The Proxy forwards all commands to the
  attached thread except the methods attach, filter,
  detachAll and destroy. The attach method can be used
  to reattach a proxy instance to a different thread, such as
  <pre>
  c1 attach t2
  </pre>
  A proxy can be (temporarily) detachted from a thread via
  <pre>
  c1 filter ""
  </pre>
  Later forwarding to the thread can be re-enabled via
  <pre>
  c1 filter forward
  </pre>
  When a proxy is attached to a thread and
  receives a destroy command, both the proxy
  and the corresponding object in the thread
  are deleted. If only the proxy object is to be
  destroyed, the proxy must be detachted at first.
  The class method detatchAll is provided to detach
  all proxies from their objects.

  @author Gustaf Neumann
  @creation-date 2005-05-13
  @cvs-id $Id$
}

::xotcl::Object setExitHandler {
  #:log "EXITHANDLER of request thread [pid]"
  #if {[catch {::xotcl::THREAD::Proxy detachAll} m]} {
  #  #:log "EXITHANDLER error in detachAll $m"
  #}
}

::Serializer exportObjects {
  ::xotcl::THREAD
  ::xotcl::THREAD::Client
}
#  ::xotcl::THREAD::Proxy

################## main thread support ##################
Class create ::xotcl::THREAD \
    -parameter {
      {persistent 0}
      {lightweight 0}
      {exithandler {ns_log notice "EXITHANDLER of slave thread SELF [pid]"}}
    }

::xotcl::THREAD instproc check_blueprint {} {
  if {![[self class] exists __blueprint_checked]} {
    if {[string first ::xotcl::THREAD [ns_ictl get]] == -1} {
      _ns_savenamespaces
    }
    [self class] set __blueprint_checked 1
  }
}

::xotcl::THREAD instproc init cmd {
  if {$cmd eq "-noinit"} {return}
  #ns_log notice "+++ THREAD cmd='$cmd', epoch=[ns_ictl epoch]"
  if {![ns_ictl epoch]} {
    #ns_log notice "--THREAD init [self] no epoch"

    # We are during initialization. For some unknown reasons, XOTcl
    # is not available in newly created threads, so we have to care for it.
    # We need only a partial initialization, to allow the exit handler
    # to be defined.
    set :initcmd {
      package req XOTcl
      namespace import -force ::xotcl::*
    }
  }
  append :initcmd {
    ns_thread name SELF
  }
  append :initcmd [subst {
    ::xotcl::Object setExitHandler [list [:exithandler]]
  }]
  regsub -all SELF ${:initcmd} [self] :initcmd
  append :initcmd \n\
      [list set ::xotcl::currentScript [info script]] \n\
      [list set ::xotcl::currentThread [self]] \n\
      $cmd
  set :mutex [ns_mutex create ns_mutex[self]]
  ns_log notice "mutex ${:mutex} created"
  next
}

::xotcl::THREAD ad_proc -private recreate {obj args} {
  this method catches recreation of THREADs in worker threads
  it reinitializes the thread according to the new definition.
} {
  :log "recreating [self] $obj, tid [$obj exists tid]"
  if {![string match "::*" $obj]} { set obj ::$obj }
  $obj set recreate 1
  next
  $obj init [lindex $args 0]
  if {[nsv_exists [self] $obj] && [$obj exists initcmd]} {
    set tid [nsv_get [self] $obj]
    ::thread::send $tid [$obj set initcmd]
    $obj set tid $tid
    :log "+++ content of thread $obj ($tid) redefined"
  }
}

::xotcl::THREAD instproc destroy {} {
  :log "destroy called"
  if {!${:persistent} &&
      [nsv_exists [self class] [self]]} {
    set tid [nsv_get [self class] [self]]
    set refcount [::thread::release $tid]
    :log "destroying thread object tid=$tid cnt=$refcount"
    if {$refcount == 0} {
      :log "thread terminated"
      nsv_unset [self class] [self]
      ns_mutex destroy ${:mutex}
      ns_log notice "mutex ${:mutex} destroyed"
    }
  }
  next
}

::xotcl::THREAD instproc get_tid {} {
  if {[nsv_exists [self class] [self]]} {
    # the thread was already started
    return [nsv_get [self class] [self]]
  }
  # start a small command in the thread
  :do info exists x
  # now we have the thread and can return the tid
  return ${:tid}
}

::xotcl::THREAD instproc do {-async:switch args} {
  if {![nsv_exists [self class] [self]]} {
    # lazy creation of a new slave thread

    ad_mutex_eval ${:mutex} {
      #:check_blueprint
      #:log "after lock"
      if {![nsv_exists [self class] [self]]} {
        if {${:lightweight}} {
          :log "CREATE lightweight thread"
          set tid [::thread::create -thin]
        } else {
          set tid [::thread::create]
        }
        nsv_set [self class] [self] $tid
        if {${:persistent}} {
          :log "--created new persistent [self class] as $tid pid=[pid]"
        } else {
          :log "--created new [self class] as $tid pid=[pid]"
        }
        #:log "--THREAD DO send [self] epoch = [ns_ictl epoch]"
        if {${:lightweight}} {
        } elseif {![ns_ictl epoch]} {
          #ns_log notice "--THREAD send [self] no epoch"
          # We are during initialization. For some unknown reasons, XOTcl
          # is not available in newly created threads, so we have to care
          # for full initialization, including xotcl blueprint.
          _ns_savenamespaces
          set initcmd [ns_ictl get]
        }
        append initcmd ${:initcmd}
        #ns_log notice "INIT $initcmd"
        ::thread::send $tid $initcmd

        #
        # There is a potential race condition during startup on a very
        # slow/busy system, where the throttle thread can receive
        # commands, although it is not full initialized. One approach
        # would be to move the nsv setting of the pid here, where the
        # thread is fully initialized, .... but unfortunately, this
        # leads to problems as well. This needs deeper investing,
        # ... but is not very important, since it is very hard to
        # reconstruct the problem case.
        #
        #nsv_set [self class] [self] $tid
      } else {
        set tid [nsv_get [self class] [self]]
      }
    }
  } else {
    #
    # Target thread is already up and running.
    #
    set tid [nsv_get [self class] [self]]
  }
  if {![info exists :tid]} {
    #
    # This is the first call.
    #
    if {!${:persistent} && ![info exists :recreate]} {
      #
      # For a shared thread, we do ref-counting through preserve.
      #
      set tid [nsv_get [self class] [self]]
      :log "THREAD::PRESERVE must preserve for sharing request-thread [pid] tid $tid"
      ::thread::preserve $tid
    }
    set :tid $tid
  }
  if {[ns_info shutdownpending]} {
    :log "thread send operation ignored due to pending shutdown: $args"
  } elseif {$async} {
    return [thread::send -async $tid $args]
  } else {
    return [thread::send $tid $args]
  }
}

#
# Create a sample persistent thread that can be accessed
# via request threads.
#
#::xotcl::THREAD create t0 {
#  Class create Counter -parameter {{value 1}}
#  Counter instproc ++ {} {incr :value}
#
#  Counter create c1
#  Counter create c2
#} -persistent 1
#

################## forwarding  proxy ##################
# Class create ::xotcl::THREAD::Proxy -parameter {attach}
# ::xotcl::THREAD::Proxy configure \
    #     -instproc forward args {
#       set cp [self calledproc]
#       if { $cp eq "attach"
#        || $cp eq "filter"
#        || $cp eq "detachAll"} {
#     next
#       } elseif {$cp eq "destroy"} {
#     eval [:attach] do [self] $cp $args
#     :log "destroy"
#     next
#       } else {
#     :log "forwarding [:attach] do [self] $cp $args"
#     eval [:attach] do [self] $cp $args
#       }
#     } -instproc init args {
#       :filter forward
#     } -proc detachAll {} {
#       foreach i [:info instances] {$i filter ""}
#     }

#
# Sample Thread client routine, calls a same named object in the
# server thread. Thread clients should be created in a connection
# thread dynamically to avoid name clashes in the blueprint.

Class create ::xotcl::THREAD::Client -parameter {server {serverobj [self]}}
::xotcl::THREAD::Client instproc do args {
  ${:server} do ${:serverobj} {*}$args
}

::xo::library source_dependent
#
# Local variables:
#    mode: tcl
#    tcl-indent-level: 2
#    indent-tabs-mode: nil
# End:
