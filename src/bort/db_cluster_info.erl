-module(db_cluster_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,cluster_info).
-define(RECORD,cluster_info).
-record(cluster_info,{
		      cluster_name,
		      num_controllers,
		      hosts,
		      cookie,
		      controller_vms,
		      deployed
		   	      
		  }).

% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(ClusterName,NumControllers,Hosts,Cookie,ControllerVms,Deployed)->
    Record=#?RECORD{
		    cluster_name=ClusterName,
		    num_controllers=NumControllers,
		    hosts=Hosts,
		    cookie=Cookie,
		    controller_vms=ControllerVms,
		    deployed=Deployed
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ClusterName,NumControllers,Hosts,Cookie,ControllerVms,Deployed}||{?RECORD,ClusterName,NumControllers,Hosts,Cookie,ControllerVms,Deployed}<-Z].

info(ClusterName)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_name==ClusterName])),
    [{XClusterName,NumControllers,Hosts,Cookie,ControllerVms,Deployed}||{?RECORD,XClusterName,NumControllers,Hosts,Cookie,ControllerVms,Deployed}<-Z].


num_controllers(ClusterName)->
    case info(ClusterName) of
	[]->
	    {error,[eexist,ClusterName]};
	[{ClusterName,NumControllers,_Hosts,_Cookie,_ControllerVms,_Deployed}]->
	    NumControllers
    end.
hosts(ClusterName)->
    case info(ClusterName) of
	[]->
	    {error,[eexist,ClusterName]};
	[{ClusterName,_NumControllers,Hosts,_Cookie,_ControllerVms,_Deployed}]->
	    Hosts
    end.

cookie(ClusterName)->
    case info(ClusterName) of
	[]->
	    {error,[eexist,ClusterName]};
	[{ClusterName,_NumControllers,_Hosts,Cookie,_ControllerVms,_Deployed}]->
	    Cookie
    end.

controller_vms(ClusterName)->
    case info(ClusterName) of
	[]->
	    {error,[eexist,ClusterName]};
	[{ClusterName,_NumControllers,_Hosts,_Cookie,ControllerVms,_Deployed}]->
	    ControllerVms
    end.

deployed(ClusterName)->
    case info(ClusterName) of
	[]->
	    {error,[eexist,ClusterName]};
	[{ClusterName,_NumControllers,_Hosts,_Cookie,_ControllerVms,Deployed}]->
	    Deployed
    end.


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
