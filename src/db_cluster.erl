-module(db_cluster).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,cluster_status).
-define(RECORD,cluster_status).
-record(cluster_status,{
			cluster_id,
			monitor_node,
			host_nodes,
			cookie,
			controller_nodes,
			worker_nodes
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

create(ClusterId,MonitorNode,HostNodes,Cookie,ControllerNodes,WorkerNodes)->
    Record=#?RECORD{
		    cluster_id=ClusterId,
		    monitor_node=MonitorNode,
		    host_nodes=HostNodes,
		    cookie=Cookie,
		    controller_nodes=ControllerNodes,
		    worker_nodes=WorkerNodes
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ClusterId,MonitorNode,HostNodes,Cookie,ControllerNodes,WorkerNodes}||
	{?RECORD,ClusterId,MonitorNode,HostNodes,Cookie,ControllerNodes,WorkerNodes}<-Z].

read(Key)->
    Return=case read_all() of
	       []->
		   {error,[no_info,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{ClusterId,MonitorNode,HostNodes,Cookie,ControllerNodes,WorkerNodes}] ->
		   case Key of
		       cluster_id->
			   ClusterId;
		       monitor_node ->
			   MonitorNode;
		       host_nodes->
			   HostNodes;
		       cookie->
			   Cookie;
		       controller_nodes->
			   ControllerNodes;
		       worker_nodes->
			   WorkerNodes;
		       Err ->
			   {error,['type not defined',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

add(Node)->
    add(worker_nodes,Node).
add(Key,Node)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			NewRecord=case Key of
				      host_nodes->
					  Nodes=[Node|lists:delete(Node,S1#?RECORD.host_nodes)],
					  S1#?RECORD{host_nodes=Nodes};
				      controller_nodes->
					  Nodes=[Node|lists:delete(Node,S1#?RECORD.controller_nodes)],
					  S1#?RECORD{controller_nodes=Nodes};
				      worker_nodes->
					  Nodes=[Node|lists:delete(Node,S1#?RECORD.worker_nodes)],
					  S1#?RECORD{worker_nodes=Nodes};
				      Err ->
					  {error,['type not defined',Err,?FUNCTION_NAME,?MODULE,?LINE]}
				  end,
			case NewRecord of
			    {error,_Err}->
				mnesia:abort(?TABLE);
			    NewRecord->
				mnesia:write(NewRecord)
			end
		end
	end,
    mnesia:transaction(F).

remove(Node)->
    remove(worker_nodes,Node).
remove(Key,Node)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			NewRecord=case Key of
				      host_nodes->
					  Nodes=lists:delete(Node,S1#?RECORD.host_nodes),
					  S1#?RECORD{host_nodes=Nodes};
				      controller_nodes->
					  Nodes=lists:delete(Node,S1#?RECORD.controller_nodes),
					  S1#?RECORD{controller_nodes=Nodes};
				      worker_nodes->
					  Nodes=lists:delete(Node,S1#?RECORD.worker_nodes),
					  S1#?RECORD{worker_nodes=Nodes};
				      Err ->
					  {error,['type not defined',Err,?FUNCTION_NAME,?MODULE,?LINE]}
				  end,
			case NewRecord of
			    {error,_Err}->
				mnesia:abort(?TABLE);
			    NewRecord->
				mnesia:write(NewRecord)
			end
		end
	end,
    mnesia:transaction(F).
    
set(Key,Value)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			NewRecord=case Key of
				      cluster_id->
					  S1#?RECORD{cluster_id=Value};
				      monitor_node ->
					  S1#?RECORD{monitor_node=Value};
				      host_nodes->
					  S1#?RECORD{host_nodes=Value};
				      cookie->
					  S1#?RECORD{cookie=Value};
				      controller_nodes->
					  S1#?RECORD{controller_nodes=Value};
				      worker_nodes->
					  S1#?RECORD{worker_nodes=Value};
				      Err ->
					  {error,['type not defined',Err,?FUNCTION_NAME,?MODULE,?LINE]}
				  end,
			case NewRecord of
			    {error,_Err}->
				mnesia:abort(?TABLE);
			    NewRecord->
				mnesia:write(NewRecord)
			end
		end
	end,
    mnesia:transaction(F).
 
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
