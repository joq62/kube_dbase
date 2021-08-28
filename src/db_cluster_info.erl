-module(db_cluster_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,cluster_info).
-define(RECORD,cluster_info).
-record(cluster_info,{
		      cluster_id,
		      monitor_node,
		      cookie
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

create(ClusterId,MonitorNode,Cookie)->
    Record=#?RECORD{
		    cluster_id=ClusterId,
		    monitor_node=MonitorNode,
		    cookie=Cookie
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ClusterId,MonitorNode,Cookie}||
	{?RECORD,ClusterId,MonitorNode,Cookie}<-Z].

cluster()->
    read(cluster_id).
monitor()->
    read(monitor_node).
cookie()->
    read(cookie).

read(Key)->
    Return=case read() of
	       []->
		   {error,[eexist,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{ClusterId,MonitorNode,Cookie}] ->
		   case  Key of
		       cluster_id->ClusterId;
		       monitor_node->MonitorNode;
		       cookie->Cookie;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.
read()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ClusterId,MonitorNode,Cookie}||{?RECORD,ClusterId,MonitorNode,Cookie}<-Z].

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
