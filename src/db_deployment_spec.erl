-module(db_deployment_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, deployment_info).
-define(RECORD,deployment_info).
-record(deployment_info,{
			 pod_id,
			 host_id,
			 cluster_id
			}).

% Start Special 

% End Special 
create_table()->
    Result={atomic,ok}=mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000),
    Result.

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(PodId,HostId,ClusterId)->
    Record=#?RECORD{
		    pod_id=PodId,
		    host_id=HostId,
		    cluster_id=ClusterId
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{PodId,HostId,ClusterId}||{?RECORD,PodId,HostId,ClusterId}<-Z].

hosts(WantedPodId,WantedClusterId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [HostId||{?RECORD,PodId,HostId,ClusterId}<-Z,
	     WantedPodId==PodId,
	     WantedClusterId==ClusterId].

read(PodId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.pod_id==PodId])),
    [{HostId,ClusterId}||{?RECORD,_,HostId,ClusterId}<-Z].

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
