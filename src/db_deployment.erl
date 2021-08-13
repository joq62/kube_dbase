-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, deployment).
-define(RECORD,deployment).
-record(deployment,{
		    reference,
		    app,
		    deployment_spec,
		    pod_node,
		    host_id,
		    cluster_id,
		    created
		  }).

% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(Reference,App,DeploymentSpec,PodNode,HostId,ClusterId,Created)->
    
    Record=#?RECORD{
		    reference=Reference,
		    app=App,
		    deployment_spec=DeploymentSpec,
		    pod_node=PodNode,
		    host_id=HostId,
		    cluster_id=ClusterId,
		    created=Created
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Reference,App,DeploymentSpec,PodNode,HostId,ClusterId,Created}||{?RECORD,Reference,App,DeploymentSpec,PodNode,HostId,ClusterId,Created}<-Z].

app(Ref)->
    read(Ref,app).
deployment(Ref)->
    read(Ref,deployment_spec).
pod(Ref)->
    read(Ref,pod_node).
host(Ref)->
    read(Ref,host_id).
cluster(Ref)->
    read(Ref,cluster_id).
created(Ref)->
    read(Ref,created).


read(Ref,Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.reference==Ref])),
    Return=case Z of
	       []->
		   {error,["eexists",Ref,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{?RECORD,Reference,App,DeploymentSpec,PodNode,HostId,ClusterId,Created}] ->
		   case Key of
		       app->
			   App;
		       deployment_spec->
			   DeploymentSpec;
		       pod_node->
			   PodNode;
		       host_id->
			   HostId;
		       cluster_id->
			   ClusterId;
		       created->
			   Created;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

read(Ref)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.reference==Ref])),
    [{App,DeploymentSpec,PodNode,HostId,ClusterId,Created}||{?RECORD,_,App,DeploymentSpec,PodNode,HostId,ClusterId,Created}<-Z].

delete(Ref) ->
    F = fun() -> 
		ToBeRemoved=[X||X<-mnesia:read({?TABLE,Ref}),
				X#?RECORD.reference==Ref],
		case ToBeRemoved of
		    []->
			mnesia:abort({ticket,"error",[eexists, Ref]});
		    ToBeRemoved ->
			[mnesia:delete_object(Deployment)||Deployment<-ToBeRemoved]
		end 
	end,
    mnesia:transaction(F).
 

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
