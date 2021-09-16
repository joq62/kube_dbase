-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE, deployment).
-define(RECORD,deployment).
-record(deployment,{
		    deployment_spec,
		    vsn,
		    node,
		    node_dir,
		    app_id,
		    host_id,
		    cluster_id,
		    status
		   
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

create(DeploymentSpec,Vsn,Node,Dir,AppId,HostId,ClusterId,Status)->
    
    Record=#?RECORD{
		    deployment_spec=DeploymentSpec,
		    vsn=Vsn,
		    node=Node,
		    node_dir=Dir,
		    app_id=AppId,
		    host_id=HostId,
		    cluster_id=ClusterId,
		    status=Status
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{DeploymentSpec,Vsn,Node,Dir,AppId,HostId,ClusterId,Status}||{?RECORD,DeploymentSpec,Vsn,Node,Dir,AppId,HostId,ClusterId,Status}<-Z].
read(DepSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.deployment_spec==DepSpec])),
    Result=case Z of
	       []->
		   {error,[eexist,DepSpec,?FUNCTION_NAME,?MODULE,?LINE]};
	       _->
		   [{DeploymentSpec,Vsn,Node,Dir,AppId,HostId,ClusterId,Statuss}||{?RECORD,DeploymentSpec,Vsn,Node,Dir,AppId,HostId,ClusterId,Statuss}<-Z]
	   end,
    Result.

vsn(DepSpec)->
    read(DepSpec,vsn).
app(DepSpec)->
    read(DepSpec,app_id).
pod_info(DepSpec)->
    read(DepSpec,node_info).
node(DepSpec)->
    read(DepSpec,node).
dir(DepSpec)->
    read(DepSpec,dir).
host(DepSpec)->
    read(DepSpec,host_id).
cluster(DepSpec)->
    read(DepSpec,cluster_id).
status(DepSpec)->
    read(DepSpec,status).

read(DepSpec,Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.deployment_spec==DepSpec])),
    Result=case Z of
	       []->
		   {error,[eexist,DepSpec,?FUNCTION_NAME,?MODULE,?LINE]}; 
	       _->
		   case Key of
		       vsn->
			   [Vsn|_]=[R#?RECORD.vsn||R<-Z],
			   Vsn;
		       node->[R#?RECORD.node||R<-Z];
		       dir->[R#?RECORD.node_dir||R<-Z];
		       app_id->[R#?RECORD.app_id||R<-Z];
		       host_id->[R#?RECORD.host_id||R<-Z];
		       cluster_id->[R#?RECORD.cluster_id||R<-Z];
		       status->[R#?RECORD.status||R<-Z];
		       node_info->
			   [{R#?RECORD.node,R#?RECORD.node_dir,R#?RECORD.host_id}||R<-Z];
		       Err ->{error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Result.

delete(DepSpec) ->
    F = fun() -> 
		ToBeRemoved=[X||X<-mnesia:read({?TABLE,DepSpec}),
				X#?RECORD.deployment_spec==DepSpec],
		case ToBeRemoved of
		    []->
			mnesia:abort({error,[eexists, DepSpec]});
		    ToBeRemoved ->
			[mnesia:delete_object(Deployment)||Deployment<-ToBeRemoved]
		end 
	end,
    mnesia:transaction(F).

delete(DepSpec,AppId) ->
    F = fun() -> 
		ToBeRemoved=[X||X<-mnesia:read({?TABLE,DepSpec}),
				X#?RECORD.deployment_spec==DepSpec,
			    	X#?RECORD.app_id==AppId],
		case ToBeRemoved of
		    []->
			mnesia:abort({error,[eexists, DepSpec,AppId]});
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
