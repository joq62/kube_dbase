-module(db_pod_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,pod_spec). 
-define(RECORD,pod_spec).

-record(pod_spec,{
		  name,
		  vsn,
		  containers,
		  wanted_hosts,
		  deployment_list
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

create(Name,Vsn,Containers,WantedHosts)->
    Record=#?RECORD{
		    name=Name,
		    vsn=Vsn,
		    containers=Containers,
		    wanted_hosts=WantedHosts,
		    deployment_list=[]
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Name,Vsn,Containers,WantedHosts,DeploymentList}||{?RECORD,Name,Vsn,Containers,WantedHosts,DeploymentList}<-Z].

read(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    [{XName,Vsn,Containers,WantedHosts,DeploymentList}||{?RECORD,XName,Vsn,Containers,WantedHosts,DeploymentList}<-Z].

member(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

vsn(Name)->
    read(Name,vsn).
containers(Name)->
    read(Name,containers).
wanted_hosts(Name)->
    read(Name,wanted_hosts).

pod_status(Name)->
    read(Name,pod_status).
container_status(Name)->
    read(Name,container_status).
deployment(Name)->
    read(Name,deployment).
read(Name,Key)->
    Return=case read(Name) of
	       []->
		   {error,[eexist,Name,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{_Name,Vsn,Containers,WantedHosts,DeploymentList}] ->
		   case  Key of
		       vsn->
			   Vsn;
		       containers->
			   Containers;
		       wanted_hosts->
			   WantedHosts;
		       deployment->
			   DeploymentList;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

add_deployment(Name,Pod,Dir,PodStatus,ContainerStatus)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewDeployment={Pod,Dir,PodStatus,ContainerStatus},
			NewDeploymentList=[NewDeployment|lists:keydelete(Pod, 1, S1#?RECORD.deployment_list)],
			NewRecord=S1#?RECORD{deployment_list=NewDeploymentList},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
      end,
    mnesia:transaction(F).

update_deployment(Name,Pod,Dir,PodStatus,ContainerStatus)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewDeployment={Pod,Dir,PodStatus,ContainerStatus},
			NewDeploymentList=lists:keyreplace(Pod, 1,S1#?RECORD.deployment_list,NewDeployment),
			NewRecord=S1#?RECORD{deployment_list=NewDeploymentList},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).
delete_deployment(Name,Pod)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewDeploymentList=lists:keydelete(Pod, 1,S1#?RECORD.deployment_list),
			NewRecord=S1#?RECORD{deployment_list=NewDeploymentList},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).
    
delete(Name) ->
    F = fun() -> 
		ToBeRemoved=[X||X<-mnesia:read({?TABLE,Name}),
				X#?RECORD.name=:=Name],
		case ToBeRemoved of
		    []->
			mnesia:abort(no_to_remove);
		    ToBeRemoved ->
			[mnesia:delete_object(PodSpec)||PodSpec<-ToBeRemoved]
		end 
	end,
    mnesia:transaction(F).
 

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
