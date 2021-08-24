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
		  pod_status,
		  container_status,
		  pod,
		  dir
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
    ContainerStatus=[{AppId,AppVsn,unloaded}||{AppId,AppVsn,_GitPath,_AppEnv}<-Containers],
    PodStatus=not_scheduled,
    Pod=not_started,
    Dir=not_defined,
    Record=#?RECORD{
		    name=Name,
		    vsn=Vsn,
		    containers=Containers,
		    wanted_hosts=WantedHosts,
		    pod_status=PodStatus,
		    container_status=ContainerStatus,
		    pod=Pod,
		    dir=Dir
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Name,Vsn,Containers,WantedHosts,PodStatus,ContainerStatus,Pod,Dir}||{?RECORD,Name,Vsn,Containers,WantedHosts,PodStatus,ContainerStatus,Pod,Dir}<-Z].

read(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    [{XName,Vsn,Containers,WantedHosts,PodStatus,ContainerStatus,Pod,Dir}||{?RECORD,XName,Vsn,Containers,WantedHosts,PodStatus,ContainerStatus,Pod,Dir}<-Z].

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
pod(Name)->
    read(Name,pod).
dir(Name)->
    read(Name,dir).
read(Name,Key)->
    Return=case read(Name) of
	       []->
		   {error,[eexist,Name,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{_Name,Vsn,Containers,WantedHosts,PodStatus,ContainerStatus,Pod,Dir}] ->
		   case  Key of
		       vsn->
			   Vsn;
		       containers->
			   Containers;
		       wanted_hosts->
			   WantedHosts;
		       pod_status->
			   PodStatus;
		       container_status->
			   ContainerStatus;
		       pod->
			   Pod;
		       dir->
			   Dir;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

set_pod(Name,Pod)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewRecord=S1#?RECORD{pod=Pod},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).
set_dir(Name,Dir)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewRecord=S1#?RECORD{dir=Dir},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).
update_pod_status(Name,NewStatus)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewRecord=S1#?RECORD{pod_status=NewStatus},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).
update_container_status(Name,{AppId,AppVsn,NewStatus})->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewContainerStatus=lists:keyreplace(AppId, 1, S1#?RECORD.container_status, {AppId,AppVsn,NewStatus}),
			NewRecord=S1#?RECORD{container_status=NewContainerStatus},
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
