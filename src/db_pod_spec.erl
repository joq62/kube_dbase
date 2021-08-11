-module(db_pod_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,pod_info). 
-define(RECORD,pod_info).
-record(pod_info,{ 
		   pod_id,
		   pod_vsn,
		   app_id,
		   app_vsn,
		   app_git_path,
		   app_env,
		   app_hosts
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

create(PodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts)->
    Record=#?RECORD{
		    pod_id=PodId,
		    pod_vsn=PodVsn,
		    app_id=AppId,
		    app_vsn=AppVsn,
		    app_git_path=AppGitPath,
		    app_env=AppEnv,
		    app_hosts=AppHosts
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{PodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts}||{?RECORD,PodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts}<-Z].

read(PodId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.pod_id==PodId])),
    [{XPodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts}||{?RECORD,XPodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts}<-Z].
hosts(PodId)->
    read(PodId,app_hosts).
app_env(PodId)->
    read(PodId,app_env).
app_id(PodId)->
    read(PodId,app_id).
git_path(PodId)->
    read(PodId,app_git_path).
read(PodId,Key)->
    Return=case read(PodId) of
	       []->
		   {error,[eexist,PodId,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{_PodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts}] ->
		   case  Key of
		       pod_vsn->
			   PodVsn;
		       app_id ->
			   AppId;
		       app_vsn->
			   AppVsn;
		       app_git_path->
			   AppGitPath;
		       app_env->
			   AppEnv;
		       app_hosts->
			   AppHosts;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.
delete(PodId) ->
    F = fun() -> 
		ToBeRemoved=[X||X<-mnesia:read({?TABLE,PodId}),
				X#?RECORD.pod_id=:=PodId],
		case ToBeRemoved of
		    []->
			mnesia:abort(no_to_remove);
		    ToBeRemoved ->
			[mnesia:delete_object(HostInfo)||HostInfo<-ToBeRemoved]
		end 
	end,
    mnesia:transaction(F).
 

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
