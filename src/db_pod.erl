-module(db_pod).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,pod_status).
-define(RECORD,pod_status).
-record(pod_status,{
		    pod_id,
		    pod_node,
		    pod_dir,
		    app_state, %[{app,loaded|started,PodSpec}]
		    host_node,
		    created
		   }).
% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(PodId,PodNode,PodDir,AppState,HostNode,Created)->
    Record=#?RECORD{
		    pod_node=PodNode,
		    pod_id=PodId,
		    pod_dir=PodDir,
		    app_state=AppState, 
		    host_node=HostNode,
		    created=Created
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

delete(PodNode)->
    F=fun()->
		   case do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.pod_node==PodNode])) of
		       []->
			   mnesia:abort({error,[ticket,"eexist",[PodNode]]});
		       [R]->  
			   mnesia:delete_object(R)
		   end
      end,
    mnesia:transaction(F).
    
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{PodId,PodNode,PodDir,AppState,HostNode,Created}||
	{?RECORD,PodId,PodNode,PodDir,AppState,HostNode,Created}<-Z].

%% 
dir(PodNode)->
    read(PodNode,pod_dir).
app_state(PodNode)->
    read(PodNode,app_state).
host_node(PodNode)->
    read(PodNode,host_node).
created(PodNode)->
    read(PodNode,created).

read(PodNode,Key)->
    Result=case do(qlc:q([X || X <- mnesia:table(?TABLE),
			       X#?RECORD.pod_node==PodNode])) of
	       []->
		   {error,[ticket,"eexist",[PodNode]]};
	       [R]->
		   
		   case Key of
		       pod_id->
			   R#?RECORD.pod_id;
		       pod_node->
			   R#?RECORD.pod_node;
		       pod_dir->
			   R#?RECORD.pod_dir;
		       app_state->
			   R#?RECORD.app_state;
		       host_node->
			   R#?RECORD.host_node;
		       created->
			   R#?RECORD.created;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Result.

add_app(PodNode,App,Stated,PodSpec)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.pod_node==PodNode])),
		case RecordList of
		    []->
			%_Result={error,[ticket,"eexist",[PodNode]]},
			mnesia:abort(?TABLE);
		    [S1]->
			case lists:keymember(App,1,S1#?RECORD.app_state) of
			    true->
			%	_Result={error,[ticket,"exists",[App,PodNode,PodSpec]]},
				mnesia:abort(?TABLE);				
			    false->
				AppState=[{App,Stated,PodSpec}|S1#?RECORD.app_state],
				NewRecord=S1#?RECORD{app_state=AppState},
			%	_Result={ok,[AppState]},
				
				mnesia:delete_object(S1),
				mnesia:write(NewRecord)
			end
		end
	end,
    mnesia:transaction(F).

remove_app(PodNode,App)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.pod_node==PodNode])),
		case RecordList of
		    []->
			%_Result={error,[ticket,"eexist",[PodNode]]},
			mnesia:abort(?TABLE);
		    [S1]->
			AppState=lists:keydelete(App,1,S1#?RECORD.app_state),
			NewRecord=S1#?RECORD{app_state=AppState},
			%_Result={ok,[AppState]},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).
    
 
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
