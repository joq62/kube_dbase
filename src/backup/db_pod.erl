-module(db_pod).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,pod_status).
-define(RECORD,pod_status).
-record(pod_status,{
		    pod_node,
		    pod_dir,
		    pod_specs, 
		    host_node,
		    created
		   }).
% Start Special 

% End Special 
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(PodNode,PodDir,PodSpecs,HostNode,Created)->
    Record=#?RECORD{
		    pod_node=PodNode,
		    pod_dir=PodDir,
		    pod_specs=PodSpecs, 
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
    [{PodNode,PodDir,PodSpecs,HostNode,Created}||
	{?RECORD,PodNode,PodDir,PodSpecs,HostNode,Created}<-Z].

%% 
read(WantedPodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.pod_node==WantedPodNode])),
    [{PodNode,PodDir,PodSpecs,HostNode,Created}||
	{?RECORD,PodNode,PodDir,PodSpecs,HostNode,Created}<-Z].
    

dir(PodNode)->
    read(PodNode,pod_dir).
pod_specs(PodNode)->
    read(PodNode,pod_specs).
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
		       pod_dir->
			   R#?RECORD.pod_dir;
		       pod_specs->
			   R#?RECORD.pod_specs;
		       host_node->
			   R#?RECORD.host_node;
		       created->
			   R#?RECORD.created;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Result.

add_spec(PodNode,PodSpec)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.pod_node==PodNode])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[PodNode,RecordList]]});
		    [S1]->
			UpdatedPodSpecs=[PodSpec|lists:delete(PodSpec,S1#?RECORD.pod_specs)],
			NewRecord=S1#?RECORD{pod_specs=UpdatedPodSpecs},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).

remove_spec(PodNode,PodSpec)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					   X#?RECORD.pod_node==PodNode])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[PodNode,RecordList]]});
		    [S1]->
			UpdatedPodSpecs=lists:delete(PodSpec,S1#?RECORD.pod_specs),
			NewRecord=S1#?RECORD{pod_specs=UpdatedPodSpecs},
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
