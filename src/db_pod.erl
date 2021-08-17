-module(db_pod).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,pod_status).
-define(RECORD,pod_status).
-record(pod_status,{
		    reference,
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

create(Reference,PodNode,PodDir,PodSpecs,HostNode,Created)->
    Record=#?RECORD{
		    reference=Reference,
		    pod_node=PodNode,
		    pod_dir=PodDir,
		    pod_specs=PodSpecs, 
		    host_node=HostNode,
		    created=Created
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

delete(Reference)->
    F=fun()->
		   case do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.reference==Reference])) of
		       []->
			   mnesia:abort({error,[ticket,"eexist",[Reference]]});
		       [R]->  
			   mnesia:delete_object(R)
		   end
      end,
    mnesia:transaction(F).
    
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Reference,PodNode,PodDir,PodSpecs,HostNode,Created}||
	{?RECORD,Reference,PodNode,PodDir,PodSpecs,HostNode,Created}<-Z].

%% 
read(WantedReference)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		     X#?RECORD.reference==WantedReference])),
    [{Reference,PodNode,PodDir,PodSpecs,HostNode,Created}||
	{?RECORD,Reference,PodNode,PodDir,PodSpecs,HostNode,Created}<-Z].
    
node(Reference)->
    read(Reference,pod_node).
dir(Reference)->
    read(Reference,pod_dir).
pod_specs(Reference)->
    read(Reference,pod_specs).
host_node(Reference)->
    read(Reference,host_node).
created(Reference)->
    read(Reference,created).

read(Reference,Key)->
    Result=case do(qlc:q([X || X <- mnesia:table(?TABLE),
			       X#?RECORD.reference==Reference])) of
	       []->
		   {error,[ticket,"eexist",[Reference]]};
	       [R]->
		   
		   case Key of
		       reference->
			   R#?RECORD.reference;
		       pod_node->
			   R#?RECORD.pod_node;
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

add_spec(Reference,PodSpec)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.reference==Reference])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Reference,RecordList]]});
		    [S1]->
			UpdatedPodSpecs=[PodSpec|lists:delete(PodSpec,S1#?RECORD.pod_specs)],
			NewRecord=S1#?RECORD{pod_specs=UpdatedPodSpecs},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).

remove_spec(Reference,PodSpec)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					   X#?RECORD.reference==Reference])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Reference,RecordList]]});
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
