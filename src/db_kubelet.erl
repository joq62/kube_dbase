-module(db_kubelet).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,kubelet_info).
-define(RECORD,kubelet_info).
-record(kubelet_info,{
		      id,
		      host_id,
		      cluster_id,
		      pod,
		      dir,
		      kubelet_pod,
		      cookie,
		      containers
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

create(PodId,HostId,ClusterId,Pod,Dir,KubeletPod,Cookie,Containers)->
    Record=#?RECORD{
		    id=PodId,
		    host_id=HostId,
		    cluster_id=ClusterId,
		    pod=Pod,
		    dir=Dir,
		    kubelet_pod=KubeletPod,
		    cookie=Cookie,
		    containers=Containers
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{PodId,HostId,ClusterId,Pod,Dir,KubeletPod,Cookie,Containers}||
	{?RECORD,PodId,HostId,ClusterId,Pod,Dir,KubeletPod,Cookie,Containers}<-Z].

member(WantedPodId,WantedHostId,WantedClusterId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result= case [PodId||{?RECORD,PodId,HostId,ClusterId,_,_,_,_,_}<-Z,
			 WantedPodId==PodId,
			 WantedHostId==HostId,
			 WantedClusterId==ClusterId] of
		[]->
			    false;
		_->
		    true
	    end,
    Result.

containers(Pod)->
    read_pod(Pod,containers).


read_pod(Pod,Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.pod==Pod])),
    Result=case Z of
	       []->
		   {error,[eexist,Pod,?FUNCTION_NAME,?MODULE,?LINE]}; 
	       _->
		   extract(Z,Key)
	   end,
    Result.
read_id(Id,HostId,ClusterId,Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.id==Id,
		     X#?RECORD.host_id==HostId,
		     X#?RECORD.cluster_id==ClusterId])),
    Result=case Z of
	       []->
		   {error,[eexist,Id,HostId,ClusterId,Key,?FUNCTION_NAME,?MODULE,?LINE]}; 
	       _->
		   extract(Z,Key)
	   end,
    Result.
extract(R,Key)->
    Result=case Key of
	       id->R#?RECORD.id;
	       host_id->R#?RECORD.host_id;
	       cluster_id->R#?RECORD.cluster_id;
	       pod->R#?RECORD.pod;
	       dir->R#?RECORD.dir;
	       kubelet_pod->R#?RECORD.kubelet_pod;
	       cookie->R#?RECORD.cookie;
	       containers->R#?RECORD.containers;
	       Err ->{error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
	   end,
    Result.
    
    
add_container(Pod,NewContainer)->
 F = fun() -> 
	     RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.pod==Pod])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     NewContainerList=[NewContainer|lists:delete(NewContainer,S1#?RECORD.containers)],
		     NewRecord=S1#?RECORD{containers=NewContainerList},
		     mnesia:delete_object(S1),
		     mnesia:write(NewRecord)
	     end
		 
     end,
    mnesia:transaction(F).

delete_container(Pod,DelContainer)->
 F = fun() -> 
	     RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.pod==Pod])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     NewContainerList=lists:delete(DelContainer,S1#?RECORD.containers),
		     NewRecord=S1#?RECORD{containers=NewContainerList},
		     mnesia:delete_object(S1),
		     mnesia:write(NewRecord)
	     end
		 
     end,
    mnesia:transaction(F).

delete(Pod)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.pod==Pod])),
		case RecordList of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1)
		end
		    
	end,
    mnesia:transaction(F).
    
 
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
