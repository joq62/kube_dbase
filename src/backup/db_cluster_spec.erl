-module(db_cluster_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,cluster_spec).
-define(RECORD,cluster_spec).
-record(cluster_spec,{
		      name,
		      hosts,
		      cookie,
		      status
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

create(Name,Hosts,Cookie)->
    Status=not_started,
    Record=#?RECORD{
		    name=Name,
		    hosts=Hosts,
		    cookie=Cookie,
		    status=Status
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Name,Hosts,Cookie,Status}||
	{?RECORD,Name,Hosts,Cookie,Status}<-Z].

hosts(Name)->
    read(Name,hosts).
cookie(Name)->
    read(Name,cookie).
status(Name)->
    read(Name,status).
    
read(Name,Key)->
    Return=case read(Name) of
	       []->
		   {error,[eexist,Name,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{Name,Hosts,Cookie,Status}] ->
		   case  Key of
		       hosts->
			   Hosts;
		       cookie->
			   Cookie;
		       status->
			   Status;
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

set_status(Name,NewStatus)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
					  X#?RECORD.name==Name])),
		case RecordList of
		    []->
			mnesia:abort({error,[ticket,"eexist",[Name,RecordList]]});
		    [S1]->
			NewRecord=S1#?RECORD{status=NewStatus},
			mnesia:delete_object(S1),
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).


read(Name)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.name==Name])),
    [{XName,Hosts,Cookie,Status}||
	       {?RECORD,XName,Hosts,Cookie,Status}<-Z].

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
