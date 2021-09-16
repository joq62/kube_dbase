-module(db_host_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,host_info).
-define(RECORD,host_info).
-record(host_info,{
		   host_id,
		   ip,
		   ssh_port,
		   uid,
		   pwd
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

create(HostId,Ip,SshPort,UId,Pwd)->
    Record=#?RECORD{
		    host_id=HostId,
		    ip=Ip,
		    ssh_port=SshPort,
		    uid=UId,
		    pwd=Pwd
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(HostId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.host_id==HostId])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

ssh_info(WantedHost)->
    read(WantedHost,ssh_info).
read(WantedHost,Key)->
    Return=case read(WantedHost) of
	       []->
		   {error,[eexist,WantedHost,?FUNCTION_NAME,?MODULE,?LINE]};
	       [{HostId,Ip,SshPort,UId,Pwd}] ->
		   case  Key of
		       ssh_info->
			   {Ip,SshPort,UId,Pwd};
		       Err ->
			   {error,['Key eexists',Err,?FUNCTION_NAME,?MODULE,?LINE]}
		   end
	   end,
    Return.

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{HostId,Ip,SshPort,UId,Pwd}||{?RECORD,Alias,HostId,Ip,SshPort,UId,Pwd}<-Z].

read(Alias)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.alias==Alias])),
    [{HostId,Ip,SshPort,UId,Pwd}||{?RECORD,XAlias,HostId,Ip,SshPort,UId,Pwd}<-Z].

delete(HostId) ->
    F = fun() -> 
		mnesia:delete({?TABLE,HostInfo})
		    
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
