-module(db_catalog).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-define(TABLE,catalog).
-define(RECORD,catalog).
-record(catalog,{
		 application,
		 vsn,
		 git_path
		   	      
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

create(Application,Vsn,GitPath)->
    Record=#?RECORD{
		    application=Application,
		    vsn=Vsn,
		    git_path=GitPath
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Application,Vsn,GitPath}||{?RECORD,Application,Vsn,GitPath}<-Z].

info(Application)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.application==Application])),
    [{XApplication,Vsn,GitPath}||{?RECORD,XApplication,Vsn,GitPath}<-Z].

gitpath(Application)->
     case info(Application) of
	[]->
	    {error,[eexist,Application]};
	 [{Application,_Vsn,GitPath}]->
	    GitPath
    end.

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------
