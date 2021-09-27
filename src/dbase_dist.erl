%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(dbase_dist).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([

	]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------


%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, _StartArgs) ->
    ok=init(),
    {ok,Pid}= dbase_dist_sup:start_link(),
    {ok,Pid}.
   
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
init()->
    io:format("node() ~p~n",[node()]),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    Nodes=[node()|nodes()],
    ok=case [Node||Node<-Nodes,rpc:call(Node,db_lock,check_init,[],2000)=:=ok] of
	   []-> % First Node
	       ok=db_lock:create_table(),
	       {atomic,ok}=db_lock:create(controller_lock,1,node()),
	       true=db_lock:is_open(controller_lock,node(),2),
	       true=db_lock:is_leader(controller_lock,node()),
	       ok;
	   ControllerNodes->
	       add_this_node(ControllerNodes,false)
       end.
    
	    

add_this_node([],Result)->
    Result;
add_this_node(_,ok) ->
    ok;
add_this_node([Node1|T],_Acc)->
    NewAcc=case rpc:call(Node1,db_lock,add_node,[node(),ram_copies],5000) of
	       {badrpc,_}->
		   false;
	       ok->
		   ok;
	       _Error->
		   false
	   end,
    
add_this_node(T,NewAcc).	    
