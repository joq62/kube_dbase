%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(etcd_test).   
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-include_lib("eunit/include/eunit.hrl").
%% --------------------------------------------------------------------

-define(GitHostConfigCmd,"git clone https://github.com/joq62/host_config.git").
-define(HostFile,"host_config/hosts.config").
-define(HostConfigDir,"host_config").


%% External exports
-export([start/0]). 


%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    io:format("~p~n",[{"Start setup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=setup(),
    io:format("~p~n",[{"Stop setup",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_0(),
%    io:format("~p~n",[{"Stop pass_0()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_1(),
%    io:format("~p~n",[{"Stop pass_1()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_2(),
%    io:format("~p~n",[{"Stop pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_3(),
%    io:format("~p~n",[{"Stop pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pass_4(),
    io:format("~p~n",[{"Stop pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pass_5(),
    io:format("~p~n",[{"Stop pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
 
    
   
      %% End application tests
    io:format("~p~n",[{"Start cleanup",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=cleanup(),
    io:format("~p~n",[{"Stop cleaup",?MODULE,?FUNCTION_NAME,?LINE}]),
   
    io:format("------>"++atom_to_list(?MODULE)++" ENDED SUCCESSFUL ---------"),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_0()->
    true=etcd:is_leader(),
    timer:sleep(1),
    false=etcd:is_leader(),
    timer:sleep(30*1000),
    true=etcd:is_leader(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_5()->
    {ok,HostId}=net:gethostname(),
    {ok,S1}=slave:start(HostId,s1,"-setcookie abc"),
    {ok,S2}=slave:start(HostId,s2,"-setcookie abc"),
    [etcd_lib:add_node(Vm,ram_disc)||Vm<-nodes()],
    
    S1Tables=[{catalog,ram_copies},{host_info,ram_copies}],
    S2Tables=[{cluster_info,ram_copies},{host_info,ram_copies}],
    R1=[etcd_lib:add_table(S1,Table,StorageType)||{Table,StorageType}<-S1Tables],
    [etcd_lib:add_table(S2,Table,StorageType)||{Table,StorageType}<-S2Tables],
    
 
%    io:format("~p~n",[mnesia:system_info()]),
 %   io:format("~p~n",[nodes()]),

    %Add nodes
    ["joq62-X550CA","c2","c1","c0"]=mnesia:dirty_all_keys(host_info),
    ["joq62-X550CA","c2","c1","c0"]=rpc:call(S1,mnesia,dirty_all_keys,[host_info]),
    %Stop slave 
    slave:stop(S1),
    {badrpc,nodedown}=rpc:call(S1,mnesia,dirty_all_keys,[host_info]),
    % Restart slave
    {ok,S1}=slave:start(HostId,s1,"-setcookie abc"),
    {badrpc,{'EXIT',{aborted,{no_exists,host_info}}}}=rpc:call(S1,mnesia,dirty_all_keys,[host_info]),
    etcd_lib:add_node(S1,ram_disc),
    timer:sleep(2000),
    ["joq62-X550CA","c2","c1","c0"]=rpc:call(S1,mnesia,dirty_all_keys,[host_info]),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_3()->
    [{"c0",_,22,"joq62","festum01"},
     {"c0",_,22,"joq62","festum01"}]=db_host_info:read("c0"),
    {atomic,[ok]}=db_host_info:delete("c0","192.168.0.200",22,"joq62","festum01"),
    [{"c0","192.168.1.200",22,"joq62","festum01"}]=db_host_info:read("c0"),

    {atomic,ok}=etcd:cluster_info_create("glurk_cluster","glurk_cookie"),
    "glurk_cluster"=etcd:cluster_name(),
    "glurk_cookie"=etcd:cluster_cookie(),

    [{"c2","192.168.0.202",22,"joq62","festum01"},
     {"c2","192.168.1.202",22,"joq62","festum01"},
     {"c1","192.168.0.201",22,"joq62","festum01"},
     {"c1","192.168.1.201",22,"joq62","festum01"},
     {"c0","192.168.1.200",22,"joq62","festum01"},
     {"joq62-X550CA","192.168.0.100",22,"joq62","festum01"},
     {"joq62-X550CA","192.168.1.50",22,"joq62","festum01"}
    ]=db_host_info:read_all(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_4()->
    [controller_leader]=db_lock:read_all(),
    [{"test_1",2,["c0","c1"],"test_1_cookie",[],false},
     {"production",3,["c0","c1"],"production_cookie",[],false}
    ]=db_cluster_info:read_all(),
    
    [{"c2","192.168.0.202",22,"joq62","festum01"},
     {"c2","192.168.1.202",22,"joq62","festum01"},
     {"c1","192.168.0.201",22,"joq62","festum01"},
     {"c1","192.168.1.201",22,"joq62","festum01"},
     {"c0","192.168.0.200",22,"joq62","festum01"},
     {"c0","192.168.1.200",22,"joq62","festum01"},
     {"joq62-X550CA","192.168.0.100",22,"joq62","festum01"},
     {"joq62-X550CA","192.168.1.50",22,"joq62","festum01"}
    ]=db_host_info:read_all(),
    
    [{"controller","1.0.0","https://github.com/joq62/controller.git"},
     {"etcd","1.0.0","https://github.com/joq62/etcd.git"},
     {"support","1.0.0","https://github.com/joq62/support.git"}
    ]=db_catalog:read_all(),
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->
    ok=cluster_lib:load_config(?HostConfigDir,?HostFile,?GitHostConfigCmd),
    {ok,HostInfoConfig}=cluster_lib:read_config(?HostFile),
    [etcd:host_info_create(HostId,Ip,SshPort,UId,Pwd)||
	    [{host_id,HostId},
	     {ip,Ip},
	     {ssh_port,SshPort},
	     {uid,UId},
	     {pwd,Pwd}]<-HostInfoConfig],

    [{"c0",_,22,"joq62","festum01"},
     {"c0",_,22,"joq62","festum01"}]=etcd:host_info_read("c0"),
    {atomic,[ok]}=etcd:host_info_delete("c0","192.168.0.200",22,"joq62","festum01"),
    [{"c0","192.168.1.200",22,"joq62","festum01"}]=etcd:host_info_read("c0"),
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_1()->
    ok=cluster_lib:load_config(?HostConfigDir,?HostFile,?GitHostConfigCmd),
    {ok,HostInfoConfig}=cluster_lib:read_config(?HostFile),
    [[{host_id,"joq62-X550CA"},
      {ip,"192.168.0.100"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"joq62-X550CA"},
      {ip,"192.168.1.50"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"c0"},
      {ip,"192.168.0.200"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"c0"},
      {ip,"192.168.1.200"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"c1"},
      {ip,"192.168.0.201"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"c1"},
      {ip,"192.168.1.201"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"c2"},
      {ip,"192.168.0.202"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}],
     [{host_id,"c2"},
      {ip,"192.168.1.202"},
      {ssh_port,22},
      {uid,"joq62"},
      {pwd,"festum01"}]]=HostInfoConfig,

    %------- etcd test
    [db_host_info:create(HostId,Ip,SshPort,UId,Pwd)||
	    [{host_id,HostId},
	     {ip,Ip},
	     {ssh_port,SshPort},
	     {uid,UId},
	     {pwd,Pwd}]<-HostInfoConfig],

    [{"c0",_,22,"joq62","festum01"},
     {"c0",_,22,"joq62","festum01"}]=db_host_info:read("c0"),
    {atomic,[ok]}=db_host_info:delete("c0","192.168.0.200",22,"joq62","festum01"),
    [{"c0","192.168.1.200",22,"joq62","festum01"}]=db_host_info:read("c0"),
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_11()->
    [{ok,[[{host_id,"c0"},
	  {ip,"192.168.0.200"},
	  {ssh_port,22},
	  {uid,"joq62"},
	  {pwd,"festum01"}],
	 [{host_id,"joq62-X550CA"},
	  {ip,"192.168.0.100"},
	  {ssh_port,22},
	  {uid,"joq62"},
	  {pwd,"festum01"}]]},
    {error,[[{host_id,"c1"},
	     {ip,"192.168.0.201"},
	     {ssh_port,22},
	     {uid,"joq62"},
	     {pwd,"festum01"}],
	    [{host_id,"c2"},
	     {ip,"192.168.0.202"},
	     {ssh_port,22},
	     {uid,"joq62"},
	     {pwd,"festum01"}]]}]=cluster:install(),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
-define(APP,etcd).
setup()->
    rpc:call(node(),application,stop,[?APP],10*5000),
    timer:sleep(500),
    application:set_env([{?APP,[{is_leader,true}]}]),
    ok=rpc:call(node(),application,start,[?APP],10*5000),
    {pong,_,?APP}=rpc:call(node(),?APP,ping,[],1*5000),	
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
    application:stop(etcd),
  %  init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
