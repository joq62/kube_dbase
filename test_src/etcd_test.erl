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

    io:format("~p~n",[{"Start pod_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pod_spec(),
    io:format("~p~n",[{"Stop pod_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start pod()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=pod(),
    io:format("~p~n",[{"Stop pod()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start deployment_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=deployment_spec(),
    io:format("~p~n",[{"Stop deployment_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start deployment()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=deployment(),
    io:format("~p~n",[{"Stop deployment()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_2(),
%    io:format("~p~n",[{"Stop pass_2()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_3(),
%    io:format("~p~n",[{"Stop pass_3()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_4(),
%    io:format("~p~n",[{"Stop pass_4()",?MODULE,?FUNCTION_NAME,?LINE}]),

%    io:format("~p~n",[{"Start pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
%    ok=pass_5(),
%    io:format("~p~n",[{"Stop pass_5()",?MODULE,?FUNCTION_NAME,?LINE}]),
 
    
   
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
deployment_spec()->
    
    [{"mymath.pod_spec","c0","lgh"}]=db_deployment_spec:read_all(),
    
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
deployment()->
    
   % create(Reference,App,DeploymentSpec,PodNode,HostId,ClusterId,Created)
    {atomic,ok}=db_deployment:create(ref1,app1,depspec1,podnode1,host1,cluster1,date1),
    {atomic,ok}=db_deployment:create(ref2,app2,depspec2,podnode2,host2,cluster2,date2),	
    {atomic,ok}=db_deployment:create(ref3,app1,depspec3,podnode1,host1,cluster1,date3),
    
    % Normal cases
    app1=db_deployment:app(ref1),
    depspec1=db_deployment:deployment(ref1),
    podnode1=db_deployment:pod(ref1),
    host1=db_deployment:host(ref1),
    cluster1=db_deployment:cluster(ref1),
    date3=db_deployment:created(ref3),

    app1=db_deployment:app(ref3),
   
    
    %
    {atomic,_}=db_deployment:delete(ref1),
    {error,_}=db_deployment:app(ref1),
    %
    app1=db_deployment:app(ref3),
  
    io:format("~p~n",[db_pod:read_all()]),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pod_spec()->
    [{"mymath_lgh","1.0.0","mymath","1.0.0",
      "https://github.com/joq62/mymath.git",[],
      [{"c2_lgh","c2"}]},
     {"kubelet","1.0.0","kubelet","1.0.0",
      "https://github.com/joq62/kubelet.git",[],[]},
     {"balcony_lgh","1.0.0","balcony","1.0.0",
      "https://github.com/joq62/balcony.git",
      "port 8080",
      [{"c0_lgh","c0"}]},
     {"controller","1.0.0","controller","1.0.0",
      "https://github.com/joq62/controller.git",[],
      []},
     {"iaas","1.0.0","iaas","1.0.0",
      "https://github.com/joq62/iaas.git",[],[]},
     {"balcony","1.0.0","balcony","1.0.0",
      "https://github.com/joq62/balcony.git",[],
      [{"c1_varmdo","c1"}]},
     {"etcd","1.0.0","etcd","1.0.0",
      "https://github.com/joq62/etcd.git",[],[]},
     {"orginal","1.0.0","orginal","1.0.0", 
      "https://github.com/joq62/orginal.git",[],[]},
     {"mymath","1.0.0","mymath","1.0.0",
      "https://github.com/joq62/mymath.git",[],
      [{"asus_varmdo","joq62-X550CA"}]}]=db_pod_spec:read_all(),
   
    ok.

   %
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pod()->
   %
 %   db_pod:create(PodId,PodNode,PodDir,AppState,HostNode,Created)
    {atomic,ok}=db_pod:create(id1,node1,dir1,[],host1,date1),
    {atomic,ok}=db_pod:create(id2,node2,dir2,[],host1,date2),	
    {atomic,ok}=db_pod:create(id3,node3,dir3,[],host2,date2),
    
    % Normal cases
    dir1=db_pod:dir(node1),
    host2=db_pod:host_node(node3),
    []=db_pod:app_state(node2),
    %
    {atomic,ok}=db_pod:delete(node1),
    %
    {atomic,ok}=db_pod:add_app(node2,app1,loaded,spec1),
    [{app1,loaded,spec1}]=db_pod:app_state(node2),
    
    {atomic,ok}=db_pod:add_app(node2,app2,started,spec2),
    [{app2,started,spec2},{app1,loaded,spec1}]=db_pod:app_state(node2),
    
    {atomic,ok}=db_pod:remove_app(node2,app2),
    [{app1,loaded,spec1}]=db_pod:app_state(node2),

    io:format("~p~n",[db_pod:read_all()]),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_2()->
    
     ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_1()->
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
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pass_4()->
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

setup()->
   ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------    

cleanup()->
  
  %  application:stop(etcd),
  %  init:stop(),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
