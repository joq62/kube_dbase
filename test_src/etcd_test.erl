%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description :  1
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

    io:format("~p~n",[{"Start cluster()",?MODULE,?FUNCTION_NAME,?LINE}]),
 %   ok=cluster(),
    io:format("~p~n",[{"Stop cluster()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start pod_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pod_spec(),
    io:format("~p~n",[{"Stop pod_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start pod()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=pod(),
    io:format("~p~n",[{"Stop pod()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start deployment_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),
  %  ok=deployment_spec(),
    io:format("~p~n",[{"Stop deployment_spec()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start deployment()",?MODULE,?FUNCTION_NAME,?LINE}]),
   % ok=deployment(),
    io:format("~p~n",[{"Stop deployment()",?MODULE,?FUNCTION_NAME,?LINE}]),

    io:format("~p~n",[{"Start kubelet()",?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=kubelet(),
    io:format("~p~n",[{"Stop kubelet",?MODULE,?FUNCTION_NAME,?LINE}]),

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
kubelet()->
    Id1=integer_to_list(1),
    H1=h1,
    C1=c1,
    Pod1=pod1,
    Dir1=dir1,
    KP1=kp1,
    Cookie1=cookie1,
    Containers1=[cont1,cont2,cont3],
    
    false=db_kubelet:member(Id1,H1,C1),
    {atomic,ok}=db_kubelet:create(Id1,H1,C1,Pod1,Dir1,KP1,Cookie1,Containers1),
    true=db_kubelet:member(Id1,H1,C1),
    
    [{"1",h1,c1,pod1,dir1,kp1,cookie1,
      [cont1,cont2,cont3]}]=db_kubelet:kubelet_pod_info(kp1),
    
    [cont1,cont2,cont3]=db_kubelet:containers(Pod1),
    {atomic,ok}==db_kubelet:add_container(Pod1,cont_added),
    [cont_added,cont1,cont2,cont3]=db_kubelet:containers(Pod1),
    {atomic,ok}==db_kubelet:delete_container(Pod1,cont2),
    [cont_added,cont1,cont3]=db_kubelet:containers(Pod1),
    
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
cluster()->
    %ClusterId,MonitorNode,HostNodes,Cookie,ControllerNodes,WorkerNodes
   [{"lgh",
     [{"c0_lgh","c0"},
      {"c2_lgh","c2"},
      {"asus_lgh","joq62-X550CA"}],
     "lgh_cookie",not_started},
    {"stage",
     [{"c0_lgh","c0"},{"c2_lgh","c2"}],
     "stage_cookie",not_started}]=db_cluster_spec:read_all(),
    
    [{"c0_lgh","c0"},
     {"c2_lgh","c2"},
     {"asus_lgh","joq62-X550CA"}]=db_cluster_spec:hosts("lgh"),

    "lgh_cookie"=db_cluster_spec:cookie("lgh"),

    not_started=db_cluster_spec:status("lgh"),
    {atomic,ok}=db_cluster_spec:set_status("lgh",running),
    running=db_cluster_spec:status("lgh"),
    

    
    ok.


    

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
deployment_spec()->
   [{"dep_1","1.0.0",[{"mymath","1.0.0",3}],"lgh"},
    {"dep_2","1.0.0",[{"mymath","1.0.0",1}],"lgh"},
    {"dep_10","1.0.0",[{"mymath","1.0.0",2}],"stage"}]=db_deployment_spec:read_all(),

    [{"dep_1","1.0.0",[{"mymath","1.0.0",3}],"lgh"}]=db_deployment_spec:read("dep_1"),
    [{"dep_2","1.0.0",[{"mymath","1.0.0",1}],"lgh"}]=db_deployment_spec:read("dep_2"),
    {error,_}=db_deployment_spec:read("glurk"),
    
    "1.0.0"=db_deployment_spec:vsn("dep_1"),
    "1.0.0"=db_deployment_spec:vsn("dep_1"),
    {error,_}=db_deployment_spec:vsn("glurk"),

    [{"mymath","1.0.0",3}]=db_deployment_spec:pods("dep_1"),
    [{"mymath","1.0.0",1}]=db_deployment_spec:pods("dep_2"),

    [{"dep_1","1.0.0",[{"mymath","1.0.0",3}],"lgh"},
     {"dep_2","1.0.0",[{"mymath","1.0.0",1}],"lgh"}]=db_deployment_spec:key_cluster_id("lgh"),
    [{"dep_10","1.0.0",[{"mymath","1.0.0",2}],"stage"}]=db_deployment_spec:key_cluster_id("stage"),
    

    "lgh"=db_deployment_spec:cluster_id("dep_1"),
    "lgh"=db_deployment_spec:cluster_id("dep_2"),

    
    
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
deployment()->
    
   % create(Reference,App,DeploymentSpec,PodNode,HostId,ClusterId,Created)
    {atomic,ok}=db_deployment:create(dep1,vsn1,pod1,dir1,host1,cluster10,running),
    {atomic,ok}=db_deployment:create(dep1,vsn1,pod2,dir2,host1,cluster10,not_scheduled),	
    {atomic,ok}=db_deployment:create(dep2,vsn1,pod3,dir3,host1,cluster10,running),
    

    % Normal cases
    [{dep2,vsn1,pod3,dir3,host1,cluster10,running},
     {dep1,vsn1,pod1,dir1,host1,cluster10,running},
     {dep1,vsn1,pod2,dir2,host1,cluster10,
      not_scheduled}]=db_deployment:read_all(),
   
    
    [{dep1,vsn1,pod1,dir1,host1,cluster10,running},
     {dep1,vsn1,pod2,dir2,host1,cluster10,not_scheduled}]=db_deployment:read(dep1),
    [{dep2,vsn1,pod3,dir3,host1,cluster10,running}]=db_deployment:read(dep2),
    {error,_}=db_deployment:read(glurk),
    


    vsn1=db_deployment:vsn(dep1),
    vsn1=db_deployment:vsn(dep2),
    {error,_}=db_deployment:vsn(glurk),

    [{pod1,dir1,host1},{pod2,dir2,host1}]=db_deployment:pod_info(dep1),
    [{pod3,dir3,host1}]=db_deployment:pod_info(dep2),

    [pod1,pod2]=db_deployment:pod(dep1),
    [pod3]=db_deployment:pod(dep2),

    [dir1,dir2]=db_deployment:dir(dep1),
    [dir3]=db_deployment:dir(dep2),

    [host1,host1]=db_deployment:host(dep1),
    [host1]=db_deployment:host(dep2),

    [cluster10,cluster10]=db_deployment:cluster(dep1),
    [cluster10]=db_deployment:cluster(dep2),

    [running,not_scheduled]=db_deployment:status(dep1),
    [running]=db_deployment:status(dep2),

       
    %
    {atomic,_}=db_deployment:delete(dep1),
    {error,_}=db_deployment:read(dep1),
    %
       ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pod_spec()->
    [{"kubelet","1.0.0",
      [{"kubelet","1.0.0",
	"https://github.com/joq62/kubelet.git",[]}],
      [],[]},
     {"balcony_lgh","1.0.0",
      [{"balcony","1.0.0",
	"https://github.com/joq62/balcony.git",[]}],
      [{"c0_lgh","c0"}],
      []},
     {"balcony_varmdo","1.0.0",
      [{"balcony","1.0.0",
	"https://github.com/joq62/balcony.git",[]}],
      [{"c1_varmdo","c1"}],
      []},
     {"controller","1.0.0",
      [{"controller","1.0.0",
	"https://github.com/joq62/controller.git",
	[]}],
      [],[]},
     {"iaas","1.0.0",
      [{"iaas","1.0.0",
	"https://github.com/joq62/iaas.git",[]}],
      [],[]},
     {"etcd","1.0.0",
      [{"etcd","1.0.0",
	"https://github.com/joq62/etcd.git",[]}],
      [],[]},
     {"mymath","1.0.0",
      [{"mymath","1.0.0",
	"https://github.com/joq62/mymath.git",[]}],
      [],[]}]=db_pod_spec:read_all(),
    
    
    [{"mymath","1.0.0",
      [{"mymath","1.0.0",
	"https://github.com/joq62/mymath.git",[]}],
      [],[]}]=db_pod_spec:read("mymath"),
    
    "1.0.0"=db_pod_spec:vsn("mymath"),
    [{"mymath","1.0.0",
      "https://github.com/joq62/mymath.git",[]}]=db_pod_spec:containers("mymath"),
    [{"c0_lgh","c0"}]=db_pod_spec:wanted_hosts("balcony_lgh"),
    []=db_pod_spec:deployment("mymath"),
    {atomic,ok}=db_pod_spec:add_deployment("mymath",pod1,dir1,host_id1,running,not_started),
    [{pod1,dir1,host_id1,running,not_started}]=db_pod_spec:deployment("mymath"),
    {atomic,ok}=db_pod_spec:update_deployment("mymath",pod1,dir1,host_id1,running,running),
    [{pod1,dir1,host_id1,running,running}]=db_pod_spec:deployment("mymath"), 

    {atomic,ok}=db_pod_spec:add_deployment("mymath",pod2,dir2,host_id2,not_running,not_started),
    [{pod2,dir2,host_id2,not_running,not_started},
    {pod1,dir1,host_id1,running,running}]=db_pod_spec:deployment("mymath"), 
    
    {pod1,dir1,host_id1,running,running}=db_pod_spec:deployment("mymath",pod1),
    []=db_pod_spec:deployment("mymath",glurk),
    
    {atomic,ok}=db_pod_spec:update_deployment("mymath",pod2,dir2,host_id2,glurk,running),
    {atomic,ok}=db_pod_spec:delete_deployment("mymath",pod1),
    {atomic,ok}=db_pod_spec:update_deployment("mymath",pod1,dir1,host_id1,running,running),
    [{pod2,dir2,host_id2,glurk,running}]=db_pod_spec:deployment("mymath"), 

    
    true=db_pod_spec:member("mymath"),
    false=db_pod_spec:member("glurk"),
    
    ok.

   %
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
pod()->
   %

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
