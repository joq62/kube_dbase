%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(etcd_lib).  
    
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterConfigPath,"https://github.com/joq62/cluster_config.git").
-define(ClusterConfigDirName,"cluster_config").
-define(ClusterConfigFile,"cluster_config/cluster.config").
-define(ClusterConfigFileName,"cluster.config").

-define(HostConfigPath,"https://github.com/joq62/host_config.git").
-define(HostConfigDirName,"host_config").
-define(HostConfigFile,"host_config/hosts.config").
-define(HostConfigFileName,"hosts.config").

-define(PodSpecsPath,"https://github.com/joq62/pod_specs.git").
-define(PodSpecsDirName,"pod_specs").
%% --------------------------------------------------------------------


%% External exports
-export([init/0,
	 add_node/2,
	 add_table/3
	]).

-define(WAIT_FOR_TABLES,5000).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

init()->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    ok=init_cluster_info(),
    ok=init_host_info(),
    ok=init_pod_specs(),
    ok.
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_cluster_info()->
    {ok,ClusterIdAtom}=application:get_env(cluster_id),
    ClusterId=atom_to_list(ClusterIdAtom),
    ClusterConfigDir=filename:join(ClusterId,?ClusterConfigDirName),
    os:cmd("rm -rf "++ClusterConfigDir),
    os:cmd("git clone "++?ClusterConfigPath),
    os:cmd("mv "++?ClusterConfigDirName++" "++ClusterConfigDir),
    ClusterConfigFile=filename:join([ClusterId,?ClusterConfigDirName,?ClusterConfigFileName]),
    {ok,Info}=file:consult(ClusterConfigFile),
    ok=db_cluster_info:create_table(),
    ok=init_cluster_info(Info,[]),
    ok.
init_cluster_info([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
    
init_cluster_info([[{cluster_name,ClusterId},{controller_host,ControllerHost},{worker_hosts,NumWorkers,WorkerHosts},{cookie,Cookie}]|T],Acc)->
    ControllerNode=[],
    R=db_cluster_info:create(ClusterId,ControllerHost,NumWorkers,WorkerHosts,Cookie,ControllerNode),
    init_cluster_info(T,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_host_info()->
  {ok,ClusterIdAtom}=application:get_env(cluster_id),
    ClusterId=atom_to_list(ClusterIdAtom),
    HostConfigDir=filename:join(ClusterId,?HostConfigDirName),
    os:cmd("rm -rf "++HostConfigDir),
    os:cmd("git clone "++?HostConfigPath),
    os:cmd("mv "++?HostConfigDirName++" "++HostConfigDir),
    HostConfigFile=filename:join([ClusterId,?HostConfigDirName,?HostConfigFileName]),
    {ok,Info}=file:consult(HostConfigFile),
   % io:format("~p~n",[{Debug,?MODULE,?LINE}]),
    {ok,Info}=file:consult(HostConfigFile),
    ok=db_host_info:create_table(),
    ok=init_host_info(Info,[]),
    ok.
init_host_info([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
    
init_host_info([[{alias,Alias},{host_id,HostId},{ip,Ip},{ssh_port,SshPort},{uid,UId},{pwd,Pwd}]|T],Acc)->
    R=db_host_info:create(Alias,HostId,Ip,SshPort,UId,Pwd),
    init_host_info(T,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
init_pod_specs()->
    {ok,ClusterIdAtom}=application:get_env(cluster_id),
    ClusterId=atom_to_list(ClusterIdAtom),
    os:cmd("git clone "++?PodSpecsPath),
    PodSpecDir=filename:join([ClusterId,?PodSpecsDirName]),
    os:cmd("mv "++?PodSpecsDirName++" "++PodSpecDir),

    ok=db_pod_spec:create_table(),
    {ok,FileNames}=file:list_dir(PodSpecDir),
    PodSpecFiles=[filename:join([PodSpecDir,FileName])||FileName<-FileNames,
							 filename:extension(FileName)==".pod_spec"],
    ok=init_pod_specs(PodSpecFiles,[]),
      
    ok.

init_pod_specs([],Result)->
    R=[R||R<-Result,
	  R/={atomic,ok}],
    case R of
	[]->
	    ok;
	R->
	    {error,[R]}
    end;
init_pod_specs([PodSpecFile|T],Acc)->
    {ok,Info}=file:consult(PodSpecFile),
    [{pod_id,PodId},{pod_vsn,PodVsn},{application,{AppId,AppVsn,AppGitPath}},{app_env,AppEnv},{app_hosts,AppHosts}]=Info,
    R=db_pod_spec:create(PodId,PodVsn,AppId,AppVsn,AppGitPath,AppEnv,AppHosts),
    init_pod_specs(T,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------  




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
add_node(Vm,StorageType)->
    Result=case net_adm:ping(Vm) of
	       pong->
		   stopped=rpc:call(Vm,mnesia,stop,[]),
		  % ok=rpc:call(Vm,mnesia,delete_schema,[[Vm]]),
		   mnesia:delete_schema([Vm]),
		   ok=rpc:call(Vm,mnesia,start,[]),
		   {ok,[Vm]}=mnesia:change_config(extra_db_nodes, [Vm]),
		   mnesia:add_table_copy(schema,Vm,StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES);
	       pang ->
		   {error,[not_running,Vm]}
	   end,    
    Result.


%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

add_table(Vm,Table,StorageType)->
    mnesia:add_table_copy(Table,Vm,StorageType),
    Tables=mnesia:system_info(tables),
    mnesia:wait_for_tables(Tables,?WAIT_FOR_TABLES).
    
%% --------------------------------------------------------------------
%% Function:start
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------

