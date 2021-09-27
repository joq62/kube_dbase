%% This is the application resource file (.app file) for the 'base'
%% application.
{application, dbase_dist,
[{description, "Resource discovery" },
{vsn, "0.1.0" },
{modules, 
	  [dbase_dist,dbase_dist_sup,dbase_dist_server]},
{registered,[dbase_dist]},
{applications, [kernel,stdlib]},
{mod, {dbase_dist,[]}},
{start_phases, []},
{git_path,"https://github.com/joq62/dbase_dist.git"},
{env,[{connect_nodes,['c0@c0','c2@c2',
	              'joq62-X550CA@joq62-X550CA',
		      'test@joq62-X550CA']}]}
]}.
