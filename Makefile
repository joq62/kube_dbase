all:
#	service
	rm -rf ebin/*;
	erlc -I ../interfaces -o ebin src/*.erl;
	rm -rf src/*.beam *.beam  test_src/*.beam test_ebin;
	rm -rf  glurk logs *~ */*~  erl_cra*;
	echo Done
doc_gen:
	echo glurk not implemented
unit_test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config deployment *.log;
	mkdir test_ebin;
#	interface
	erlc -I ../interfaces -o test_ebin ../interfaces/*.erl;
#	support
	cp ../applications/support/src/*.app test_ebin;
	erlc -I ../interfaces -o test_ebin ../kube_support/src/*.erl;
	erlc -I ../interfaces -o test_ebin ../applications/support/src/*.erl;
#	etcd
	cp ../applications/etcd/src/*.app ebin;
	erlc -I ../interfaces -o ebin ../kube_dbase/src/*.erl;
	erlc -I ../interfaces -o ebin ../applications/etcd/src/*.erl;
#	kubelet
	cp ../applications/kubelet/src/*.app test_ebin;
	erlc -I ../interfaces -o test_ebin ../node/src/*.erl;
	erlc -I ../interfaces -o test_ebin ../applications/kubelet/src/*.erl;
#	test application
	cp test_src/*.app test_ebin;
	erlc -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
	    -setcookie etcd_test_cookie\
	    -sname etcd_test\
	    -unit_test cluster_id glurk\
	    -unit_test monitor_node etcd_test\
	    -run unit_test start_test test_src/test.config
