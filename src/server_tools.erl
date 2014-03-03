-module(server_tools).

-export([get_vsn/2]).
-export([check_vsn/3, check_vsn/1, check_vsn/0]).

-define(CONST_BASE_VSN_NODE, 'sanguo_4399_center@127.0.0.1').



%% @doc get a node module's vsn
get_vsn(Module, Node) ->
    case catch rpc:call(Node, Module, module_info, [attributes]) of
    	Att when is_list(Att) ->
    		case lists:keyfind(vsn, 1, Att) of
    			{vsn, Vsn} ->
    				Vsn;
    			_ ->
    				-1
    		end;
    	_ ->
    		-1
    end.

check_vsn() ->
	DefaultModule = center_api,
	check_vsn(DefaultModule).

check_vsn(Module) ->
	BaseVsn = get_base_vsn(Module),
	Nodes = get_all_center_node(),
	case check_vsn(Module, Nodes, BaseVsn) of
		false ->
			lager:error("check failed! by Module ~p, BaseVsn ~p", [Module, BaseVsn]);
		true ->
			lager:info("check success, by Module ~p, BaseVsn ~p", [Module, BaseVsn])
	end.

%% @doc check nodes's module vsns is same
check_vsn(_Module, [], _BaseVsn) ->
	true;
check_vsn(Module, [Node, RestNodes], BaseVsn) ->
	case get_vsn(Module, Node) of
		BaseVsn ->
			check_vsn(Module, RestNodes, BaseVsn);
		OtherVsn ->
			lager:error("Node ~p's Vsn is ~p, check failed!", [Node, OtherVsn]),
			false
	end.

%% @doc private get base vsn 
get_base_vsn(Module) ->
	BaseNode = ?CONST_BASE_VSN_NODE,
	case get_vsn(Module, BaseNode) of
		-1 ->
			lager:error("get base vsn error !"),
			throw("get base vsn error !");
		Vsn ->
			Vsn
	end.

%% get all center node by rpc call from base node
get_all_center_node() ->
	BaseNode = ?CONST_BASE_VSN_NODE,
	CenterInfoList = rpc:call(BaseNode, data_misc, get_platform_list, []),
	Fun =
		fun({_, IpBinary, NameBinary}) ->
			Ip = binary_to_list(IpBinary),
			Name = binary_to_list(NameBinary),
			string:join(["sanguo_", Name, "_center@", Ip], "")
		end,
	lists:map(Fun, CenterInfoList).







    
