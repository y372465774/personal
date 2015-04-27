-module(amqp_rpc_client).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile([export_all]).

publish(Channel,RPC_Request,RPC_Response) ->
    
    Payload = <<"body">>,
    Publish = #'basic.publish'{exchange = <<>>,routing_key = RPC_Request},
    Props = #'P_basic'{reply_to = RPC_Response,correlation_id = <<"123456">>},
    amqp_channel:cast(Channel,Publish,#amqp_msg{props = Props,
                                                payload = Payload}),
    ok.

consume(Channel,RPC_Response) ->
    
    Get = #'basic.get'{queue = RPC_Response},
    %%%io:format("REspobse ~p ~n",[RPC_Response]), 
    %%{#'basic.get_ok'{delivery_tag =Tag},Content}
    %%    = amqp_channel:call(Channel,Get),
    case amqp_channel:call(Channel,Get) of
        {#'basic.get_ok'{delivery_tag = Tag},Content} ->
            io:format("Content~p ~n",[Content]),
            io:format("Payload ~p ~n",[Content#'amqp_msg'.payload]),
            amqp_channel:cast(Channel,#'basic.ack'{delivery_tag = Tag});
        
        {'basic.get_empty',<<>>} ->
            timer:sleep(200),
            consume(Channel,RPC_Response)
    end.
start() ->
    {ok,Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok,Channel} = amqp_connection:open_channel(Connection),
    
    #'queue.declare_ok'{queue = RPC_Request}
            = amqp_channel:call(Channel,#'queue.declare'{queue = <<"rpc_queue">>,auto_delete = true}),
    
    #'queue.declare_ok'{queue = RPC_Response}
            = amqp_channel:call(Channel,#'queue.declare'{}),
    publish(Channel,RPC_Request,RPC_Response),
    
    consume(Channel,RPC_Response),
    
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    
    ok.
