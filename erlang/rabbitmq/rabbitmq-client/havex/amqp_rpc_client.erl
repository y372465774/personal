-module(amqp_rpc_client).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile([export_all]).

publish(Channel) ->
    
    %%
    %%发送 请求
    %%
    Payload = <<"body">>,%%% 消息体
    Publish = #'basic.publish'{exchange = <<"request">>,routing_key = <<"test">>},
    Props = #'P_basic'{correlation_id = <<"123456">>},%% 关联id
    amqp_channel:cast(Channel,Publish,#amqp_msg{props = Props,
                                                payload = Payload}),
    ok.

consume(Channel,RPC_Response) ->
    
    %%%
    %%% 从约定的队列中取数据 ，理论上是要根据 correlation-id判断信息是否属于自己的响应
    %%%
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
            timer:sleep(100),
            consume(Channel,RPC_Response)
    end.

start() ->
    %%
    %% 建立和Rabbitmq服务器的连接
    %%
    {ok,Connection} = amqp_connection:start(#amqp_params_network{
                                            host = "192.168.115.132"}),
    
    %%
    %%建立通道
    %%
    {ok,Channel} = amqp_connection:open_channel(Connection),
    
    %%
    %%建立 exchange
    %%
    Ex1 = #'exchange.declare'{exchange = <<"request">>,
                             type = <<"direct">>}, 
    #'exchange.declare_ok'{} = amqp_channel:call(Channel,Ex1),
    
    %%
    %%建立队列
    %%
    TQ1 = #'queue.declare'{queue = <<"request_queue">> , auto_delete = true},
    #'queue.declare_ok'{queue = RPC_Request}
            = amqp_channel:call(Channel,TQ1),
    %%
    %% 将exchange 和 队列绑定
    %%
    Bind1 = #'queue.bind'{queue = RPC_Request,
                            exchange = <<"request">>,
                            routing_key = <<"test">> 
                           },
    #'queue.bind_ok'{} = amqp_channel:call(Channel,Bind1),
    


    Ex2 = #'exchange.declare'{exchange = <<"response">> , 
                             type = <<"direct">>}, 
    #'exchange.declare_ok'{} = amqp_channel:call(Channel,Ex2),

    TQ2 = #'queue.declare'{queue = <<"response_queue">>},
    #'queue.declare_ok'{queue = RPC_Response}
            = amqp_channel:call(Channel,TQ2),
    io:format("Response : ~p ~n",[RPC_Response]), 
    Bind2 = #'queue.bind'{queue = RPC_Response,
                            exchange = <<"response">>,
                            routing_key = <<"test">>
                            },
    #'queue.bind_ok'{} = amqp_channel:call(Channel,Bind2),
    
    %%
    %%发送 请求
    %%
    publish(Channel),
    
    %%
    %%接受响应
    %%
    consume(Channel,RPC_Response),
    
    %% 关闭连接 释放资源
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    
    ok.
