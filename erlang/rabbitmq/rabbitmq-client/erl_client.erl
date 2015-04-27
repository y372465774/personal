-module(erl_client).

-include_lib("amqp_client/include/amqp_client.hrl").
%%%-include("amqp_client.hrl").
%%%-export([link/0]).

-compile([export_all]).

print(Key,Value) ->
    io:format("~p -> ~p ~n",[Key,Value]).

test() ->
    %% 建立一个网络连接
    {ok,Connection} = amqp_connection:start(#amqp_params_network{}),
    print("connection",Connection),
    %% 建立在连接上建立一个 通道
    {ok,Channel} = amqp_connection:open_channel(Connection),
    print("channel",Channel),

    %% 声明一个队列
    #'queue.declare_ok'{queue = Q}
        = amqp_channel:call(Channel,#'queue.declare'{queue = <<"first_queue">>}),
    print("Q",Q),

    %% 发布一条消息到rabbitmq服务器
    PayLoad  = <<"foobar">>,
    Publish = #'basic.publish'{exchange = <<>>,routing_key = Q},
    amqp_channel:cast(Channel,Publish,#amqp_msg{payload = PayLoad}),
    print("ok","OK_send"),

    %%==============================
    %% 从ribbitmq服务器的队列中中获取消息
    Get = #'basic.get'{queue=Q},
    {#'basic.get_ok'{delivery_tag = Tag},Content}
        = amqp_channel:call(Channel,Get),
    
    print("Get",Get),
    print("Content",Content),

    %% 发送确认消息，告诉服务器我已经接收到消息了
    amqp_channel:cast(Channel,#'basic.ack'{delivery_tag = Tag}),
    print("ok","OK_receive"),

    %%关闭通道
    amqp_channel:close(Channel),
    %%关闭和服务器的连接
    amqp_connection:close(Connection),

    ok.


