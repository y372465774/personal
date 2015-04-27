-module(publish).
-include_lib("amqp_client/include/amqp_client.hrl").
%%-include("amqp_client.hrl").
-compile([export_all]).

publish() ->
    %%  establish a network connection
    {ok,Connection} = amqp_connection:start(#amqp_params_network{}),
    %% Open a channel on the connection
    {ok,Channel} = amqp_connection:open_channel(Connection),
    %% declare a queue
    #'queue.declare_ok'{queue = Q}
        = amqp_channel:call(Channel,#'queue.declare'{queue = <<"first_queue">>}),
    %% the message you want to publish
   Payload = <<"hello world!">>,
    %% publish with cast .... asynchronous
   Publish = #'basic.publish'{exchange = <<>>,routing_key = Q},
   amqp_channel:cast(Channel,Publish,#amqp_msg{payload = Payload}),

   %%
   %%
   %% do something you like !
   %%
   %%
   %% close channel and connection 
   amqp_channel:close(Channel),
   amqp_connection:close(Connection),

   ok.
