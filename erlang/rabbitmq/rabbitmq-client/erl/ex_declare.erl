-module (ex_declare).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile([export_all]).

start() ->
    {ok,Connection} = amqp_connection:start(#amqp_params_network{
                                            host = "192.168.115.135"}),

    {ok,Channel} = amqp_connection:open_channel(Connection),
    
    exchange_declare(Channel,<<"backup.hstore.exchange">>,<<"topic">>),
    exchange_declare(Channel,<<"cbh.hstore.exchange">>,<<"topic">>),
    exchange_declare(Channel,<<"cbs.teller.broadcast">>,<<"fanout">>),
    exchange_declare(Channel,<<"efs.hstore.exchange">>,<<"topic">>),
    exchange_declare(Channel,<<"hbase1.hstore.exchange">>,<<"topic">>),
    exchange_declare(Channel,<<"hbase2.hstore.exchange">>,<<"topic">>),

    amqp_channel:close(Channel),
    amqp_connection:close(Connection),

    ok.

exchange_declare(Channel,Exchange,Type) ->
    Ex = #'exchange.declare'{exchange = Exchange,
                            type = Type,
                            durable = true,
                            auto_delete = false,
                            internal = false},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel,Ex).
    

