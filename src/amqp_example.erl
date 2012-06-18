-module(amqp_example).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile([export_all]).

test() ->
    %% Start a network connection (
    Params = #amqp_params_network{
      username = <<"guest">>,
      password = <<"guest">>,
      virtual_host = <<"/">>,
      host = "localhost",
      port = 5672
     },
    {ok, Connection} = amqp_connection:start(Params),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),

    %% Declare a queue
    Queue = <<"demo">>,
    Declare = #'queue.declare'{queue = Queue, durable = true},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),

    %% Publish a message
    Payload = <<"foobar">>,
    Publish = #'basic.publish'{exchange = <<>>, routing_key = Queue},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),

    %% Get the message back from the queue
    Get = #'basic.get'{queue = Queue},
    {#'basic.get_ok'{delivery_tag = Tag}, Content}
        = amqp_channel:call(Channel, Get),

    %% Do something with the message payload
    io:format("got message tag: ~p, content: ~p~n", [Tag, Content]),

    %% Ack the message
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),

    ok.
