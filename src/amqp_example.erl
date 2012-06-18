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

    %% Declare topic exchange
    Exchange = <<"topic_exchange">>,
    #'exchange.declare_ok'{} = amqp_channel:call(
                                Channel,
                                #'exchange.declare'{
                                  exchange = Exchange,
                                  type = <<"topic">>,
                                  durable = true}),

    %% Declare a queue
    Queue = <<"demo">>,
    #'queue.declare_ok'{} = amqp_channel:call(
                              Channel,
                              #'queue.declare'{
                                queue = Queue,
                                durable = true}),

    %% Bind queue to exchange
    %% queue will receive all messages with routing key starting with "prefix."
    #'queue.bind_ok'{} = amqp_channel:call(
                           Channel,
                           #'queue.bind'{
                             queue = Queue,
                             exchange = Exchange,
                             routing_key = <<"prefix.#">>}),

    %% Publish message with non-matching routing key
    amqp_channel:cast(
      Channel,
      #'basic.publish'{
        exchange = Exchange,
        routing_key = <<"random.routing.key">>},
      #amqp_msg{payload = <<"qwerty">>}),

    %% Publish message with matching routing key
    amqp_channel:cast(
      Channel,
      #'basic.publish'{
        exchange = Exchange,
        routing_key = <<"prefix.suffix">>},
      #amqp_msg{
        payload = <<"foobar">>,
        props = #'P_basic'{
        delivery_mode = 2     % persistent message
       }
      }), 

    %% Get the message back from the queue (poll)
    Get = #'basic.get'{queue = Queue},
    {#'basic.get_ok'{delivery_tag = Tag}, Content}
        = amqp_channel:call(Channel, Get),

    %% Do something with the message payload
    io:format("got message tag: ~p, content: ~p~n", [Tag, Content]),

    %% Ack the message
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),

    %% message with non-matching routing key was discarded
    #'basic.get_empty'{} = amqp_channel:call(Channel, Get),

    %% there is a method #'basic.consume' which allows to subscribe to a queue
    %% instead of polling

    %% Close the channel
    amqp_channel:close(Channel),
    %% Close the connection
    amqp_connection:close(Connection),

    ok.
