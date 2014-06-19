-record(cs,
  {
    conn_info, % {ip, port}
    sent_count = 0,
    recv_count = 0,
    seq = 0,
    myack = 0,
    unacks = []
  }).

-define(ACKLENGTH, 32).
-define(PACKET, <<Version:8, Seq:16, Seen:16, Ack:?ACKLENGTH, Cmd:8, Arg/binary>>).
-define(ACKHIGHBIT, (1 bsl ?ACKLENGTH)).
