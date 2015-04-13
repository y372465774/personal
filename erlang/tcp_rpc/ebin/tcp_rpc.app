%% -*- mode: Erlang; file-column: 75 ; comment-column: 50; -*-
{	application,
	tcp_rpc,
	[	{description,"RPC server for Erlang and OTP in action"},
		{vsn,"0.1.0"},
		{module,[tr_app,tr_sup,tr_server]},
		{registered,[tr_sup]},
		{application,[kernel,stdlib]},
		{mod,{tr_app,[]}}
	]
}.
