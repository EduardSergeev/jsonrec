-module(astdemo).

-compile({parse_transform, mapfold_trans}).

-type mytype() :: atom() | {atom(), atom()}.

-type name() :: binary().

-type two() :: atom_a | atom_b.

-type mylist() :: [two()].

-type orddict(Key, Val) :: [{Key, Val}].

-record(myrecord,
	{field1 = 42 :: integer(),
	 field2 = foo :: mytype()}).

-json(myrecord).
