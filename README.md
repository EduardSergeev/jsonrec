Jsonrec
=======
[![Build Status](https://travis-ci.org/EduardSergeev/jsonrec.png?branch=dev)](https://travis-ci.org/EduardSergeev/jsonrec)

Overview
--------
Jsonrec is a yet another JSON encode/decode library written in Erlang. Even though one cannot say there is a lack of JSON libraries written in Erlang none of existing ones, to my knowledge, provides direct, seamless mapping between Erlang records and JSON (while jsonrec does).
This library uses compile-time code generation to produce JSON encoding and decoding code based on record type annotations.

How it works
----------------
Given Erlang record definition with type annotation (fields can be of any JSON-compatible types including user-defined types, lists and nested records of arbitrary depth) jsonrec produces the body for "encode" and "decode" function (with a help of [meta](https://github.com/EduardSergeev/meta) library which in tern uses Erlang [parse_transform/2 function](http://www.erlang.org/doc/man/erl_id_trans.html) for source manipulation). The resulting functions can then be used as normal Erlang function.

Benefits of jsonrec
-------------------
* Resulting functions consume and produce Erlang records which is much more convenient and safer (bug-free) to use then proplists or other weakly-typed structures (Erlang compiler and, optionally, Dialyzer can detects bugs and discrepancies at compile time)
* Encoding/decoding functions are tailored in compile-time using type annotations so the resulting code can be much more efficient (in comparison to generic JSON parser/generator).
In fact [initial tests](https://github.com/EduardSergeev/erlbench/tree/latest) show that jsonrec is in majority cases faster then any existing purely Erlang-based JSON library,
for both [encoding](https://raw.github.com/EduardSergeev/erlbench/latest/results/JSON/encode.png) and [decoding](https://raw.github.com/EduardSergeev/erlbench/latest/results/JSON/decode.png).
C-based parsers, like `ejson`, are still understandably faster but this may change once critical parsers code is rewriten using NIF.

Quickstart examples
-------------------
###### Note: the code below can be found in [example/readme.erl](https://github.com/EduardSergeev/jsonrec/blob/dev/example/readme.erl) file.
To use jsonrec simply add the following header:
 
    -include_lib("jsonrec/include/jsonrec.hrl").

Then, lets say we have the following set of records which we want to generate serialization code for:

    -type country() :: 'AU' | 'RU' | 'UK' | 'US'.
    -record(address,
            {line1 :: string(),
             line2 = "" :: string(),
             line3 = "" :: string(),
             country :: country(),
             zip :: string()}).
    
    -type phone_kind() :: work | home | mobile.
    -record(phone,
            {kind = mobile :: phone_kind(),
             number :: string(),
             is_prefered :: boolean()}).
    
    -record(person,
            {id :: integer(),
             first_name :: string(),
             last_name :: string(),
             address = unknown :: #address{},
             phones = [] :: [#phone{}]}).

Then JSON encoding function for `#person{}` can be coded with the following line:

    encode(#person{} = Rec) ->
        ?encode_gen(#person{}, Rec).

While JSON decoding function (from `binary()` input into `#person{}` record):

    decode(Bin) ->
        ?decode_gen(#person{}, Bin).

Now we can test if it works as expected:

    1> rr(readme).
    [address,person,phone]
    2> A = #address{line1 = "John Smith", line2 = "Elm Street", country = 'US'},                          
    2> Ph1 = #phone{number = "123456", kind = home},                                                      
    2> Ph2 = #phone{number = "0404123456", is_prefered = true},                                           
    2> Rec = #person{id = 42, first_name = "John", last_name = "Smith", address = A, phones = [Ph1, Ph2]}.
    #person{id = 42,first_name = "John",last_name = "Smith",
            address = #address{line1 = "John Smith",
                               line2 = "Elm Street",line3 = [],country = 'US',
                               zip = undefined},
            phones = [#phone{kind = home,number = "123456",
                             is_prefered = undefined},
                      #phone{kind = mobile,number = "0404123456",
                             is_prefered = true}]}
    3> IoList = readme:encode(Rec),                                                                       
    3> io:format("~s~n", [IoList]).                                                                       
    {"id":42,"first_name":"John","last_name":"Smith","address":{"line1":"John Smith","line2":"Elm Street","line3":"","country":"US"},"phones":[{"kind":"home","number":"123456"},{"kind":"mobile","number":"0404123456","is_prefered":true}]}
    ok
    4> Bin = list_to_binary(IoList),                                                                      
    4> {ok, Restored} = readme:decode(Bin).
    {ok,#person{id = 42,first_name = "John",last_name = "Smith",
                address = #address{line1 = "John Smith",
                                   line2 = "Elm Street",line3 = [],country = 'US',
                                   zip = undefined},
                phones = [#phone{kind = home,number = "123456",
                                 is_prefered = undefined},
                          #phone{kind = mobile,number = "0404123456",
                                 is_prefered = true}]}}
    5> Rec == Restored.                    
    true 

Decoding function is quite flexible: fields can be present in JSON in a different order or some can be omitted (in which case either `undefined` or "default" value is set in the resulting record):

    6> readme:decode(<<"{}">>).                                                                           
    {ok,#person{id = undefined,first_name = undefined,
                last_name = undefined,address = unknown,phones = []}}

While attempt to pass invalid JSON will result in parsing error:

    7> readme:decode(<<"}">>). 
    {error,{expected,<<"{">>,at,<<"}">>}}

Supported types
---------------
* The following standard types are currently supported (corresponding JSON types mapping is also given):
  * `boolean()` <-> `true` | `false`
  * `integer()` <-> `number`
  * `float()`   <-> `number`
  * `binary()`  <-> `string`
  * `string()`  <-> `string`
  * `atom()`    <-> `string`
  * `undefined` <-> omitted field or `null`
* User defined types (in the form `-type` *some_type()* :: *type_def*)
* `list(Type)` <-> `array` where Type can be any supported type
* Union of types (*some_type()* | *another_type()* | *more_types*) Note: these types can be problematic especially for decoding (even though `binary() | integer()` is not a problem, how can you decode `#rec1{} | #rec2{}` type?) - such cases may require some manual coding (see example below)
* `record` <-> `object` - where every field of the record is mapped according to its type. Default values specified in records are also handled. `undefined` value of any field currently results in field being omitted in generated JSON.
* `any()` (or missing type annotation) - transparent type mapping: the value of the field simply inserted into generated JSON (so it better be of `iolist()` type) on encoding while `binary()` part of the corresponding JSON input is assigned to this field on decoding without any processing. This type can be used if custom decoding/encoding is required (See `decode_options()` type description below).

Detailed library description
----------------------------
`TODO`

Current state of the library
----------------------------
As of today the library is in "experimental" stage: it was not thoroughly tested and is still in active development.
