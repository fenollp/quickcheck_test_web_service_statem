-module(mylib).

-ifdef(TEST).
-export([patch/1]).

patch(URL) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, Txt}} =
        httpc:request(patch, {URL,[],"application/json",<<>>}, [], []),
    {_, LengthStr} = lists:keyfind("content-length", 1, Headers),
    Length =
        - length("{\"data\": ")
        + list_to_integer(LengthStr)
        - length("}"),
    <<"{\"data\": ", Data:Length/binary, "}">> = list_to_binary(Txt),
    binary_to_integer(Data).
-endif.
