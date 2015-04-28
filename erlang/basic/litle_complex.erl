%% this module is use for convert the temps
-module(litle_complex).
-export([format_temps/1]).


%% export function
format_temps([])->
    ok;
format_temps([City|Rest])->
    print_temps(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name,{c,Temp}}) ->
    {Name,{c,Temp}};
convert_to_celsius({Name,{f,Temp}}) ->
    {Name,{c,(Temp -32)*5/9}}.

print_temps({Name,{c,Temp}}) ->
    io:format("~-15w ~w c~n",[Name,Temp]).
