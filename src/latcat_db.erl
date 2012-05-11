-module(latcat_db).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-include("latcat.hrl").

init() ->
	% Add case lists:member(note, mnesia:tables([node()]) of [] -> add; _ -> dont end.
	mnesia:create_table(count, [{attributes, record_info(fields, count)}]),
	mnesia:create_table(note, [{attributes, record_info(fields, note)}]),
    F = fun() ->
            mnesia:write(#count{key=note, val=0})
    end, mnesia:transaction(F).

add(Lat, Lon, Text) ->
	F = fun() ->
            [C|_] = mnesia:read(count, note),
            NewCount = C#count.val+1,
            ok = mnesia:write(C#count{val=NewCount}),
			R = #note{id=NewCount, lat=Lat, lon=Lon, text=Text},
			mnesia:write(R)
	end, mnesia:transaction(F).

delete(Id) ->
    F = fun() ->
            mnesia:delete({note, Id})
    end, mnesia:transaction(F).

box(X, Y) -> box(X, Y, 1).

box(X, Y, R) -> box([X-R, Y-R, X+R, Y+R]).

box([Top, Left, Bottom, Right]) ->
	F = fun() ->
			Q = qlc:q([N || N <- mnesia:table(note),
								 N#note.lat >= Bottom,
								 N#note.lat =< Top,
								 N#note.lon =< Right,
								 N#note.lon >= Left]),
			qlc:e(Q)
	end, {atomic, R} = mnesia:transaction(F),
	R.

json(X, Y) ->
	F = fun(R) ->
			{struct, [{"id", R#note.id},
					  {"timestamp", list_to_binary(R#note.timestamp)},
					  {"lat", R#note.lat},
					  {"lon", R#note.lon},
					  {"text", list_to_binary(R#note.text)}]}
	end,
	{struct, [{"notes", lists:map(F, box(X, Y))}]}.
