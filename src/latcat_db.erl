-module(latcat_db).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(counter, {val}).
-record(note, {id, user, timestamp=httpd_util:rfc1123_date(), lat, lon, text}).

init() ->
	% Add case lists:member(note, mnesia:tables([node()]) of [] -> add; _ -> dont end.
	mnesia:create_table(note, [{attributes, record_info(fields, note)}]).

add(Id, User, Lat, Lon, Text) ->
	F = fun() ->
			R = #note{id=Id, user=User, lat=Lat, lon=Lon, text=Text},
			mnesia:write(R)
	end,
	mnesia:transaction(F).

box(X, Y) ->
	box(X, Y, 1).

box(X, Y, R) ->
	box(X-R, Y-R, X+R, Y+R).

box(X1, Y1, X2, Y2) ->
	F = fun() ->
			Q = qlc:q([N || N <- mnesia:table(note),
								 N#note.lat >= X1,
								 N#note.lat =< X2,
								 N#note.lon >= Y1,
								 N#note.lon =< Y2]),
			qlc:e(Q)
	end,
	{atomic, R} = mnesia:transaction(F),
	R.

json(X, Y) ->
	F = fun(R) ->
			{struct, [{"id", R#note.id},
					  {"user", R#note.user},
					  {"timestamp", list_to_binary(R#note.timestamp)},
					  {"lat", R#note.lat},
					  {"lon", R#note.lon},
					  {"text", list_to_binary(R#note.text)}]}
	end,
	{struct, [{"notes", lists:map(F, box(X, Y))}]}.
