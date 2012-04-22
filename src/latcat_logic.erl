-module(latcat_logic).

-compile(export_all).

-define(TAKE(K, L), proplists:get_value(K, L)).

-define(List2Num(L), try list_to_float(L) catch _:_ -> list_to_integer(L) end).

search(LatL, LonL) ->
	Lat = ?List2Num(LatL),
	Lon = ?List2Num(LonL),
	latcat_db:json(Lat, Lon).

add_post(Note) ->
	Id = ?List2Num(?TAKE("id", Note)),
	User = ?List2Num(?TAKE("user", Note)),
	Lat = ?List2Num(?TAKE("lat", Note)),
	Lon = ?List2Num(?TAKE("lon", Note)),
	Text = ?TAKE("text", Note),
	latcat_db:add(Id, User, Lat, Lon, Text).

