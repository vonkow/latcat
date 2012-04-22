-module(latcat_logic).

-compile(export_all).

-include("latcat.hrl").

-define(TAKE(K, L), proplists:get_value(K, L)).
-define(List2Num(L), try list_to_float(L) catch _:_ -> list_to_integer(L) end).

search(LatL, LonL) ->
	Lat = ?List2Num(LatL),
	Lon = ?List2Num(LonL),
	Notes = latcat_db:box(Lat, Lon),
	F = fun(R) ->
			{struct, [{"id", R#note.id},
					  {"timestamp", list_to_binary(R#note.timestamp)},
					  {"lat", R#note.lat},
					  {"lon", R#note.lon},
					  {"text", list_to_binary(R#note.text)}]}
	end,
	{struct, [{"notes", lists:map(F, Notes)}]}.


add_post(Note) ->
	Lat = ?List2Num(?TAKE("lat", Note)),
	Lon = ?List2Num(?TAKE("lon", Note)),
    Text = ?TAKE("text", Note),
    Text1 = re:replace(Text, "<", "\\&lt;", [{return, list}, global]),
    Text2 = re:replace(Text1, ">", "\\&gt;", [{return, list}, global]),
	latcat_db:add(Lat, Lon, Text2).

delete_note(Data) ->
	Id = ?List2Num(?TAKE("id", Data)),
    latcat_db:delete(Id).
