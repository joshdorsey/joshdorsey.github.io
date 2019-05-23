-module(draw_mb).
-export([run/2, export/2]).

% 1:
	% X: -0.74514 -0.74544
	% Y: 0.112925 0.113225

% 2:
	% X: -0.74595 -0.74465
	% Y: 0.11205 0.11335

% 3:
	% X: -0.745458 -0.745398
	% Y: 0.112979 0.113039

% 4:
	% X: -0.15 -0.1
	% Y: 0.95 1.0

run(W, H) ->
	BottomX = -0.745458,
	TopX = -0.745398,
	Middle1X = BottomX + ((TopX - BottomX) / 3),
	Middle2X = BottomX + (((TopX - BottomX) * 2) / 3),
	
	BottomY = 0.112979,
	TopY = 0.113039,
	Middle1Y = BottomY + ((TopY - BottomY) / 3),
	Middle2Y = BottomY + (((TopY - BottomY) * 2) / 3),
	
	run_bounds(BottomX, Middle1X, BottomY, Middle1Y, W, H, "1_1"),
	run_bounds(Middle1X, Middle2X, BottomY, Middle1Y, W, H, "2_1"),
	run_bounds(Middle2X, TopX, BottomY, Middle1Y, W, H, "3_1"),
	
	run_bounds(BottomX, Middle1X, Middle1Y, Middle2Y, W, H, "1_2"),
	run_bounds(Middle1X, Middle2X, Middle1Y, Middle2Y, W, H, "2_2"),
	run_bounds(Middle2X, TopX, Middle1Y, Middle2Y, W, H, "3_2"),
	
	run_bounds(BottomX, Middle1X, Middle2Y, TopY, W, H, "1_3"),
	run_bounds(Middle1X, Middle2X, Middle2Y, TopY, W, H, "2_3"),
	run_bounds(Middle2X, TopX, Middle2Y, TopY, W, H, "3_3").

run_bounds(BottomX, TopX, BottomY, TopY, W, H, Tag) ->
	Set = mb:mandelbrot_set(BottomX, TopX, BottomY, TopY, W, H, 200),
	{{Y, M, D}, {Hour, Min, S}} = erlang:localtime(),
	Id = S + 61*Min + 61*Hour + 25*D + 35*M + 14*Y,
	Fname = io_lib:format("mb_~s_~w.png", [Tag, Id]),
	export(Set, Fname).

export({W, H, Values}, Fname) ->
	Img = egd:create(W, H),
	build_image(Img, Values, H, W),
	egd:save(egd:render(Img, png), Fname),
	egd:destroy(Img),
	ok.

%% @doc Base case for a row
build_image(_, [], 0, _) ->
	ok;
%% @doc Base case for a column
build_image(_, [], _, 0) ->
	ok;
%% @doc Finished drawing
build_image(Img, [], 0, 0) ->
	Img;
%% @doc Run for each row
build_image(Img, [First | Rest], X, Y) when is_list(First) ->
	build_image(Img, First, X, Y),
	build_image(Img, Rest, X, Y-1);
%% @doc Run for each column
build_image(Img, [First | Rest], X, Y) when is_integer(First) ->
	egd:line(Img, {X, Y}, {X-1, Y-1}, color_mapper(First)),
	build_image(Img, Rest, X-1, Y).

mod(X, Y) when X > Y ->
	mod(X - Y, Y);
mod(X, Y) when X < 0 ->
	mod(X + Y, Y);
mod(X, _) ->
	X.

hsv_color(H, S, V) ->
	C = V * S,
	X = C * (1 - abs(mod(H / 60, 2) - 1)),
	M = V - C,
	
	if
		H >= 300 ->
			Rp = C,
			Gp = 0,
			Bp = X;
		H >= 240 ->
			Rp = X,
			Gp = 0,
			Bp = C;
		H >= 180 ->
			Rp = 0,
			Gp = X,
			Bp = C;
		H >= 120 ->
			Rp = 0,
			Gp = C,
			Bp = X;
		H >= 60 ->
			Rp = X,
			Gp = C,
			Bp = 0;
		H >= 0 ->
			Rp = C,
			Gp = X,
			Bp = 0
	end,
	
	R = (Rp + M)*255,
	G = (Gp + M)*255,
	B = (Bp + M)*255,
	egd:color({R, G, B}).

color_mapper(Val) when is_integer(Val) ->
	hsv_color(mod(Val * 5 + 190, 360), 0.8, 0.9).