-module(mb).
-export([mandelbrot_set/7]).


mandelbrot_set(Xmin, Xmax, Ymin, Ymax, W, H, Iter) ->
	L1 = linespace(Xmin, Xmax, W, [Xmin]),
	L2 = linespace(Ymin, Ymax, H, [Ymin]),
	L3 = create_set_portion(L1, L2, Iter, []),	
	{W, H, L3}.

linespace(MinVal, MaxVal, Num, Arr) ->
	Diff = MaxVal - MinVal,
	Temp = Num - 1,
	Temp2 = Diff / Temp,
	linespace(MinVal, Temp2, Num - 1, 0, Arr).

linespace(Val, Inc, Num, Ind, List) when Ind < Num ->
	linespace(Val + Inc, Inc, Num, Ind + 1, [Val + Inc | List]);
linespace(_, _, Num, Ind, List) when Ind >= Num ->
	lists:reverse(List).

%% @doc Checks if (and how fast) the series diverges
mandelbrot_calc(_, _, 0, _, _, _) ->
	0;
mandelbrot_calc(Real, Imaginary, Maxiter, Index, InitR, InitC) when Maxiter > 0 ->
	Mag = math:sqrt(Real * Real + Imaginary * Imaginary), 
	if 
		Mag >= 2 ->
			Index;
	 	Mag < 4 ->
			NewR = Real * Real - Imaginary * Imaginary + InitR,
			NewI = 2 * Real * Imaginary + InitC,
			mandelbrot_calc(NewR, NewI, Maxiter - 1, Index + 1, InitR, InitC)
	end.

%% @doc Calculates each row in the set
create_set_portion(_, [], _, List) ->
	lists:reverse(List);
create_set_portion(L1, [Current | Rest], MaxIter, List) ->
	ListElem = create_set_row(L1, Current, MaxIter, []),
	create_set_portion(L1, Rest, MaxIter, [ListElem | List]).

%% @doc Calculates a row of the set
create_set_row([], _, _, List) ->
	List;
create_set_row([Current | Rest], Test, MaxIter, List) ->
	Elem = mandelbrot_calc(Current, Test, MaxIter, 0, Current, Test),
	create_set_row(Rest, Test, MaxIter, [Elem | List]).
