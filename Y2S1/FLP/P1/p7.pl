% a. Write a predicate to compute the intersection of two sets.
% b. Write a predicate to create a list (m, ..., n) of all integer numbers from the interval [m, n].

% a)
% elem(a, l1..ln) =
%   { False, if n = 0
%   , True, if a = l1
%   , elem(a, l2..ln), otherwise
%   }
%
% intersect(l1..ln, k1..km) =
%   { [], if n = 0
%   , l1 : intersect(l2..ln, k1..km), if elem(l1, k1..km)
%   , intersect(l2..ln, k1..km), otherwise
%   }

% Flow model: (i i), (i i)

elem(A, [A|_]).
elem(A, [_|T]) :- elem(A, T).

% Flow model: (i i i), (i i o)

intersect([], _, []).
intersect([H|T], L, [H|R]) :- elem(H, L), intersect(T, L, R).
intersect([_|T], L, R) :- intersect(T, L, R).

:- E = 1, L = [1, 2, 3], elem(E, L), format('~w is elem of ~w~n', [E, L]).
:- X = [1, 2], Y = [1, 2, 3, 4], intersect(X, Y, Z), format('intersect(~w, ~w) is ~w~n', [X, Y, Z]).
:- X = [], Y = [1, 2], intersect(X, Y, Z), format('intersect(~w, ~w) is ~w~n', [X, Y, Z]).
:- X = [1, 2], Y = [], intersect(X, Y, Z), format('intersect(~w, ~w) is ~w~n', [X, Y, Z]).

% b)
% iota(m, n) =
%   { [], if m > n
%   , m : iota(m + 1, n), otherwise
%   }

% Flow model: (i i i), (i i o)

iota(M, N, R) :- M < N, iota_asc(M, N, R).
iota(M, N, R) :- M >= N, iota_desc(M, N, R).

iota_asc(M, N, []) :- M > N.
iota_asc(M, N, [M|T]) :- Mn is M + 1, iota_asc(Mn, N, T).

iota_desc(M, N, []) :- M < N.
iota_desc(M, N, [M|T]) :- Mn is M - 1, iota_desc(Mn, N, T).

:- X = 1, Y = 0, iota(X, Y, Z), format('iota(~w, ~w) is ~w~n', [X, Y, Z]).
:- X = 1, Y = 4, iota(X, Y, Z), format('iota(~w, ~w) is ~w~n', [X, Y, Z]).
:- X = 1, Y = 1, iota(X, Y, Z), format('iota(~w, ~w) is ~w~n', [X, Y, Z]).
:- X = 4, Y = 10, iota(X, Y, Z), format('iota(~w, ~w) is ~w~n', [X, Y, Z]).
:- X = 10, Y = 1, iota(X, Y, Z), format('iota(~w, ~w) is ~w~n', [X, Y, Z]).
