% a)

% unique(a1..an) =
%   { [], if n = 0
%   ; unique(a2..an), if a1 = a2
%   ; a1 U unique(a2..an), otherwise
%   }

% (i, o)
unique([], []).
unique([A, A | As], L) :- unique([A | As], L).
unique([A | As], [A | L]) :- unique(As, L).

:- L = [], unique(L, Lu), format('unique(~w) is ~w~n', [L, Lu]).
:- L = [1], unique(L, Lu), format('unique(~w) is ~w~n', [L, Lu]).
:- L = [1, 1], unique(L, Lu), format('unique(~w) is ~w~n', [L, Lu]).
:- L = [1, 1, 2, 3, 4, 4, 5], unique(L, Lu), format('unique(~w) is ~w~n~n', [L, Lu]).

% merge(a1..am, b1..bn) =
%   { b1..bn, if m = 0
%   ; a1..am, if n = 0
%   ; a1 U merge(a2..am, b1..bn), if a1 < b1
%   ; b1 U merge(a1..am, b2..bn), otherwise
%   }

% (i, i, o)
merge([], L, L).
merge(L, [], L).
merge([A | As], [B | Bs], [A | L]) :- A < B, merge(As, [B | Bs], L).
merge([A | As], [B | Bs], [B | L]) :- merge([A | As], Bs, L).

:- A = [1, 2], B = [], merge(A, B, L), format('merge(~w, ~w) is ~w~n', [A, B, L]).
:- A = [], B = [1, 2], merge(A, B, L), format('merge(~w, ~w) is ~w~n', [A, B, L]).
:- A = [1, 2, 2, 3], B = [1, 4, 4], merge(A, B, L), format('merge(~w, ~w) is ~w~n', [A, B, L]).
:- A = [1, 2, 3, 7, 10], B = [1, 2, 4, 4, 4, 8, 15], merge(A, B, L), format('merge(~w, ~w) is ~w~n~n', [A, B, L]).

% mergeUnique(a1..am, b1..bn) = unique(merge(a1..am, b1..bn))

% (i, i, o)
mergeUnique(As, Bs, Lu) :- merge(As, Bs, L), unique(L, Lu).

:- A = [1, 2], B = [], mergeUnique(A, B, L), format('mergeUnique(~w, ~w) is ~w~n', [A, B, L]).
:- A = [], B = [1, 2], mergeUnique(A, B, L), format('mergeUnique(~w, ~w) is ~w~n', [A, B, L]).
:- A = [1, 2, 2, 3], B = [1, 4, 4], mergeUnique(A, B, L), format('mergeUnique(~w, ~w) is ~w~n', [A, B, L]).
:- A = [1, 2, 3, 7, 10], B = [1, 2, 4, 4, 4, 8, 15], mergeUnique(A, B, L), format('mergeUnique(~w, ~w) is ~w~n~n', [A, B, L]).

% b)

% mergeLists(a1..an) =
%   { [], if n = 0
%   ; mergeLists(a2..an), if a1 is number
%   ; mergeUnique(a1, mergeLists(a2..an)), otherwise
%   }

% (i, o)
mergeLists([], []).
mergeLists([A | As], M) :- number(A), mergeLists(As, M).
mergeLists([As | Es], L) :- mergeLists(Es, M), mergeUnique(As, M, L).

:- L = [], mergeLists(L, Lm), format('mergeLists(~w) is ~w~n', [L, Lm]).
:- L = [1, 2, 3, 4, 5, 5, 5, 7], mergeLists(L, Lm), format('mergeLists(~w) is ~w~n', [L, Lm]).
:- L = [1, 2, [], [], [1, 2, 2], 4, [0, 3, 4], 4], mergeLists(L, Lm), format('mergeLists(~w) is ~w~n', [L, Lm]).
:-
  L = [1, [2, 3], 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5, [1, 1, 11], 8],
  mergeLists(L, Lm),
  format('mergeLists(~w) is ~w~n~n', [L, Lm]).

% Extra requirement
% Compute the product of all elements from a list

% product(a1..an) =
%   { 1, if n = 0
%   ; a1 * product(a2..an), otherwise
%   }

% (i, o)
product([], 1).
product([A | As], P) :- product(As, Ps), P is A * Ps.

:- L = [], product(L, P), format('product(~w) is ~w~n', [L, P]).
:- L = [2, 2], product(L, P), format('product(~w) is ~w~n', [L, P]).
:- L = [1, 2, 4, 9], product(L, P), format('product(~w) is ~w~n', [L, P]).
:- L = [1, 2, 3, 0], product(L, P), format('product(~w) is ~w~n~n', [L, P]).
