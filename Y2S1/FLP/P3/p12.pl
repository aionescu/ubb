% valid(x) = x ∈ { -1, 0, 1 }
% Flow model: (i)
valid(X) :- member(X, [-1, 0, 1]).

% validDiff(x) = |x| ∈ { 1, 2 }
% Flow model: (i)
validDiff(X) :- Y is abs(X), member(Y, [1, 2]).

% string(n) =
%   1. [], if n = 0
%   2. [x], if n = 1 and valid(x)
%   3. a ⋃ b ⋃ l, if n >= 2 and b ⋃ l = string(n - 1) and valid(a) and valid(b) and validDiff(b - a)
% Flow model: (i, o)
string(0, []).
string(1, [H]) :- valid(H).
string(N, [H1, H2 | T]) :- N >= 2, valid(H1), valid(H2), D is H2 - H1, validDiff(D), M is N - 1, string(M, [H2 | T]).

% strings(n) = ⋃ string(n)
% Flow model: (i, o)
strings(N, L) :- findall(X, string(N, X), L).
