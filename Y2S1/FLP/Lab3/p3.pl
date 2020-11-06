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

% mergeUnique(a1..am, b1..bn) = unique(merge(a1..am, b1..bn))

% (i, i, o)
mergeUnique(As, Bs, Lu) :- merge(As, Bs, L), unique(L, Lu).

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
