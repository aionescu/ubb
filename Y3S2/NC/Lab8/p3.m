f = @(p, r) @(x) sqrt(1 - ((p / r) ^ 2) .* sin(x));

function r = repeatedTrapezium(n, a, b, f)
  h = (b - a) / n;
  x = a .* ones(1, n + 1) + (0 : n) .* h;

  r = ((b - a) / (2 * n)) * (f(a) + f(b) + 2 * sum(f(x(2 : n))));
end

h = @(p, r, n) (60 * r / (r ^ 2 - p ^ 2)) * repeatedTrapezium(n, 0, 2 * pi, f(p, r));

r = 110;
p = 75;

h(p, r, 1)
h(p, r, 5)
