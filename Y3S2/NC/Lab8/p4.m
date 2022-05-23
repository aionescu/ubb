f = @(x) x .* log(x);

function r = repeatedTrapezium(n, a, b, f)
  h = (b - a) / n;
  x = a .* ones(1, n + 1) + (0 : n) .* h;

  r = ((b - a) / (2 * n)) * (f(a) + f(b) + 2 * sum(f(x(2 : n))));
end

eps = 0.0007;
actual = 0.636294368858383;

n = 1;
while abs(actual - repeatedTrapezium(n, 1, 2, f)) >= eps
  n = n + 1;
end

n
repeatedTrapezium(n, 1, 2, f)
