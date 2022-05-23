f = @(x) 1 ./ (4 + sin(20 .* x));

function r = repeatedSimpson(n, a, b, f)
  sigma = @(x, f) sum(arrayfun(f, x));

  h = (b - a) / n;
  x = a .* ones(1, n + 1) + (0 : n) .* h;

  s1 = sigma(2 : n + 1, @(k) f((x(k - 1) + x(k)) / 2));
  s2 = sigma(2 : n, @(k) f(x(k)));

  r = ((b - a) / (6 * n)) * (f(a) + f(b) + 4 * s1 + 2 * s2);
end

repeatedSimpson(10, 0, pi, f)
repeatedSimpson(30, 0, pi, f)
