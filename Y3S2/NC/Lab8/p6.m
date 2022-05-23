f = @(t) exp(-(t ^ 2));

function r = repeatedSimpson(n, a, b, f)
  sigma = @(x, f) sum(arrayfun(f, x));

  h = (b - a) / n;
  x = a .* ones(1, n + 1) + (0 : n) .* h;

  s1 = sigma(2 : n + 1, @(k) f((x(k - 1) + x(k)) / 2));
  s2 = sigma(2 : n, @(k) f(x(k)));

  r = ((b - a) / (6 * n)) * (f(a) + f(b) + 4 * s1 + 2 * s2);
end

erF = @(n, x) (2 / sqrt(pi)) * repeatedSimpson(n, 0, x, f);

actual = 0.520499876;

n4 = erF(4, 0.5)
n10 = erF(10, 0.5)

err4 = abs(actual - n4)
err10 = abs(actual - n10)
