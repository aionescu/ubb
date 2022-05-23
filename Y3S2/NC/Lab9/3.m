"";

function r = simpson(f, a, b)
  r = ((b - a) / 6) * (f(a) + 4 * f((a + b) / 2) + f(b));
end

function r = repeatedSimpson(f, a, b, n)
  sigma = @(x, f) sum(arrayfun(f, x));

  h = (b - a) / n;
  x = a .* ones(1, n + 1) + (0 : n) .* h;

  s1 = sigma(2 : n + 1, @(k) f((x(k - 1) + x(k)) / 2));
  s2 = sum(f(x(2 : n)));

  r = ((b - a) / (6 * n)) * (f(a) + f(b) + 4 * s1 + 2 * s2);
end

function r = adQuad(f, a, b, err)
  i1 = simpson(f, a, b);
  i2 = simpson(f, a, (a + b) / 2) + simpson(f, (a + b) / 2, b);

  if (abs(i1 - i2) < 15 * err)
    r = i2;
  else
    r = adQuad(f, a, (a + b) / 2, err / 2) + adQuad(f, (a + b) / 2, b, err / 2);
  end
end

f = @(x) 100 ./ (x .^ 2) .* sin(10 ./ x);
a = 1;
b = 3;

eps = 10 ^ -4;

adQuad(f, a, b, eps)
repeatedSimpson(f, a, b, 50)
repeatedSimpson(f, a, b, 100)

x = linspace(1, 3, 100);
plot(x, f(x));

pause();
