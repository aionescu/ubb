"";

function r = repeatedTrapezium(f, a, b, n)
  h = (b - a) / n;
  xi = a : h : b;
  r = h / 2 * (f(a) + f(b) + 2 * sum(f(xi(2 : length(xi) - 1))));
end

function r = romberg(f, qPrev, k, a, b, eps)
  h = b - a;
  j = 1 : 2 ^ (k - 1);
  q = 0.5 * qPrev + h / (2 ^ k) * sum(f(a + (2 .* j - 1) / (2 ^ k) * h));

  if (abs(q - qPrev) > eps)
      r = romberg(f, q, k + 1, a, b, eps);
  else
      r = q;
  end
end

function r = rombergAitken(f, i, prev, a, b, eps)
  row = linspace(0, 0, i);
  row(1) = repeatedTrapezium(f, a, b, 2 ^ (i - 1));

  for j = 2 : i
    row(j) = (4 ^ (-j) * prev(j - 1) - row(j - 1)) / (4 ^ (-j) - 1);
  end

  if (abs(row(i) - prev(i - 1)) > eps)
    r = rombergAitken(f, i + 1, row, a, b, eps);
  else
    r = row(i);
  end
end

f = @(x) 2 ./ (1 + x .^ 2);
a = 0;
b = 1;

eps = 10 ^ -4;

format long;
q0 = ((b - a) / 2) * (f(a) + f(b));
romberg(f, q0, 1, a, b, eps)

t0 = repeatedTrapezium(f, a, b, 1);
rombergAitken(f, 2, t0, a, b, eps)
