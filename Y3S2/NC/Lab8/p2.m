f = @(x, y) log(x + 2 * y);

function r = doubleTrapezium(a, b, c, d, f)
  ab2 = (a + b) / 2;
  cd2 = (c + d) / 2;

  factor = (b - a) * (d - c) / 16;

  sum = ...
    f(a, c) + f(a, d) + f(b, c) + f(b, d) + ...
    2 * f(ab2, c) + 2 * f(ab2, d) + 2 * f(a, cd2) + ...
    2 * f(b, cd2) + 4 * f(ab2, cd2);

  r = factor * sum;
end

doubleTrapezium(1.4, 2, 1, 1.5, f)
