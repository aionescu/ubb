function r = newton(x, f, x0)
  df = ddiff(x, f);
  r = f(1, 1) + sum(arrayfun(@(i) prod(arrayfun(@(j) x0 - x(j), 1 : i)) .* df(1, i), 1 : length(x) - 1));
end
