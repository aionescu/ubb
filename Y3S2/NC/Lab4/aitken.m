function r = aitken(x, y, eps, x0)
  m = zeros(length(x));
  m(:, 1) = y;

  for i = 2 : length(x)
    for j = 1 : i - 1
      m(i, j + 1) = (1 / (x(i) - x(j))) * (m(j, j) * (x(i) - x0) - m(i, j) * (x(j) - x0));
    end

    if abs(m(i, i) - m(i - 1, i - 1)) <= eps
      r = m(i, i);
      return;
    end
  end

  error("error > eps");
end
