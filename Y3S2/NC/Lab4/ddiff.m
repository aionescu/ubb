function r = ddiff(x, f)
  n = length(x);
  m = zeros(n, n + 1);

  m(:, 1) = x';
  m(:, 2) = f';

  for j = 3 : n + 1
    for i = 1 : n - (j - 2)
      m(i, j) = (m(i + 1, j - 1) - m(i, j - 1)) / (x(1, i + j - 2) - x(1, i));
    end
  end

  r = m(:, 3 : end);
end
