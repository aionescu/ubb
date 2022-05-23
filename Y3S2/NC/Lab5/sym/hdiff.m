function r = hdiff(x, f, fd)
  m = length(fd);
  n = length(x);
  t = zeros(n, n + 1);

  t(:, 1) = x;
  t(:, 2) = f;

  % 1st order divided differences
  for i = 1 : m
      t(2 * i - 1, 3) = fd(i);
  end

  for i = 1: m-1
      t(2 * i, 3) = (f(2 * i + 1) - f(2 * i)) / (x(2 * i + 1) - x(2 * i));
  end

  % order > 1
  for i = 4 : n + 1
      o = i - 2;
      for j = 1 : n
          if j <= n - o
              t(j, i) = (t(j + 1, i - 1) - t(j, i - 1)) / (x(1, j + o) - x(1, j));
          else
              t(j, i) = NaN;
          end
      end
  end

  r = t(1 : end - 1, 3 : end);
end
