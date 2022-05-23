function m = hdiff(x, f, fd)
  n = 2 * length(f);
  m = nan(n);

  m(:, 1) = repelem(f, 2);
  m(1 : 2 : n - 1, 2) = fd;
  m(2 : 2 : n - 2, 2) = diff(f) ./ diff(x);

  x = repelem(x, 2);

  for i = 3 : n
      m(1 : end - i + 1, i) = ...
        (m(2 : end - i + 2, i - 1) - m(1 : end - i + 1, i - 1)) ...
        ./ (x(i : end) - x(1 : end - i + 1))';
  end
end
