function r = neville(x, f, x0)
  q = zeros(length(x));
  q(:, 1) = f;

  for i = 2 : length(x)
    for j = 2 : i
      q(i, j) = ((x(i) - x0) * q(i - 1, j - 1) + (x0 - x(i - j + 1)) * q(i, j - 1)) / (x(i) - x(i - j + 1));
    end
  end

  r = q(length(x), length(x));
end
