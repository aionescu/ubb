function [hf, hfd] = hermite(x, f, fd)
  pkg load symbolic;
  warning("off", "all");

  m = length(x);
  n = 2 * m;

  z = zeros(1, n);

  for i = 1 : m
      z(2 * i - 1) = x(i);
      z(2 * i) = x(i);
  end

  fz = zeros(1, n);

  for i = 1 : m
      fz(2 * i - 1) = f(i);
      fz(2 * i) = f(i);
  end

  % divided differences
  dfz = hdiff(z, fz, fd);

  %polynomial
  syms x

  hf = fz(1);
  for i = 2 : n
      prod = 1;

      for j = 1 : i - 1
          prod = prod .* (x - z(j));
      end

      prod = prod .* dfz(1, i-1);

      hf = hf + prod;
  end

  hfd = diff(hf);

  hf = matlabFunction(hf);
  hfd = matlabFunction(hfd);
end
