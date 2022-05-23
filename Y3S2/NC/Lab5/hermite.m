function h = hermite(x, f, fd, x0)
  coefs = hdiff(x, f, fd)(1, :);

  h = zeros(size(x0));
  x = repelem(x, 2);

  for i = 1 : length(x0)
    product = 1;

    for j = 1 : length(coefs)
      h(i) = h(i) + coefs(j) * product;
      product = product * (x0(i) - x(j));
    end
  end
end
