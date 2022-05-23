for n = 10 : 15
  c = cond(fliplr(vander(-1 + (2 / n) .* (1 : n))));
  printf("n = %d, cond = %d\n", n, c);
end
