function r = lagrange(f, x, a)
  fst = 0;
  snd = 0;

  for i = 1 : length(x)
    if a == x(i)
      r = f(i);
      return
    end

    c = A(x, i);
    fst = fst + c * f(i) ./ (a - x(i));
    snd = snd + c ./ (a - x(i));
  end

  r = fst / snd;
end

function r = A(x, i)
  p = 1;

  for j = 1 : length(x)
    if j != i
      p = p * (x(i) - x(j));
    end
  end

  r = 1 / p;
end
