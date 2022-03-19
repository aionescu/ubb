n = [2 4 6 8];
f = @(x) 1 ./ (1 .+ x .* x);

maxErr = zeros(1, 4);

for i = 1 : 4
  n = i * 2;
  x = (0 : n) .* (10 / n) - 5;
  y = (0 : 100) ./ 10 .- 5;

  fPlot = [];

  for j = y
    v = lagrange(f(x), x, j);
    err = abs(f(j) - v);
    fPlot = [fPlot v];

    maxErr(i) = max(maxErr(i), err);
  end

  subplot(2, 2, i);
  plot(y, fPlot);
end

maxErr
pause();
