x = -1 : 0.01 : 3;

function p = P(n, x)
  p = 0;

  for k = 0:n
    p = p + x .^ k / factorial(k);
  end
end

hold on;
plot(x, exp(x));

for n = 1:6
  plot(x, P(n, x));
end

hold off;
pause();
