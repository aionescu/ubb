x = -1 : 0.01 : 1;

function t = T(n, x)
  if (n == 0)
    t = ones(size(x));
  elseif (n == 1)
    t = x;
  else
    t = 2 .* x .* T(n - 1, x) - T(n - 2, x);
  end
end

hold on;

for n = 0:5
  plot(x, T(n, x));
end

hold off;
pause();
