x = 0 : 0.001 : 10;
f = (1 + cos(pi .* x)) ./ (1 + x);

subplot(1, 2, 1);
plot(x, f, "r");

x = linspace(0, 10, 21);
y = 0 : 0.01 : 10;
f = (1 + cos(pi .* x)) ./ (1 + x);
g = zeros(1, length(y));

for index = 1 : length(g)
  g(index) = lagrange(f, x, y(index));
end

subplot(1, 2, 2);
plot(y, g, "b");

pause();
