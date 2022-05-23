x = linspace(0, 6, 13);
y = exp(sin(x));

plot(x, y, "r*");
hold on;

y0 = arrayfun(@(x0) newton(x, y, x0), x);

plot(x, y0, "g");
hold off;
pause();
