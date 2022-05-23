trapezium = @(a, b, f) ((b - a) / 2) * (f(a) + f(b));
simpson = @(a, b, f) ((b - a) / 6) * (f(a) + 4 * f((a + b) / 2) + f(b));
f = @(x) 2 ./ (1 + x .^ 2);

trapezium(0, 1, f)
simpson(0, 1, f)

x = 0 : 0.1 : 1;

plot(x, f(x));
hold on;
fill([0, 0, 1, 1], [0, f(0), f(1), 0], 'y');

pause();
