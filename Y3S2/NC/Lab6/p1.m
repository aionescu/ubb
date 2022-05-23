n = [0, pi / 2, pi, 3 * pi / 2, 2 * pi];
f = sin(n);

sin(pi / 4)
spline(n, f, pi / 4)
spline(n, [1 f 1], pi / 4)

x = 0 : 0.1 : 2 * pi;

plot(
  x, sin(x), 'm',
  x, spline(n, f, x), 'b',
  x, spline(n, [1 f 1], x), 'g'
);

legend("f", "spline", "clamped");
pause();
