x = 1 : 7;
f = [13 15 20 14 15 13 10];

p = polyfit(x, f, 1);
fprintf('φ(x) = %f * x + %f\n', p);

v = polyval(p, 8)
minE = sum((f - polyval(p, x)) .^ 2)

points = 1 : 0.01 : 8;

plot(
  x, f, 'bo',
  points, polyval(p, points), 'r-'
);

pause();
