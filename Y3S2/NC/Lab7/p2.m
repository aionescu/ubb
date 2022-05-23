t = [0 10 20 30 40 60 80 100];
p = [0.0061 0.0123 0.0234 0.0424 0.0738 0.1992 0.4736 1.0133];

p1 = polyfit(t, p, 2);
p2 = polyfit(t, p, 7);

v1 = polyval(p1, 45)
v2 = polyval(p2, 45)

e1 = abs(0.095848 - v1)
e2 = abs(0.095848 - v2)

points = 0 : 0.01 : 100;

plot(
  t, p, 'bo',
  points, polyval(p1, points), 'r-',
  points, polyval(p2, points), 'b-'
);

pause();
