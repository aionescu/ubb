rectangle('Position', [0 0 3 5]);

[x y] = ginput(10);
p = polyfit(x, y, 2);

points = 0 : 0.01 : 3;

plot(
  x, y, 'ro',
  points, polyval(p, points), 'b-'
);

pause();
