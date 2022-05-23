[x y] = ginput(5);
i = 0 : 0.01 : 1;

plot(
  x, y, '*',
  i, spline(x, y, i), 'r'
);

pause();
