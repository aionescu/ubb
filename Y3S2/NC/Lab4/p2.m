x = [1 2 3 4 5];
y = [22 23 25 30 28];

printf("2.5: %d\n", newton(x, y, 2.5));

plot(x, y, "r*");
hold on;

x0 = 1 : 0.05 : 5;
y0 = arrayfun(@(x0) newton(x, y, x0), x0);

plot(x0, y0, "b");
hold off;
pause();
