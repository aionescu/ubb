x = [1 1.5 2 3 4];
f = [0 0.17609 0.30103 0.47712 0.60206];

printf("2.5: %d\n", newton(x, f, 2.5));
printf("3.25: %d\n", newton(x, f, 3.25));

y = (10 : 35) ./ 10;
fy = arrayfun(@(y) newton(x, f, y), y);

printf("max error: %d\n", max(abs(log10(y) - fy)));
