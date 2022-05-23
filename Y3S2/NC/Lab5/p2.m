x = [1 2];
f = [0 0.6931];
fd = [1 0.5];
x0 = 1.5;

r = hermite(x, f, fd, x0);
err = abs(log(x0) - r);

printf("f(%d) = %d\nerr = %d\n", x0, r, err);
