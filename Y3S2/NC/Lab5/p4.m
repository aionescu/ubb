x = [8.3 8.6];
f = [17.56492 18.50515];
fd = [3.116256 3.151762];
x0 = 8.4;

r = hermite(x, f, fd, x0);
err = abs(x0 * log(x0) - r);

printf("f(%d) = %d\nerr = %d\n", x0, r, err);
