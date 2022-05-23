x = [0 3 5 8 13];
f = [0 225 383 623 993];
fd = [75 77 80 74 72];

[hf, hfd] = hermite(x, f, fd);

x0 = 10;
printf("distance: %d\nspeed: %d\n", hf(x0), hfd(x0));
