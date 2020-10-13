x = 0 : 0.01 : 3
fx = binocdf(x, 3, 1 / 2)
plot(x, fx)

hold on

xx = 0 : 3
gx = binopdf(xx, 3, 1 / 2)
plot(xx, gx, "r--")
