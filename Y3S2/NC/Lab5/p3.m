x = [-5: 0.1: 5];
f = sin(2 .* x);

t = linspace(-5, 5, 15);
v = sin(2 .* t);
vd = 2 .* cos(2 .* t);

hdiff(t, v, vd);

result = hermite(t, v, vd, x);

plot(x, f, x, result, 'ro');
pause();
