"";

function r = rectangle(a, b, f)
  r = (b - a) * f((a + b) / 2);
end

function r = repeatedRectangle(a, b, n, f)
  h = (b - a) / n;
  x1 = a + h / 2;
  i = [2 : n];
  xi = x1 + (i - 1) * h;
  r = h * (f(x1) + sum(f(xi)));
end

x = linspace(0, 10, 20);
a = 1;
b = 1.5;
f = @(x) exp(-x .^ 2);

% a
rectangle(a, b, f)

% b
hold on;
axis([a b f(b) f(a)]);
fill([a a b b], [0 f((a + b) / 2) f((a + b) / 2) 0], 'g');
fplot(f, [a b]);
hold off;

% c
printf("n = 150: %d\n", repeatedRectangle(a, b, 150, f));
printf("n = 500: %d\n", repeatedRectangle(a, b, 500, f));

pause();
