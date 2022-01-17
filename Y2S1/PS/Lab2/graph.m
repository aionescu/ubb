x = 0 : 0.01 : 3

f = @(x) x .^ 5 ./ 10
g = @(x) x .* sin(x)
h = @(x) cos(x)

title("Graph")
legend("Data1", "Data2", "Data3")

subplot(3, 1, 1): plot(x, f(x), "r--")
subplot(3, 1, 2): plot(x, g(x), "m+")
subplot(3, 1, 3): plot(x, h(x), "g-.")
