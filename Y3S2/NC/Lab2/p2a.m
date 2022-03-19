t = -1 : 0.01 : 1;
T = @(n, t) cos(n .* acos(t));

hold on
plot(t, T(1, t));
plot(t, T(2, t));
plot(t, T(3, t));
hold off
pause();
