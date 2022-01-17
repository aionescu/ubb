n = input('Number of simulations: ');
u = rand(3, n);

y = (u < 1 / 2);
x = sum(y);

clf;
hist(x);
