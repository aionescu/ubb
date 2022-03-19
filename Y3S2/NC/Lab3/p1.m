x = [1930 1940 1950 1960 1970 1980];
f = [123203 131669 150697 179323 203212 226505];

approx = @(year) printf("%d: %d\n", year, round(lagrange(f, x, year)));

approx(1955);
approx(1995);
