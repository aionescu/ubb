printf("a)\n");

a = [
  10 7 8 7;
  7 5 6 5;
  8 6 10 9;
  7 5 9 10
];

b = [32 23 33 31]';

x = a \ b
condNum = cond(a)

printf("\nb)\n");

b_ = [32.1 22.9 33.1 30.9]';

x_ = a \ b_

inErr = norm(b - b_) / norm(b)
outErr = norm(x - x_) / norm(x)
ratio = outErr / inErr

printf("\nc)\n");

a_ = [
  10 7 8.1 7.2;
  7.08 5.04 6 5;
  8 5.98 9.89 9;
  6.99 4.99 9 9.98
];

x_ = a_ \ b

inErr = norm(a - a_) / norm(b)
outErr = norm(x - x_) / norm(x)
ratio = outErr / inErr
