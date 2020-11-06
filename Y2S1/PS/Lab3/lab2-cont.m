n = input('Nr. of trials: ');
p = input('Probability of success: ');

% c)
p1 = binopdf(0, n, p);
p2 = 1 - binopdf(1, n, p);

fprintf('P(X=0)=%1.6f\n', p1);
fprintf('P(X~=1)=%1.6f\n', p2);

% d)
p3 = binocdf(2, n, p);
p4 = binocdf(1, n, p);

fprintf('P(X<=2)=%1.6f\n', p3);
fprintf('P(X<2)=%1.6f\n', p4);

% e)
p5 = 1 - binocdf(0, n, p);
p6 = 1 - binocdf(1, n, p);

fprintf('P(X>=1)=%1.6f\n', p5);
fprintf('P(X>1)=%1.6f\n', p6);
