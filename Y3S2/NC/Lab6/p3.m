pkg load splines;

time = [0 3 5 8 13];
dst = [0 225 383 623 993];
speed = [75 77 80 74 72];

spl = spline(time, [75 dst 72]);
ppval(spl, 10)

f = fnder(spl, 1);
ppval(f, 10)
