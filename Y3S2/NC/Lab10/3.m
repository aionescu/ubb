"";

function r = gauss(a, b)
  m = [a b];
  lines = length(m(:, 1));

  for l = 1 : lines
    [pivot pivotLine] = max(abs(m(l : end, l)));
    pivotLine = pivotLine + l - 1;

    if l != pivotLine
      tmp = m(l, :);
      m(l, :) = m(pivotLine, :);
      m(pivotLine, :) = tmp;
    end

    for k = (l + 1) : lines
      m(k, :) = m(k, :) - m(k, l) / m(l, l) .* m(l, :);
    end
  end

  x = zeros(1, l);
  x(lines) = m(end, end) / m(end, end - 1);

  for l = (lines - 1) : -1 : 1
    x(l) = (m(l, end) - sum(m(l, l + 1 : lines) .* x(l + 1 : lines))) / m(l, l);
  end

  r = x';
end

a = [
  1 1 1 1;
  2 3 1 5;
  -1 1 -5 3;
  3 1 7 -2
];

b = [10 31 -2 18]';

computed = gauss(a, b)
actual = a \ b
