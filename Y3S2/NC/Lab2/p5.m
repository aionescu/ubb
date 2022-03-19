x = [2 4 6 8];
f = [4 8 14 16];

function m = fill(m, i)
  if (i <= size(m, 2))
    for j = 1 : (size(m, 2) - i + 1)
      m(j, i) = (m(j + 1, i - 1) - m(j, i - 1)) / (m(j + i - 2, 1) - m(j, 1));
    end

    m = fill(m, i + 1);
  end
end

matrix = zeros(length(x), length(x) + 1);
matrix(:, 1) = x';
matrix(:, 2) = f';
matrix = fill(matrix, 3)
