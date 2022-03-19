h = 0.25;
i = 0 : 6;
x = @(i) 1 .+ i .* h;
f = @(x) sqrt(5 .* x .^ 2 .+ 1);

function m = fill(m, i)
  if (i < size(m, 2))
    for j = 1 : (length(m(:, i)) - i)
      m(j, i + 1) = m(j + 1, i) - m(j, i);
    end

    m = fill(m, i + 1);
  end
end

matrix = zeros(length(i), length(i));
matrix(:, 1) = f(x(i))';
matrix = fill(matrix, 1)
