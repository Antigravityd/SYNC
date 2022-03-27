1;


function y = f(x)

  # Change these to desired initial conditions
  a = 2;
  A = 5;
  b = 3;
  B = 5;

  y = zeros(3, 1);
  y(1) = x(1)*exp(a*x(3)) + x(2)*exp(-a*x(3)) - A;
  y(2) = x(1)*exp(b*x(3)) + x(2)*exp(-b*x(3)) - B;
  y(3) = 4*x(1)*x(2)*x(3)^2-1;

endfunction

# Change [1;1;1] to the desired inital guess [x;y;z]
[x, F, converged] = fsolve(@f, [10;10;10])
