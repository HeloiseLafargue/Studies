% Fonction MCO
function Beta = MCO(x, y)
    A = [x.^2-y.^2 x.*y x y ones(length(x),1)];
    b = -y.^2;
    B = pinv(A)*b;
    Beta = [B(1) B(2) 1-B(1) B(3) B(4) B(5)];
end