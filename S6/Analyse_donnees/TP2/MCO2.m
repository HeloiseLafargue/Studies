% Fonction MCO exercice 2
function Beta = MCO2(I, J)
    A = [-I ones(length(I),1)];
    b = log(J);
    Beta = pinv(A)*b;
end