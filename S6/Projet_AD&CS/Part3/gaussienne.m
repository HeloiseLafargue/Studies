% calcule les probabilités p(X | w) 
% x données de dimension (nb,q)
% la classe w est caractérisée par les paramètres de sa loi de probabilité gaussienne :
%  - moyenne mu(q,1)
%  - matrice de variance/covariance Sigma(q,q)

function P = gaussienne(x, mu, Sigma)

[nb, q] = size(x);

P = zeros(nb,1);

for i = 1:nb
    x_centre = x(i, :)' - mu;
    P(i) = exp(-(x_centre')*(Sigma \ x_centre)/2);
end

denominateur = (2*pi)^(q/2)*sqrt(det(Sigma));

P = P / denominateur;

end

