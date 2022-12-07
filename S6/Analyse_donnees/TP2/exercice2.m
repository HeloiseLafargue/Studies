% Estimation des parametres par les moindres carres ordinaires :
Beta_chapeau = MCO2(Data, DataMod);
Img = -1/Beta_chapeau(1) * (log(ImMod) - Beta_chapeau(2));


% Affichage de l'image estimee par MCO :
imshow(Img);

% Estimation de l'erreur par les moindres carres ordinaires :
[n, m] = size(ImMod);
RMSE = sqrt( sum( sum(I - Img).^2))/(n*m);

% Estimation des parametres par les moindres carres totaux :