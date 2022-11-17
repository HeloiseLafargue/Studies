
% TP3 de Statistiques : fonctions a completer et rendre sur Moodle
% Nom : LAFARGUE
% Prenom : Héloïse
% Groupe : 1SN-C

function varargout = fonctions_TP3_stat(varargin)

    [varargout{1},varargout{2}] = feval(varargin{1},varargin{2:end});

end

% Fonction estimation_F (exercice_1.m) ------------------------------------
function [rho_F,theta_F,ecart_moyen] = estimation_F(rho,theta)

    A = [cos(theta), sin(theta)];
    B = rho;
    X = A\B;                             % matrice [xf, yf].'
    rho_F = sqrt(X(1)^2 + X(2)^2);
    theta_F = atan2(X(2), X(1));

    % A modifier lors de l'utilisation de l'algorithme RANSAC (exercice 2)
    % ecart_moyen = Inf;
    ecart_moyen = 1/ length(rho) * sum( abs(rho - rho_F*cos(theta - theta_F)));

end

% Fonction RANSAC_2 (exercice_2.m) ----------------------------------------
function [rho_F_estime,theta_F_estime] = RANSAC_2(rho,theta,parametres)

    S1 = parametres(1);
    S2 = parametres(2);
    kmax = parametres(3);
    n = length(rho);
    ecart_moy_min = Inf;
       
    for k = 1:kmax 
        % Récupérer 2 indices tirées aléatoirement entre 1 et n
        indices = randperm(n,2);
        % Estimer le modèle en appelant estimation_F
        [rho_etoile,theta_etoile,~] = estimation_F(rho(indices),theta(indices));  
        % Déterminer les données conformes avec le critère par rapport au seuil S1
        donnees_conformes = find( abs(rho - rho_etoile*cos(theta - theta_etoile)) < S1);
        % Calculer la proportion des données conformes 
        proportion = length(donnees_conformes) / n;
   
        % Accepter le modèle si la proportion de données conformes est supérieure au seuil S2
        if proportion >= S2
            % Réestier le modèle à partir des données conformes
            [~,~,ecart_moyen] = estimation_F(rho(donnees_conformes),theta(donnees_conformes));
            % Retenir le modèle qui minimise l'écart moyen des données conformes
            if ecart_moyen < ecart_moy_min
                [rho_F_estime,theta_F_estime,ecart_moy_min] = estimation_F(rho(donnees_conformes),theta(donnees_conformes));
            end
        end
    end

end

% La photo a été prise légèrement penchée par rapport au plan horizontal.

% Fonction G_et_R_moyen (exercice_3.m, bonus, fonction du TP1) ------------
function [G, R_moyen, distances] = ...
         G_et_R_moyen(x_donnees_bruitees,y_donnees_bruitees)



end

% Fonction estimation_C_et_R (exercice_3.m, bonus, fonction du TP1) -------
function [C_estime,R_estime,critere] = ...
         estimation_C_et_R(x_donnees_bruitees,y_donnees_bruitees,n_tests,C_tests,R_tests)
     
    % Attention : par rapport au TP1, le tirage des C_tests et R_tests est 
    %             considere comme etant deje effectue 
    %             (il doit etre fait au debut de la fonction RANSAC_3)



end

% Fonction RANSAC_3 (exercice_3, bonus) -----------------------------------
function [C_estime,R_estime] = ...
         RANSAC_3(x_donnees_bruitees,y_donnees_bruitees,parametres)
     
    % Attention : il faut faire les tirages de C_tests et R_tests ici



end
