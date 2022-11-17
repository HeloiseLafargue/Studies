
% TP2 de Statistiques : fonctions a completer et rendre sur Moodle
% Nom : Lafargue  
% Prénom : Héloïse
% Groupe : 1SN-C

function varargout = fonctions_TP2_stat(varargin)

    [varargout{1},varargout{2}] = feval(varargin{1},varargin{2:end});

end

% Fonction centrage_des_donnees (exercice_1.m) ----------------------------
function [x_G, y_G, x_donnees_bruitees_centrees, y_donnees_bruitees_centrees] = ...
                centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees)

    x_G = mean(x_donnees_bruitees);                         % Coordonnées du centre de gravité
    y_G = mean(y_donnees_bruitees);
    x_donnees_bruitees_centrees = x_donnees_bruitees - x_G; % Vecteurs centrés des données
    y_donnees_bruitees_centrees = y_donnees_bruitees - y_G;   
     
end

% Fonction estimation_Dyx_MV (exercice_1.m) -------------------------------
function [a_Dyx,b_Dyx] = ...
           estimation_Dyx_MV(x_donnees_bruitees,y_donnees_bruitees,n_tests)
    
    [x_G, y_G,x_prime, y_prime] = centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);
    psi = pi*rand(n_tests,1) - pi/2;            % angle psi entre -pi/2 et pi/2
    y_i = repmat(y_prime, n_tests, 1)
    terme = (y_i - tan(psi)*x_prime).^2;        % terme de la somme
    [val, ind] = min(sum(terme,2));             % on somme sur les colonnes
    a_Dyx = tan(psi(ind));
    b_Dyx = y_G - a_Dyx*x_G;
    
end

% Fonction estimation_Dyx_MC (exercice_2.m) -------------------------------
function [a_Dyx,b_Dyx] = ...
                   estimation_Dyx_MC(x_donnees_bruitees,y_donnees_bruitees)
    
    A = [x_donnees_bruitees; ones(1, length(x_donnees_bruitees))].';
    B = y_donnees_bruitees.';
    A_plus = (A.'*A)\A.';
    X = A_plus*B;               % résolution du système linéaire AX = B
    a_Dyx = X(1);
    b_Dyx = X(2);
    
end

% Fonction estimation_Dorth_MV (exercice_3.m) -----------------------------
function [theta_Dorth,rho_Dorth] = ...
         estimation_Dorth_MV(x_donnees_bruitees,y_donnees_bruitees,n_tests)
    
    [x_G, y_G,x_prime, y_prime] = centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);
    theta = pi*rand(n_tests,1);                                  % theta angle entre  et pi
    
    terme = (cos(theta)*x_prime + sin(theta)*y_prime).^2;        % terme de la somme
    [val, ind] = min(sum(terme,2));                              % on somme sur les colonnes
    theta_Dorth = theta(ind);
    rho_Dorth = x_G*cos(theta_Dorth) + y_G*sin(theta_Dorth);

end

% Fonction estimation_Dorth_MC (exercice_4.m) -----------------------------
function [theta_Dorth,rho_Dorth] = ...
                 estimation_Dorth_MC(x_donnees_bruitees,y_donnees_bruitees)
    
    [x_G, y_G,x_prime, y_prime] = centrage_des_donnees(x_donnees_bruitees,y_donnees_bruitees);
    C = [x_prime; y_prime].';
    [vec, val] = eigs(C.'*C);                     
    [~, ind] = min(diag(val));                            % on obtient la plus petite valeur propre de C(T)C
    vec_propre = vec(:,ind);                              % le vecteur propre associé à la plus petite valeur propre de C(T)C
    theta_Dorth = atan2(vec_propre(2),vec_propre(1));     % on en déduit l'angle theta_Dorth, vec_propre = [ sin(theta_Dorth); cos(theta_Dorth)].'   
    rho_Dorth = x_G*cos(theta_Dorth) + y_G*sin(theta_Dorth);

end
