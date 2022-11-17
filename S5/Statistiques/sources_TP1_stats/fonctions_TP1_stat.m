
% TP1 de Statistiques : fonctions a completer et rendre sur Moodle
% Nom : LAFARGUE
% Prénom : Héloïse 
% Groupe : 1SN-C

function varargout = fonctions_TP1_stat(varargin)

    [varargout{1},varargout{2}] = feval(varargin{1},varargin{2:end});

end

% Fonction G_et_R_moyen (exercice_1.m) ----------------------------------
function [G, R_moyen, distances] = ...
         G_et_R_moyen(x_donnees_bruitees,y_donnees_bruitees)

    G = [mean(x_donnees_bruitees) , mean(y_donnees_bruitees)];
    d_x = x_donnees_bruitees - G(1);
    d_y = y_donnees_bruitees - G(2);
    distances = sqrt(d_x.^2 + d_y.^2);
    R_moyen = mean(distances);
     
end

% Fonction estimation_C_uniforme (exercice_1.m) ---------------------------
function [C_estime, R_moyen] = ...
         estimation_C_uniforme(x_donnees_bruitees,y_donnees_bruitees,n_tests)
 
    [G, R_moyen, distances] =   G_et_R_moyen(x_donnees_bruitees,y_donnees_bruitees);  
    mat_C_test = 2*R_moyen*rand(n_tests,2) + G - R_moyen ;  % la matrice générant selon une loi uniforme les centres aléatoires C à tester

     % version possible avec matlab car répétition automatique
    % D_x = x_donnees_bruitees - mat_C_test(:,1);            
    % D_y = y_donnees_bruitees - mat_C_test(:,2);
 
    D_x = repmat(x_donnees_bruitees, n_tests, 1) - repmat(mat_C_test(:,1), 1 , length(x_donnees_bruitees));
    D_y = repmat(y_donnees_bruitees, n_tests, 1) - repmat(mat_C_test(:,2), 1 , length(y_donnees_bruitees));

    D = sqrt(D_x.^2 + D_y.^2);                     % la matrice des distances entre les Pi et les C à tester
    [val, ind] = min(sum((D - R_moyen).^2,2));     % on somme sur l'indice de colonne, d'où sum(...,2)
    C_estime = mat_C_test(ind, :);

end

% Fonction estimation_C_et_R_uniforme (exercice_2.m) ----------------------
function [C_estime, R_estime] = ...
         estimation_C_et_R_uniforme(x_donnees_bruitees,y_donnees_bruitees,n_tests)

    [G, R_moyen, distances] =   G_et_R_moyen(x_donnees_bruitees,y_donnees_bruitees);  
    mat_C_test = 2*R_moyen*rand(n_tests,2) + G - R_moyen ;
    mat_R_test = R_moyen*rand(n_tests,1) + R_moyen/2 ; % on génère la matrice des rayons à tester, ils sont donnés suivant une loi uniforme centrée en R_moyen

    D_x = repmat(x_donnees_bruitees, n_tests, 1) - repmat(mat_C_test(:,1), 1 , length(x_donnees_bruitees));
    D_y = repmat(y_donnees_bruitees, n_tests, 1) - repmat(mat_C_test(:,2), 1 , length(y_donnees_bruitees));

    D = sqrt(D_x.^2 + D_y.^2);                     
    [val, ind] = min(sum((D - mat_R_test).^2,2));
    C_estime = mat_C_test(ind,:);
    R_estime = mat_R_test(ind,:);

end

% Fonction occultation_donnees (donnees_occultees.m) ----------------------
function [x_donnees_bruitees, y_donnees_bruitees] = ...
         occultation_donnees(x_donnees_bruitees, y_donnees_bruitees, theta_donnees_bruitees)
    
    % On génère aléatoirement deux angles compris entre 0 et 2PI
    theta_1 = 2*pi*rand();
    theta_2 = 2*pi*rand();
    
    if theta_1 <= theta_2 
        ind = find(theta_1 <= theta_donnees_bruitees & theta_donnees_bruitees <= theta_2);
    else
        ind = find(0 <= theta_donnees_bruitees & theta_donnees_bruitees<= theta_2| theta_1 <= theta_donnees_bruitees & theta_donnees_bruitees< 2*pi);
    end   

    x_donnees_bruitees(ind) = [];
    y_donnees_bruitees(ind) = [];

end

% Fonction estimation_C_et_R_normale (exercice_4.m, bonus) ----------------
function [C_estime, R_estime] = ...
         estimation_C_et_R_normale(x_donnees_bruitees,y_donnees_bruitees,n_tests)



end
