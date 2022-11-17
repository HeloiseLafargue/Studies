
% TP3 de Probabilites : fonctions a completer et rendre sur Moodle
% Nom : LAFARGUE
% Prénom : Héloïse
% Groupe : 1SN-C

function varargout = fonctions_TP3_proba(varargin)

    switch varargin{1}
        
        case 'matrice_inertie'
            
            [varargout{1},varargout{2}] = feval(varargin{1},varargin{2:end});
            
        case {'ensemble_E_recursif','calcul_proba'}
            
            [varargout{1},varargout{2},varargout{3}] = feval(varargin{1},varargin{2:end});
    
    end
end

% Fonction ensemble_E_recursif (exercie_1.m) ------------------------------
function [E,contour,G_somme] = ...
    ensemble_E_recursif(E,contour,G_somme,i,j,voisins,G_x,G_y,card_max,cos_alpha)
    
    contour(i,j) = 0;                           % Indique que le pixel courant a été traité

    for k = 1:length(voisins)
        
        i_vois = i + voisins(k,1);
        j_vois = j + voisins(k,2);
        
        if contour(i_vois, j_vois) ~= 0

            x = G_x(i_vois, j_vois);            % Récupère les coordonnées du voisin n°k
            y = G_y(i_vois, j_vois);
            vect_k = [x,y]    ;                 % Récupère le vecteur situé à ces coordonnées
            vect_k = vect_k / norm(vect_k);
        
            G_k = sum(G_somme, 1);
            G_k = G_k / norm(G_k);
        
            cos_angle = G_k(1)*vect_k(1) + G_k(2)*vect_k(2); % Cacule le cos de l'angle entre le vecteur n)k et la moyenne de G_somme 
              
            if (length(E) <= card_max) && (cos_angle >= cos_alpha) % Mettre à jour l'appel récursif si l'angle est ok

                E = [E; i_vois j_vois];
                G_somme = [G_somme; x y];
                [E, contour, G_somme] = ensemble_E_recursif(E,contour,G_somme,i_vois,j_vois,voisins,G_x,G_y,card_max,cos_alpha);
        
            end
        end
    end   
end

% Fonction matrice_inertie (exercice_2.m) ---------------------------------
function [M_inertie,C] = matrice_inertie(E,G_norme_E)

    PI = sum(G_norme_E);
    E = fliplr(E);
    
    moy_x = 1/PI * sum(G_norme_E.*E(:,1));
    moy_y = 1/PI * sum(G_norme_E.*E(:,2));

    M11 = 1/PI * sum(G_norme_E .* (E(:,1) - moy_x).^2);
    M12 = 1/PI * sum(G_norme_E .* (E(:,1) - moy_x).* (E(:,2) - moy_y));
    M22 = 1/PI * sum(G_norme_E .* (E(:,2) - moy_y).^2);
    C = [moy_x  moy_y];
    M_inertie = [ M11, M12; M12 , M22];

end

% Fonction calcul_proba (exercice_2.m) ------------------------------------
function [x_min,x_max,probabilite] = calcul_proba(E_nouveau_repere,p)

    x_min = min(E_nouveau_repere(:,1));
    x_max = max(E_nouveau_repere(:,1));
    y_min = min(E_nouveau_repere(:,2));
    y_max = max(E_nouveau_repere(:,2));
    
    l = length(E_nouveau_repere);
    n = round((y_max - y_min)*(x_max - x_min));
    probabilite = 1 - binocdf(l-1, n, p);


    
end