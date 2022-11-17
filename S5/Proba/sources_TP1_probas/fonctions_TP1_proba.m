
% TP1 de Probabilites : fonctions a completer et rendre sur Moodle
% Nom : LAFARGUE
% Prénom : Héloïse
% Groupe : 1SN-C

function varargout = fonctions_TP1_proba(varargin)

    switch varargin{1}     
        case 'ecriture_RVB'
            varargout{1} = feval(varargin{1},varargin{2:end});
        case {'vectorisation_par_colonne','decorrelation_colonnes'}
            [varargout{1},varargout{2}] = feval(varargin{1},varargin{2:end});
        case 'calcul_parametres_correlation'
            [varargout{1},varargout{2},varargout{3}] = feval(varargin{1},varargin{2:end}); 
    end

end

% Fonction ecriture_RVB (exercice_0.m) ------------------------------------

function image_RVB = ecriture_RVB(image_originale)

[nb_lignes,nb_colonnes] = size(image_originale);
image_RVB = zeros(nb_lignes/2,nb_colonnes/2,3);

image_RVB(:,:,1) = image_originale(1:2:end, 2:2:end);
image_RVB(:,:,2) = (image_originale(1:2:end, 1:2:end) + image_originale(2:2:end, 2:2:end)) / 2
image_RVB(:,:,3) = image_originale(2:2:end, 1:2:end)
    
end

% Fonction vectorisation_par_colonne (exercice_1.m) -----------------------
function [Vd,Vg] = vectorisation_par_colonne(I)

[nb_lignes,nb_colonnes] = size(I);

Vd = I(:, 2:nb_colonnes);
Vg = I(:, 1:nb_colonnes-1);
Vd = Vd(:);
Vg = Vg(:);

end

% Fonction calcul_parametres_correlation (exercice_1.m) -------------------
function [r,a,b] = calcul_parametres_correlation(Vd,Vg)

moy_Vd = mean(Vd);
moy_Vg = mean(Vg);
ecart_type_Vd = sqrt( mean((Vd-moy_Vd).^2) );
ecart_type_Vg = sqrt( mean((Vg-moy_Vg).^2) );
cov = mean(Vd.*Vg)- moy_Vd*moy_Vg;
a = cov / (ecart_type_Vg ^2);
b = a*(-moy_Vg) + moy_Vd;
r = cov / (ecart_type_Vd * ecart_type_Vg);

end

% Fonction decorrelation_colonnes (exercice_2.m) --------------------------
function [I_decorrelee,I_min] = decorrelation_colonnes(I,I_max)

[nb_lignes,nb_colonnes] = size(I);
I_decorrelee = I;
I_decorrelee(:, 2:nb_colonnes) = I(:, 2:nb_colonnes) - I(:, 1:nb_colonnes-1);
I_min = -I_max;

end



