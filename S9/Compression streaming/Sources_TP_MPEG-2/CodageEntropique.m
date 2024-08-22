
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                                                      
%--------------------------------------------------------------------------
% Fonction de calcul d'entropie binaire
%--------------------------------------------------------------------------
% [poids, H] = CodageEntropique(V_coeff)
%
% sorties : poids = poids du vecteur de donnees encode (en ko)
%           H = entropie de la matrice (en bits/pixel)
% 
% entree  : V_coeff = vecteur contenant les symboles dont on souhaite 
%                     calculer l'entropie (ex : l'image vectorisee)
%--------------------------------------------------------------------------

function [poids, H] = CodageEntropique(V_coeff)
    symboles = unique(V_coeff);
    nb_symboles = length(symboles);
    occurences = zeros(1,nb_symboles);

    for s = 1:nb_symboles
        occurences(s) = sum(V_coeff == symboles(s));
    end

    frequences = occurences / sum(occurences);
    H = - sum(frequences .* log2(frequences));
    poids = H * length(V_coeff) / (8*1024); % conversion de 8 bits en ko  
    
end
