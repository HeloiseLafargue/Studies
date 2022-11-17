
% TP2 de Probabilites : fonctions a completer et rendre sur Moodle
% Nom : LAFARGUE
% Prenom : Héloïse
% Groupe : 1SN-C

function varargout = fonctions_TP2_proba(varargin)

    switch varargin{1}
        
        case {'calcul_frequences_caracteres','determination_langue','coeff_compression','gain_compression','partitionnement_frequences'}

            varargout{1} = feval(varargin{1},varargin{2:end});
            
        case {'recuperation_caracteres_presents','tri_decroissant_frequences','codage_arithmetique'}
            
            [varargout{1},varargout{2}] = feval(varargin{1},varargin{2:end});
            
        case 'calcul_parametres_correlation'
            
            [varargout{1},varargout{2},varargout{3}] = feval(varargin{1},varargin{2:end});
            
    end

end

% Fonction calcul_frequences_caracteres (exercice_0.m) --------------------
function frequences = calcul_frequences_caracteres(texte,alphabet)
    n = length(alphabet);
    frequences = zeros(n,1);
    for i = 1:n
        frequences(i) = sum(texte == alphabet(i)); 
    end
    frequences = frequences/sum(frequences);
end

% Fonction recuperation_caracteres_presents (exercice_0.m) ----------------
function [selection_frequences,selection_alphabet] = ...
                      recuperation_caracteres_presents(frequences,alphabet)
    
    indices = find(frequences > 0);
    selection_frequences = frequences(indices);
    selection_alphabet = alphabet(frequences > 0);
    
end

% Fonction tri_decremental_frequences (exercice_0.m) ----------------------
function [frequences_triees,indices_frequences_triees] = ...
                           tri_decroissant_frequences(selection_frequences)
    
    [frequences_triees, indices_frequences_triees] = sort(selection_frequences, 'descend');


end

% Fonction determination_langue (exercice_1.m) ----------------------------
function langue = determination_langue(frequences_texte, frequences_langue, nom_norme)
    % Note : la variable nom_norme peut valoir 'L1', 'L2' ou 'Linf'.
    mat_freq = repmat(frequences_texte', size(frequences_langue,1), 1);
    mat_error = (mat_freq - frequences_langue);
    switch nom_norme
        case 'L2'
            error = sum(mat_error.^2, 2);
        case 'L1'
            error = sum(abs(mat_error), 2);
        case 'Linf'
            error = max(abs(mat_error), [], 2);
    end
    [val_min, indice_min] = min(error);
    langue = indice_min;

end

% Fonction coeff_compression (exercice_2.m) -------------------------------
function coeff_comp = coeff_compression(signal_non_encode,signal_encode)

    coeff_comp = length(signal_non_encode) / length(signal_encode);

end

% Fonction coeff_compression (exercice_2bis.m) -------------------------------
function gain_comp = gain_compression(coeff_comp_avant,coeff_comp_apres)

    gain_comp = coeff_comp_apres / coeff_comp_avant;

end

% Fonction partitionnement_frequences (exercice_3.m) ----------------------
function bornes = partitionnement_frequences(selection_frequences)

    n = length(selection_frequences);
    bornes = zeros(2, n);
    borne_sup_int = selection_frequences(1);
    for k = 1:n-1
        bornes(1,k+1) = borne_sup_int;
        bornes(2,k) = borne_sup_int;
        borne_sup_int = borne_sup_int + selection_frequences(k+1);
    end
    bornes(2,n) = borne_sup_int;
end

% Fonction codage_arithmetique (exercice_3.m) -----------------------------
function [borne_inf,borne_sup] = ...
                       codage_arithmetique(texte,selection_alphabet,bornes)

    % Initialisation
    borne_inf = 0;
    borne_sup = 1;
    
    % Pour chaque caractère de l'alphabet j dans le message
    for j = 1:length(texte)
        indice = find(selection_alphabet == texte(j));
        largeur = borne_sup-borne_inf;
        borne_sup = borne_inf+largeur*bornes(2,indice);
        borne_inf = borne_inf+largeur*bornes(1,indice);
    end
    
    % Choix d'un nombre quelconque dans l'intervalle [borne_inf,borne_sup] pour encoder le texte
    k = rand*(borne_sup-borne_inf)+borne_inf;
   
end