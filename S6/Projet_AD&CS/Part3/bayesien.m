% Calcul de la personne la plus vraisemblable en utilisant la
% classification bayésienne
% 
% entrées: - C matrice de la représentation compacte des images de données
%          - C_requete matrice de la représentation compacte de l'image requête

% sortie: personne et posture la plus vraisemblable dans la base de données


function [personne_proche,posture_proche] = bayesien(C, C_requete)

    Pmax = 0;
    [nb_personnes_base,~] = size(C);

    for i = 1:nb_personnes_base
        image_compacte_i = C(i,:);

        [mu, sigma] = estimation_mu_Sigma(image_compacte_i');
        P = gaussienne(C_requete, mu, sigma);

        if P >= Pmax
            Pmax = P;
            posture_proche = mod(i,6);
            if posture_proche == 0
                posture_proche = 6;
            end
            personne_proche = fix(i/6) + 1;
        end

    end
    
end