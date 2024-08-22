
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022

%--------------------------------------------------------------------------
% Prediction de l'image courante avec l'image de reference et le mouvement
%--------------------------------------------------------------------------
% Ip = PredictionImage(Ir,MVr)
%
% sortie  : Ip = image predictive
%           
% entrees : Ir = image de reference
%           MVr = matrice des vecteurs de déplacements relatifs
%--------------------------------------------------------------------------

function Ip = PredictionImage(Ir,MVr)
    
    [m, n] = size(Ir);  
    MB_size = 16;
    Ip = zeros(m, n);

    for i = 1:size(MVr, 1)
        for j = 1:size(MVr, 2)

            deplacement = MVr(i, j, :);

            % Coordonnées du macro-bloc prédit
            MB_x = (i - 1) * MB_size + 1;
            MB_y = (j - 1) * MB_size + 1;

            % Coordonnées du macro-bloc référence
            Ref_x = MB_x + deplacement(1);
            Ref_y = MB_y + deplacement(2);

            % Vérifier que les coordonnées ne sortent pas des limites de l'image
            if Ref_x >= 1 && Ref_x + MB_size - 1 <= m && Ref_y >= 1 && Ref_y + MB_size - 1 <= n
                % Copier le macro-bloc de l'image de référence dans l'image prédite
                Ip(MB_x:MB_x+MB_size-1, MB_y:MB_y+MB_size-1) = Ir(Ref_x:Ref_x+MB_size-1, Ref_y:Ref_y+MB_size-1);
            end
        end
    end

end
