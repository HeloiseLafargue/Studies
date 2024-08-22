
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022

%--------------------------------------------------------------------------
% Fonction d'encodage MPEG d'une image (+ reference en JPEG)
%--------------------------------------------------------------------------
% [IRes_Codee,MVdr,Ir_Codee,Poids,Compression] = ...
%        CompressionMPEG(Ic_Originale,Ir_Originale,canal,methode,F_Qualite)
%
% sorties : IRes_Codee = image residuelle (DCT quantifiee)
%           MVdr = matrice des vecteurs de deplacements relatifs
%           Ir_Codee = image de reference (DCT quantifiee)
%           Poids = poids de l'image en ko pour les differentes etapes de 
%                   la compression (inclure les mouvements pour MPEG)
%           Compression = taux de compression final
% 
% entrees : Ic_Originale = image courante originale
%           Ir_Originale = image de reference originale
%           canal = canal pour le choix de la table de quantification :
%                   'Luminance', 'Chrominance' ou 'Residu'
%           methode = methode de calcul de la DCT : 'Matlab' ou 'Rapide'
%           F_Qualite = facteur de qualite pour la compression
%--------------------------------------------------------------------------
% Fonctions a coder/utiliser : EstimationMouvement.m
%                              PredictionImage.m
%                              CompressionJPEG.m
%--------------------------------------------------------------------------

function [IRes_Codee,MVdr,Ir_Codee,Poids,Compression] = ...
         CompressionMPEG(Ic_Originale,Ir_Originale,canal,methode,F_Qualite)
    
    IRes_Codee = [];
    MVdr = [];
    Ir_Codee = [];
    Poids = [];
    Compression = [];
    Poids = struct('Origine', 0, 'H_JPEG', 0, 'H_MPEG', 0, 'MVdr', 0);

    % Calcul des cartes de mouvements MVdr des macro-blocs de l'image courante Ic_Originale
    MVdr = EstimationMouvement(Ic_Originale, Ir_Originale);

    % Calcul de la prédiction Ip de l'image courante Ic_Originale à partir de l'image de référence Ir_Originale et des mouvements MVdr
    Ip = PredictionImage(Ir_Originale, MVdr);

    % Calcul de l'image résiduelle IRes comme la différence entre l'image courante Ic_Originale et l'image prédite Ip
    IRes = Ic_Originale - Ip;

    % Compression JPEG de l'image de référence Ir_Originale (canal Luminance)
    [Ir_Codee,~,~,~,~] = CompressionJPEG(Ir_Originale, 'Luminance', methode, F_Qualite);

    % Compression JPEG de l'image résiduelle IRes (canal Residu)
    [IRes_Codee, Poids_Res,~,~,~] = CompressionJPEG(IRes, 'Residu', methode, F_Qualite);

    % Codage entropique des mouvements MVdr et ajout à l'image résiduelle IRes
    Poids.Origine = length(Ic_Originale(:)) / 1024;
    Poids.H_JPEG = Poids_Res.H_JPEG;
    Poids.MVdr = CodageEntropique(MVdr(:));
    Poids.H_MPEG = Poids.MVdr + Poids.H_JPEG;
    
end

