
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Fonction de decompression/reconstruction JPEG d'une image
%--------------------------------------------------------------------------
% I_Decodee = DecompressionJPEG(I_Quant,canal,F_Qualite,methode)
%
% sorties : I_Decodee = image reconstruite par quantification et DCT inverses
% 
% entrees : I_Codee = image de DCT quantifiee
%           canal = canal pour le choix de la table de quantification :
%                   'Luminance', 'Chrominance' ou 'Residu'
%           methode = methode de calcul de la IDCT : 'Matlab' ou 'Rapide'
%           F_Qualite = facteur de qualite pour la compression
%--------------------------------------------------------------------------
% Fonctions a coder/utiliser : QuantificationDCT.m
%                              DCT2DParBlocs.m
%--------------------------------------------------------------------------

function I_Decodee = DecompressionJPEG(I_Codee, canal, methode, F_Qualite)

    taille_bloc = 8;
    I_Quant = QuantificationDCT('Inverse',I_Codee,canal,F_Qualite,taille_bloc);
    I_Decodee = DCT2DParBlocs('Inverse',I_Quant,methode,taille_bloc);
end

