
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercie_3 : Test de la quantification par blocs
%--------------------------------------------------------------------------
% Fonction a coder/utiliser : QuantificationDCT.m
%--------------------------------------------------------------------------

clear
close all
clc
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 3 - Test de la quantification par blocs',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement de l'image de test
I_for_Quant = load('Donnees_TP_MPEG-2.mat').I_for_Quant;
% Facteur de qualite de la quantification
F_Qualite = 1;
% Traitement de la quantification par blocs
taille_bloc = 8;
% Quantification de l'image
I_Quant = uint8(QuantificationDCT('Direct',I_for_Quant,'Luminance',F_Qualite,taille_bloc));
% Affichage de l'image avant quantification (quantification inverse effectuee)
subplot 121
    imagesc(I_for_Quant)
    colormap gray
    axis image off
    title({'Image avant quantification :'...
           'quantification inverse effectuee'})
    set(gca,'FontSize',15)
% Affichage de l'image apres quantification
subplot 122
    imagesc(I_Quant)
    colormap gray
    axis image off
    if (sum(I_Quant(:)) == 300150)
        title({'Image apres quantification :'...
               'bonne quantification'})
    else
        title({'Image apres quantification :'...
               'mauvaise quantification'})
    end
    set(gca,'FontSize',15)
