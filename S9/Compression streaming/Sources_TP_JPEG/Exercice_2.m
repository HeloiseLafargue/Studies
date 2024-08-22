
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercie_2 : Test de la DCT 2D par blocs
%--------------------------------------------------------------------------
% Fonction a coder/utiliser : DCT2DParBlocs.m
%--------------------------------------------------------------------------
 
clear
close all
clc
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 2 - Test de la DCT 2D par blocs',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement de l'image de test
I_for_DCT = load('Donnees_TP_MPEG-2.mat').I_for_DCT;
% Traitement de la DCT 2D par blocs
taille_bloc = 8;
% Calcul de la DCT 2D par blocs
%I_DCT = uint8(DCT2DParBlocs('Direct',I_for_DCT,'Matlab',taille_bloc));
%I_DCT = uint8(DCT2DParBlocs('Direct',I_for_DCT,'Rapide',taille_bloc));
I_DCT = uint8(DCT2DParBlocs('Inverse',I_for_DCT,'Rapide',taille_bloc));
% Affichage de l'image avant DCT (DCT inverse effectuee)
subplot 121
    imagesc(abs(I_for_DCT))
    colormap gray
    axis image off
    title({'Image avant la DCT :' ...
           'DCT inverse effectuee'})
    set(gca,'FontSize',15)
% Affichage de l'image apres DCT
subplot 122
    imagesc(I_DCT)
    colormap gray
    axis image off
    if (round(sum(I_DCT(:))/length(I_DCT(:))) == 178)
        title({'Image apres la DCT :' ...
               ' bonne transformation'})
    else
        title({'Image apres la DCT :' ...
                'mauvaise transformation'})
    end
    set(gca,'FontSize',15)
