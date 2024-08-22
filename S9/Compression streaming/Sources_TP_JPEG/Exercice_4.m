
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercice_4 : Compression JPEG et calcul d'entropie
%-------------------------------------------------------------------------- 
% Fonction a coder/utiliser : CompressionJPEG.m
%--------------------------------------------------------------------------

clear;
close all;
clc;

taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 4 - Test de la compression JPEG',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement de l'image de test
I_for_JPEG = load('Donnees_TP_MPEG-2.mat').I_for_JPEG;
% Choix du canal pour la quantification
canal = 'Luminance';
% Methode de calcul de la DCT 2D par blocs ('Matlab' ou 'Rapide')
methode = 'Matlab';
% Choix du facteur de qualite
F_Qualite = 30;
% Compression JPEG avec entropie des coefficients AC/DC separes
[I_Codee,Poids,Compression,nb_coeffs_AC,nb_coeffs_DC] = ...
                       CompressionJPEG(I_for_JPEG,canal,methode,F_Qualite);
% Affichage de l'image avant compression
subplot 121
    imagesc(uint8(I_for_JPEG))
    colormap gray
    axis image off
    title(['Image d''origine (Poids = ' num2str(Poids.Origine,'%.0f') ' ko)'])
    set(gca,'FontSize',15)
% Affichage de l'image apres DCT et quantification
subplot 122
    imagesc(abs(I_Codee))
    colormap gray
    axis image off
    title({'Image quantifiee' ...
           ['Poids de l''image = ' num2str(Poids.H_JPEG,'%.3g') ' ko (attendu : ' num2str(4.97) ' ko)'] ...
           ['Compression = ' num2str(Compression,'%.3g') '% (attendu : ' num2str(92.2) '%)'] ...
           ['Nombre de coefficients AC = ' num2str(nb_coeffs_AC) ' (attendu : 12498)'] ...
           ['Nombre de coefficients DC = ' num2str(nb_coeffs_DC) ' (attendu : ' num2str(length(I_for_JPEG(:))/64) ')']})
    set(gca,'FontSize',11)
