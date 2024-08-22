
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercice_5 : Compression/decompression JPEG avec entropie et PSNR
%--------------------------------------------------------------------------
% Fonctions a coder/utiliser : CompressionJPEG.m
%                              DecompressionJPEG.m
%--------------------------------------------------------------------------

clear;
close all;
clc;

taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 5 - Test de la compression/decompression JPEG',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement de l'image de test
I_for_JPEG = load('Donnees_TP_MPEG-2.mat').I_for_JPEG;
% Choix du canal pour la quantification
canal = 'Luminance';
% Methode de calcul de la DCT 2D par blocs ('Matlab' ou 'Rapide')
methode = 'Rapide';
% Choix du facteur de qualite (ici vecteur allant de 1% a 97%)
F_Qualite_Max = 97;
F_Qualite = 1:F_Qualite_Max;
% Vecteur pour recuperer le poids de l'image pour chaque facteur de qualite
vecteur_Poids = zeros(1,length(F_Qualite));
% Vecteur pour recuperer le PSNR de l'image pour chaque facteur de qualite
vecteur_PSNR = zeros(1,length(F_Qualite));
% Traitement pour chaque facteur de qualite
for f = 1:length(F_Qualite)
    % Compression de l'image
    [I_Codee,Poids] = CompressionJPEG(I_for_JPEG,canal,methode,f);
    % Decompression de l'image
    I_Decodee = DecompressionJPEG(I_Codee,canal,methode,f);
    % Recuperation du poids des coefficients AC/DC separes
    vecteur_Poids(f) = Poids.H_JPEG;
    % Calcul et recuperation du PSNR (en uint8 !)
    vecteur_PSNR(f) = psnr(uint8(I_Decodee),uint8(I_for_JPEG));
    % Affichage de l'image d'origine (en uint8)
    subplot 221
        imagesc(uint8(I_for_JPEG))
        colormap gray
        axis image off
        title(['Image d''origine (Poids = ' num2str(Poids.Origine,'%.3g') 'ko)'])
        set(gca,'FontSize',15)
    % Affichage de l'image reconstruite pour un facteur donne (en uint8)
    subplot 122
        imagesc(uint8(I_Decodee))
        colormap gray
        axis image off
        title({['Image reconstruite pour F_q = ' num2str(f)] ...
               ['Poids = ' num2str(Poids.H_JPEG,'%.3g') ' ko'] ...
               ['PSNR = ' num2str(vecteur_PSNR(f),'%.3g') ' dB']})
        set(gca,'FontSize',15)
    % Courbes du poids et du PSNR en fonction du facteur de qualite
    subplot 223
        % Courbe du PSNR
        yyaxis left
        plot(1:f, vecteur_PSNR(1:f))
        xlim([1 97])
        ylim([10 50])
        xlabel('F_q')
        ylabel('PSNR (dB)')
        % Courbe du poids
        yyaxis right
        semilogy(1:f, vecteur_Poids(1:f),...
                 1:F_Qualite_Max,Poids.Origine*ones(1,F_Qualite_Max),'.')
        ylim([0 1.1*Poids.Origine])
        ylabel('Poids (ko)')
        title('Courbes de PSNR et de poids de l''image compressee')
        set(gca,'FontSize',15)
        legend('PSNR',...
               'Poids de la compression',...
               'Poids d''origine',...
               'Location', 'NorthWest')
        grid on
    drawnow;
    
end
% Affichage de l'image reconstruite pour un facteur donne (en uint8)
subplot 122
    imagesc(uint8(I_Decodee))
    colormap gray
    axis image off
    title({['Image reconstruite pour F_q = ' num2str(f)] ...
           ['Poids = ' num2str(Poids.H_JPEG,'%.3g') ' ko (attendu : 30.7 ko)'] ...
           ['PSNR = ' num2str(vecteur_PSNR(f),'%.3g') ' (attendu : 47.4)']})
    set(gca,'FontSize',15)

