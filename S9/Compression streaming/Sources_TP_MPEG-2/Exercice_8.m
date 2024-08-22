
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercice_8 : Codage/decodage MPEG avec entropie et PSNR
%--------------------------------------------------------------------------
% Fonctions a coder / utiliser : CompressionJPEG.m
%                                CompressionMPEG.m
%                                DecompressionJPEG.m
%                                DecompressionMPEG.m
%--------------------------------------------------------------------------

clear;
close all;
clc;

taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 8 - Test de la compression/decompression MPEG',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Creation de l'image residuelle
Ir_for_MPEG = load('Donnees_TP_MPEG-2.mat').Ir_for_MPEG;
Ic_for_MPEG = load('Donnees_TP_MPEG-2.mat').Ic_for_MPEG;
% Choix du canal pour la quantification
canal = 'Luminance';
% Methode de calcul de la DCT 2D par blocs ('Matlab' ou 'Rapide')
methode = 'Matlab';
% Choix du facteur de qualite (ici vecteur allant de 1% a 97%)
F_Qualite_Max = 97;
F_Qualite = 1:F_Qualite_Max;
% Vecteur pour recuperer le poids de l'image pour chaque facteur de qualite
vecteur_Poids = zeros(2,length(F_Qualite));
% Vecteur pour recuperer le PSNR de l'image pour chaque facteur de qualite
vecteur_PSNR = zeros(2,length(F_Qualite));
% Traitement pour chaque facteur de qualite
for f = 1:length(F_Qualite)
    % Compression
    [Ic_Codee,PCou] = CompressionJPEG(Ic_for_MPEG,canal,methode,f);
    [IRes_Codee,MVdr,Ir_Codee,PRes,Compression] = ...
                  CompressionMPEG(Ic_for_MPEG,Ir_for_MPEG,canal,methode,f);
    % Reconstruction
    Ic_Decodee_JPEG = DecompressionJPEG(Ic_Codee,canal,methode,f);
    [Ic_Decodee_MPEG,Ir_Decodee] = ...
               DecompressionMPEG(IRes_Codee,Ir_Codee,MVdr,canal,methode,f);
    % Recuperation du poids des coefficients AC/DC separes
    vecteur_Poids(1,f) = PCou.H_JPEG;
    vecteur_Poids(2,f) = PRes.H_MPEG;
    % Calcul et recuperation du PSNR
    vecteur_PSNR(1,f) = psnr(uint8(Ic_Decodee_JPEG),uint8(Ic_for_MPEG));
    vecteur_PSNR(2,f) = psnr(uint8(Ic_Decodee_MPEG),uint8(Ic_for_MPEG));
    % Affichage de l'image d'origine
    subplot 222
        imagesc(uint8(Ic_Decodee_JPEG))
        colormap gray
        axis image off
        title({['Image reconstruite (JPEG) pour F_q = ' num2str(f)] ...
               ['(Poids = ' num2str(PCou.H_JPEG,'%.3g') ' ko ' ...
                'et PSNR = ' num2str(vecteur_PSNR(1,f),'%.3g') ' dB)']})
        set(gca,'FontSize',15)
    % Affichage de l'image reconstruite pour un facteur donne
    subplot 224
        imagesc(uint8(Ic_Decodee_MPEG))
        colormap gray
        axis image off
        title({['Image reconstruite (MPEG) pour F_q = ' num2str(f)] ...
               ['(Poids = ' num2str(PRes.H_MPEG,'%.3g') ' ko ' ...
                'et PSNR = ' num2str(vecteur_PSNR(2,f),'%.3g') ' dB)']})
        set(gca,'FontSize',15)
    % Courbes du poids et du PSNR en fonction du facteur de qualite
    subplot 121
        % Courbe du PSNR
        yyaxis left
        plot(1:f, vecteur_PSNR(:,1:f))
        xlim([1 97])
        ylim([15 55])
        xlabel('F_q')
        ylabel('PSNR (dB)')
        % Courbe du poids
        yyaxis right
        semilogy(1:f, vecteur_Poids(:,1:f),...
                 1:F_Qualite_Max,PCou.Origine*ones(1,F_Qualite_Max),'.')
        ylim([0 1.1*PCou.Origine])
        ylabel('Poids (ko)')
        title('Courbes de PSNR et de poids de l''image compressee')
        set(gca,'FontSize',15)
        legend('PSNR avec compression JPEG',...
               'PSNR avec compression MPEG',...
               'Poids de la compression JPEG',...
               'Poids de la compression MPEG',...
               'Poids d''origine',...
               'Location', 'NorthWest')
        grid on
    drawnow;
    
end