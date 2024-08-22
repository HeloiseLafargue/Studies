
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercice_7 : Compression MPEG et calcul d'entropie
%--------------------------------------------------------------------------
% Fonctions a coder / utiliser : CompressionJPEG.m
%                                CompressionMPEG.m
%--------------------------------------------------------------------------

clear;
close all;
clc;

taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 7 - Test de la compression MPEG',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement des images de test
Ir_for_MPEG = load('Donnees_TP_MPEG-2.mat').Ir_for_MPEG;
Ic_for_MPEG = load('Donnees_TP_MPEG-2.mat').Ic_for_MPEG;
% Choix du canal pour la quantification
canal = 'Luminance';
% Methode de calcul de la DCT 2D par blocs ('Matlab' ou 'Rapide')
methode = 'Matlab';
% Choix du facteur de qualite (ici vecteur allant de 1% a 97%)
F_Qualite = 30;
% Compression JPEG avec entropie des coefficients AC/DC separes
[Ic_Codee,PCou] = CompressionJPEG(Ic_for_MPEG,canal,methode,F_Qualite);
[IRes_Codee,MVdr,Ir_Codee,PRes,Compression] = ...
          CompressionMPEG(Ic_for_MPEG,Ir_for_MPEG,canal,methode,F_Qualite);
% Affichage de l'image avant compression
subplot 211
    imagesc(Ic_for_MPEG)
    colormap gray
    axis image off
    title({'Image courante d''origine' ...
           ['Poids = ' num2str(PRes.Origine,'%.3g') ' ko (attendu : 412 ko)']})
    set(gca,'FontSize',15)
% Affichage de l'image courante apres DCT et quantification
subplot 223
    imagesc(abs(Ic_Codee))
    colormap gray
    axis image off
    title({'Image courante quantifiee' ...
           ['Poids = ' num2str(PCou.H_JPEG,'%.3g') ' ko (attendu : 13.3 ko)']})
    set(gca,'FontSize',15)
% Affichage de l'image residuelle apres DCT et quantification
subplot 224
    imagesc(abs(IRes_Codee))
    colormap gray
    axis image off
    title({'Image residuelle quantifiee de l''image courante' ...
           ['Poids total = ' num2str(PRes.H_MPEG,'%.3g') ' ko (attendu : 5.62 ko)'] ...
           ['Poids des mouvements = ' num2str(PRes.MVdr,'%.3g') ' ko (attendu : 1.24 ko)']})
    set(gca,'FontSize',15)
