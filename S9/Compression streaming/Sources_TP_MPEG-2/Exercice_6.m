
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercice_6 : Prediction d'image et calcul de residu
%--------------------------------------------------------------------------
% Fonctions a coder/utiliser : EstimationMouvement.m
%                              PredictionImage.m
%--------------------------------------------------------------------------

clear;
close all;
clc;

taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 6 - Prediction d''image par Cross Search Algorithm',...
       'Position',[0.1*L,0.1*H,0.8*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement des images de test (image de reference)
Ir_for_Res = load('Donnees_TP_MPEG-2.mat').Ir_for_Res;
% Affichage de l'image de reference
subplot 231
    imagesc(uint8(Ir_for_Res))
    axis image off
    colormap gray
    title('Image de reference (Ir)')
    set(gca,'FontSize',12)
% Chargement des images de test (image courante)    
Ic_for_Res = load('Donnees_TP_MPEG-2.mat').Ic_for_Res;
% Affichage de l'image courante
subplot 232
    imagesc(uint8(Ic_for_Res))
    axis image off
    colormap gray
    title('Image courante (Ic)')
    set(gca,'FontSize',12)
% Affichage de l'image de differences
subplot 233
    imagesc(abs(Ir_for_Res-Ic_for_Res))
    axis image off
    colormap gray
    title('Image de differences (Ir - Ic en valeur absolue)')
    set(gca,'FontSize',12)
% Estimation du mouvement entre les images
MVr = EstimationMouvement2(Ic_for_Res, Ir_for_Res);
% Affichage du flux optique    
subplot 234
    imagesc(ColorationFluxOptique(MVr(:,:,1),MVr(:,:,2)))
    axis image off
    title({'Mouvements relatifs des blocs' ...
           ['Somme des mouvements : ' num2str(sum(abs(MVr(:)))) ' (attendue : 5553)']})
    set(gca,'FontSize',12)
% Prediction de l'image courante avec l'image de reference
Ip = PredictionImage(Ir_for_Res, MVr);
% Affichage de l'image predictive
subplot 235
    imagesc(uint8(Ip))
    axis image off
    colormap gray
    title({'Image predictive (Ip)' ...
           ['Niveau de gris moyen : ' num2str(mean(uint8(Ip(:))),'%.4g') ' (attendu : 82.48)']})
    set(gca,'FontSize',12)
% Calcul de l'image residuelle
Ires = Ic_for_Res - Ip;
% Affichage de l'image residuelle
subplot 236
    imagesc(abs(Ires))
    colormap gray
    axis image off
    title('Image residuelle (Ip - Ic en valeur absolue)')
    set(gca,'FontSize',12)
