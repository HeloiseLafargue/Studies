
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                                 
%--------------------------------------------------------------------------
% Exercie_1 : Test du codage entropique
%--------------------------------------------------------------------------
% Fonction a coder/utiliser : CodageEntropique.m
%--------------------------------------------------------------------------
 
clear
close all
clc
taille_ecran = get(0,'ScreenSize');
L = taille_ecran(3);
H = taille_ecran(4);
figure('Name','Exercice 1 - Test du codage entropique',...
       'Position',[0.2*L,0.1*H,0.6*L,0.7*H],...
       'Color',[0.7 0.75 0.85]);

%--------------------------------------------------------------------------

% Chargement des images de test
I_Ent_1 = load('Donnees_TP_MPEG-2.mat').I_Ent_1;
I_Ent_2 = load('Donnees_TP_MPEG-2.mat').I_Ent_2;
% Calcul de l'entropie avec la fonction Matlab 
% (seulement pour une image en niveaux de gris)
H_ref = entropy(I_Ent_1);
% Calcul de l'entropie generalisee pour n'importe quel vecteur
[P,H_v] = CodageEntropique(I_Ent_1(:));
% Verification de la bonne valeur de l'entropie
if (abs(H_v - H_ref) < 1e-10 && abs(P - 26.9471949267) < 1e-10)
    imagesc(I_Ent_1)
    colormap gray
    axis image off
    title({'Le calcul d''entropie est bon :' ...
           ['Entropie = ' num2str(H_v,'%.3g') ' bits/pixel (attendue : ' num2str(H_ref,'%.3g') ' bits/pixel)'] ...
           ['Poids de l''image = ' num2str(P,'%.3g') ' ko (attendu : ' num2str(26.9) ' ko)']})
else
    imagesc(I_Ent_2)
    colormap gray
    axis image off
    title({'Le codage entropique n''est pas bon :' ...
           ['Entropie = ' num2str(H_v,'%.3g') ' bits/pixel (attendue : ' num2str(H_ref,'%.3g') ' bits/pixel)'] ...
           ['Poids de l''image = ' num2str(P,'%.3g') ' ko (attendu : ' num2str(26.9) ' ko)']})
end
set(gca,'FontSize',15)
