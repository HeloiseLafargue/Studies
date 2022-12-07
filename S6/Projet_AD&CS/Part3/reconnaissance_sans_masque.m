clear;
close all;

load eigenfaces_part3;

% Tirage aléatoire d'une image de test :
personne = randi(nb_personnes);
posture = randi(nb_postures);
% si on veut tester/mettre au point, on fixe l'individu
%personne = 10
%posture = 6

ficF = strcat('./Data/', liste_personnes{personne}, liste_postures{posture}, '-300x400.gif');
img = imread(ficF);
image_test = double(transpose(img(:)));

% Nombre q de composantes principales à prendre en compte 
q = 2;

% dans un second temps, q peut être calculé afin d'atteindre le pourcentage
% d'information avec q valeurs propres (contraste)

% Pourcentage d'information 
per = 0.75;

% % calcul du contraste
% c = [];
% som = 0;
% total = sum(lambda1);
% for i = 1:16
%     som = som + lambda1(i);
%     c = [c; som/total];
% end
% 
% contr = 0;
% ind = 0;
% while (contr <= per)
%     ind = ind + 1;
%     contr = c(ind);
% end
% q = ind;


% DataA      : les données d'apprentissage (connues)
% LabelA     : les labels des données d'apprentissage
%
% DataT      : les données de test (on veut trouver leur label)
% Nt_test    : nombre de données tests qu'on veut labelliser
%
% K          : le K de l'algorithme des k-plus-proches-voisins
% ListeClass : les classes possibles (== les labels possibles)

% Calcul de DataA
DataA = [];
for j = 1:nb_personnes_base,
	for k = liste_postures_base,  
        ficF2 = strcat('./Data/', liste_personnes_base{j}, liste_postures{k}, '-300x400.gif');
        img2 = imread(ficF2);
        DataA = [DataA ; double(transpose(img2(:)))];
	end
end

% Calcul de labelA
labelA_pers = 1:(nb_personnes_base*nb_postures_base);
labelA_post = 1:nb_postures_base;

% Calcul de DataT
W = W(:,1:q); % prise en compte q composantes principales
DataT = (image_test-X_moyen)*W;

% Choix de Nt_test
Nt_test = 1; % A changer, pouvant aller de 1 jusqu'à Nt

% Choix du nombre de voisins
K = 1;

% Calcul de ListeClass
ListeClass_pers = 1:(nb_personnes_base*nb_postures_base);
ListeClass_post = 1:nb_postures_base;

C_base = DataA*W;

% Classement par l'algorithme des k-ppv
dpers = C_base(1:nb_postures_base:end,:);
[Partition] = kppv(dpers, labelA_pers, DataT, Nt_test, K, ListeClass_pers);
personne_c = Partition(1);

dpost = C_base(nb_postures_base*(personne_c-1)+1:nb_postures_base*personne_c,:);
[Partition] = kppv(dpost, labelA_post, DataT, 1, K, ListeClass_post);
posture_c = Partition(1);


% individu pseudo-résultat pour l'affichage (A CHANGER)
personne_proche = personne_c;
posture_proche = posture_c;

figure('Name','Image tiree aleatoirement','Position',[0.2*L,0.2*H,0.8*L,0.5*H]);

subplot(1, 2, 1);
% Affichage de l'image de test :
colormap gray;
imagesc(img);
title({['Individu de test : posture ' num2str(posture) ' de ', liste_personnes{personne}]}, 'FontSize', 20);
axis image;

ficF = strcat('./Data/', liste_personnes_base{personne_proche}, liste_postures{posture_proche}, '-300x400.gif');
img_predite = imread(ficF);
        
subplot(1, 2, 2);
imagesc(img_predite);
title({['Individu la plus proche : posture ' num2str(posture_proche) ' de ', liste_personnes_base{personne_proche}]}, 'FontSize', 20);
axis image;


% Matrice de confusion

image_predite = double(transpose(img_predite(:)));
Matrice_confusion = confusionmat(image_test,image_predite)