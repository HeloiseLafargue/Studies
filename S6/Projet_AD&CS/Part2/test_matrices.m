clear all;
close all;
format long;

% Figures des spectres des 4 types de matrices

figure;

load A_200_1.mat
subplot(2, 2, 1);
D = sort(D, 'descend');
plot(D);
ylabel("Valeurs propres");
title("Classification des vp matrice type 1");

load A_200_2.mat
subplot(2, 2, 2);
D = sort(D, 'descend');
plot(D);
ylabel("Valeurs propres");
title("Classification des vp matrice type 2");

load A_200_3.mat
subplot(2, 2, 3);
D = sort(D, 'descend');
plot(D);
ylabel("Valeurs propres");
title("Classification des vp matrice type 3");

load A_200_4.mat
subplot(2, 2, 4);
D = sort(D, 'descend');
plot(D);
ylabel("Valeurs propres");
title("Classification des vp matrice type 4");
