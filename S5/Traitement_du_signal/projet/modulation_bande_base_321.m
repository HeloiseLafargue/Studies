clear
close all
%%_________________________________________________________________________
%% 3.2 Implantation
%%_________________________________________________________________________

%% 3.2.1 Modulation bande base

    % Signaux m1 et m2
load donnees1.mat
load donnees2.mat
m1 = 2*bits_utilisateur1 -1;
m2 = 2*bits_utilisateur2 -1;
Ns = 10;                      % Ns = (40*10^(-3) *120*10^3)/480
m1 = repmat(m1, Ns, 1);
m2 = repmat(m2, Ns, 1);
Te = 1 / (120*(10^3));        % Fe = 120*(10^3)
t = 0 : Te : Te*Ns*480 - Te;  % 9 = 10 - 1
m1 = m1(:)';
m2 = m2(:)';

figure(1);
subplot(2,1,1);
plot(t,m1);
xlabel("Temps (s)");
ylabel("m1(t)");
title("Signal m1(t) avec une échelle temporelle en secondes");
subplot(2,1,2);
plot(t,m2);
xlabel("Temps (s)");
ylabel("m2(t)");
title("Signal m2(t) avec une échelle temporelle en secondes");

     % Densité spectrale
Fe = 1/ Te;  
DSP1 = (1/Ns)*abs(fft(m1)).^2;
DSP2 = (1/Ns)*abs(fft(m2)).^2;
freq1 = -Fe : 2*Fe/(length(fft(m1))-1) :  Fe;
freq2 = -Fe : 2*Fe/(length(fft(m2))-1) :  Fe;
  
figure(2);
subplot(1,2,1);
plot(freq1, fftshift(DSP1));
xlabel("Fréquence (Hz)");
ylabel("DSP de m1 ");
title("Graphe de la DSP de m1"),
subplot(1,2,2);
plot(freq2, fftshift(DSP2));
xlabel('Fréquence (Hz)');
ylabel('DSP de m2');
title('Graphe de la DSP de m2')

%% 3.2.2 Construction du signal MF-TDMA
  % 3.2.2.1)a
    % générer un signal nul sur 40 ms
s_vide = zeros(1,length(m1));  

    % générer un signal comportant 5 slots de durée T = 40 ms contenant m1(t)
s1 = [s_vide ,m1, s_vide ,s_vide ,s_vide];
T1 = 0 : Te : (length(s1)-1)*Te;
  
figure(3);
subplot(3,1,1);
plot(T1,s1);
xlabel("Temps (s)");
ylabel("Signal 1");
title("1er signal sur le 2ème slot");

    % générer un signal comportant 5 slots de durée T = 40 ms contenant m2(t)
s2 = [s_vide,s_vide,s_vide,s_vide,m2];
T2 = 0 : Te : (length(s2)-1)*Te;
  
subplot(3,1,2);
plot(T2,s2);
xlabel("Temps (s)");
ylabel("Signal 2");
title("2e signal sur le 5ème slot");

    % le signal comportant 5 slots de durée T = 40 ms contenant m1(t) et m2(t)
s3 = [s_vide,m1,s_vide,s_vide,m2];
T3 = 0 : Te : (length(s3)-1)*Te;

subplot(3,1,3);
plot(T3,s3);
xlabel("Temps (s)");
ylabel("Signal résultant");
title("Signal comportant 5 slots et portant les messages m1 et m2");

   %  3.2.2.1)b
    % placer pour chaque utilisateur, les messages m1 et m2 sur les
    % fréquences porteuses allouées
x1 = s1;                                % dans ce cas fp1  = 0 Khz
fp2 = (46*10^3);
temps = 0: Te: (length(s2)-1)*Te;
x2 = s2.*cos(2*pi*fp2*temps);
message = 0 : Te : (length(x2)-1)*Te;

figure(4);
subplot(2,1,1);
plot(message,x1);
xlabel("Temps (s)");
ylabel("x1");
title("Le 1er message porté par la fréquence fp1");
subplot(2,1,2);
plot(message,x2);
xlabel("Temps (s)");
ylabel("x2");
title("Le 2ème message porté par la fréquence fp2");

% 3.2.2.2)
    % Sommer les signaux x1(t) et x2(t) et ajouter le bruit gaussien pour obtenir le signal MF-TDMA
Psignal = mean(abs(x1+x2).^2);
Pbruit = Psignal*(10^-10);
bruit = sqrt(Pbruit)*randn(1,5*length(m1));
trame_MFTDMA = bruit+x1+x2;                 % la trame MF-TDMA à laquelle on a ajouté un bruit gaussien
T3 = 0: Te: (length(trame_MFTDMA)-1)*Te;

figure(5);
plot(T3,trame_MFTDMA);
title("Graphe du signal bruité MF-TDMA");
xlabel("Temps (s)");
ylabel("Signal x");

% 3.2.2.3)
    % la densité spectrale de puissance du signal MF-TDMA
fft_MFTDMA = fft(trame_MFTDMA);
DSP_MFTDMA = (1/length(fft_MFTDMA))*(abs(fft_MFTDMA)).^2;    % DSP du signal MF-TDMA

figure(6);
plot(linspace(-Fe/2,Fe/2,length(DSP_MFTDMA)),fftshift(DSP_MFTDMA));
xlabel("Fréquence (Hz)");
ylabel("DSP");
title("DSP du signal MF-TDMA");

%%_________________________________________________________________________
%% 4.1 Démultiplexage des porteuses
%%_________________________________________________________________________

%% 4.1.1 Synthèse du filtre passe-bas
 
% 4.1.2.3)
% la réponse impulsionnelle et la réponse en fréquence du filtre passe-bas de
% type RIF permettant de récupérer le signal x1(t) provenant de l'utilisateur 1

% réponse impulsionnelle
N = 61;             % ordre du filtre
k = -(N-1)/2:(N-1)/2;
fcoupure = fp2/2;
pb = 2*fcoupure*Te*sinc(2*k*fcoupure*Te);
T4 = linspace(-1/fp2,1/fp2,length(pb));

figure(7);
subplot(2,1,1);
plot(T4,pb);
xlabel("Temps (s)");
ylabel("y(t)");
title("Réponse impulsionnelle du filtre passe-bas");

% réponse fréquentielle
Hpb = fftshift(fft(pb));
F4 = linspace(-fp2,fp2,length(Hpb));

subplot(2,1,2);
plot(F4,abs(Hpb));
xlabel("Fréquence (Hz)");
ylabel("H pb");
title("Réponse fréquencielle du filtre passe-bas");

% 4.1.2.4)
 % la densité spectrale de puissance du signal MF-TDMA reçu et le module 
 % de la réponse en fréquences du filtre implanté
F5 = linspace(-fp2,fp2,length(fft(pb,length(DSP_MFTDMA))));
filtre_passebas_normalise = (1/max(fft(pb,length(DSP_MFTDMA))))*abs(fft(pb,length(DSP_MFTDMA)));
DSP_MFTDMA_normalise = (1/max(DSP_MFTDMA))*abs(DSP_MFTDMA);

figure(8);
plot(F5,fftshift(abs(filtre_passebas_normalise)));
hold on;
plot(F5,fftshift(DSP_MFTDMA_normalise));
hold off;
xlabel("Fréquence (Hz)");
ylabel("Réponse en fréquence");
title("Superposition de la réponse en fréquence du filtre Pb et DSP du signal MF-TDMA reçu");
legend("Filtre Pb","DSP du signal MF-TDMA"); 

%% 4.1.2 Synthèse du filtre passe-Haut

% 4.1.2.3)
% déduction de la synthèse du filtre passe-haut de celle du filtre passe-bas.
% Réponse impulsionnelle du filtre passe-haut
ph = -pb;
ph((N-1)/2+1) = 1-pb((N-1)/2+1);

figure(9);
subplot(2,1,1);
plot(T4,ph);
xlabel("Temps (s)");
ylabel("y1(t)");
title("Réponse impulsionnelle du filtre passe-haut");

% Réponse fréquentielle du filtre passe-haut
Hph = fft(ph);
F6 = linspace(-fp2,fp2,length(ph));

subplot(2,1,2);
plot(F6,fftshift(abs(Hph)));
xlabel("Fréquence (Hz)");
ylabel("Y1(f)");
title("Réponse en fréquence du filtre passe-haut");

% 4.1.2.4)
% Densité spectrale de puissance du signal MF-TDMA reçu et module de 
% la réponse en fréquences du filtre implanté
ffiltre_passehaut = fft(ph,length(DSP_MFTDMA_normalise));    
f_passehaut_normalise = (1/max(ffiltre_passehaut))*abs(ffiltre_passehaut);
F7= linspace(-fp2/2,fp2/2,length(f_passehaut_normalise));

figure(10);
plot(F7,abs(fftshift(f_passehaut_normalise)));
hold on;
plot(F7,abs(fftshift(DSP_MFTDMA_normalise)));
hold off;
xlabel("Fréquence (Hz)");
ylabel("Réponse en fréquence");
title("Superposition de la réponse féquentiellelle du filtre passe-haut et DSP du signal");
legend("Filtre Ph","DSP du signal MF-TDMA");

%% 4.1.3 Filtrage

    % Tracés de x_tilde_1 et x_tilde_2   

x_tilde_1 = conv(trame_MFTDMA,pb,'same');
x_tilde_2 = conv(trame_MFTDMA,ph,'same');
T3 = linspace(0,200*(10^-3),length(x_tilde_1));

figure(11); 
subplot(2,1,1);
plot(T3,x_tilde_1);
xlabel("Temps (s)");
ylabel("x_tilde_1");
title("Filtrage du message 1");
subplot(2,1,2);
plot(T3,x_tilde_2);
xlabel("Temps (s)");
ylabel("x_tilde_2");
title("Filtrage du message 2");

%%_________________________________________________________________________
%% 4.2 Retour en bande de base
%%_________________________________________________________________________

    % retard
retard = 30;
    % Retour en bande de base
fp1 = 0;    
Tf = 0:Te:(length(x_tilde_2)-1)*Te;
x_tilde_1_bande = x_tilde_1.*cos(2*pi*fp1*Tf);
x_tilde_2_bande = x_tilde_2.*cos(2*pi*fp2*Tf);
x_tilde_1_bande_1 = [x_tilde_1_bande, zeros(1, retard)];
x_tilde_2_bande_2 = [x_tilde_2_bande, zeros(1, retard)];
x1_final = filter(pb, 1, x_tilde_1_bande_1);
x2_final = filter(pb, 1, x_tilde_2_bande_2);
x1_final = x1_final(retard + 1: end);
x2_final = x2_final(retard + 1 : end);

%%_________________________________________________________________________
%% 4.3 Détection du slot utile
%%_________________________________________________________________________

tranche_1 =reshape(x1_final, length(m1), 5);
tranche_2 =reshape(x2_final, length(m1), 5);
energie_1 = mean(abs(tranche_1).^2)*length(tranche_1);
energie_2 = mean(abs(tranche_2).^2)*length(tranche_2);
[~, slot_1] = max(energie_1);
[~, slot_2] = max(energie_2);
slot1 = tranche_1(:, slot_1);
slot2 = tranche_2(:, slot_2);
% subplot(2,1,1);
% plot(t, slot1);
% xlabel("Temps (s))");
% ylabel("slot1");
% title("le slot1 en fonction du temps");
% subplot(2,1,2);
% plot(t, slot1);
% xlabel("Temps (s)");
% ylabel("slot2");
% title("le slot2 en fonction du temps");

%%_________________________________________________________________________
%% 4.4 Démodulation bande de base
%%_________________________________________________________________________
SignalFiltre_1 = conv(slot1, ones(1,Ns), 'same');
SignalEchantillonne_1 = SignalFiltre_1(1:Ns:end);
BitsRecuperes_1 = (sign(SignalEchantillonne_1)+1)/2;

SignalFiltre_2 = conv(slot2, ones(1,Ns), 'same');
SignalEchantillonne_2 = SignalFiltre_2(1:Ns:end);
BitsRecuperes_2 = (sign(SignalEchantillonne_2)+1)/2;

texte_1=bin2str(BitsRecuperes_1);
texte_2=bin2str(BitsRecuperes_2);
fprintf('Le message 1 est :\n\t %s\n\n', texte_1);
fprintf('Le message 2 est :\n\t %s\n', texte_2);
