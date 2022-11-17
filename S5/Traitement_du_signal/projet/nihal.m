close all;
clear;

%%___________________________________________________________________
% Implantation_________________________________________________________________________
%%
% 3.2.1 Modulation bande base
    % Signaux m1 et m2
  load donnees1.mat
  load donnees2.mat
  m1 = 2*bits_utilisateur1 - 1;
  m2 = 2*bits_utilisateur2 - 1;
  Ns = 10;       % Ns = (40*10^(-3) *120*10^3)/480
  m1 = repmat(m1, Ns, 1);
  m2 = repmat(m2, Ns, 1);
  Te = 1 / (120*(10^3));       % Fe = 120*(10^3)
  t = 0 : Te : Te*Ns*480 - Te;     % 9 = 10 - 1
  m1 = m1(:)';
  m2 = m2(:)';
  figure;
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
  t1 = 0:Te:(length(fft(m1))-1)*Te;
  DSP1=(1/Ns)*abs(fft(m1)).^2;
  DSP2=(1/Ns)*abs(fft(m2)).^2;
  freq1 = -Fe : 2*Fe/(length(fft(m1))-1) :  Fe;
  freq2 = -Fe : 2*Fe/(length(fft(m2))-1) :  Fe;
  figure;
  subplot(2,1,1);
  plot(freq1, fftshift(DSP1));
  xlabel("frequences en Hz");
  ylabel("Densité spectrale de puissance de m1 ");
  title("Graphe de la DSP de m1");
  subplot(2,1,2);
  plot(freq2, fftshift(DSP2));
  xlabel('frequences en Hz');
  ylabel('Densité spectrale de puissance de m2');
  title('Graphe de la DSP de m2')
% 3.2.2 Construction du signal MF-TDMA
  % 3.2.2.1)a
    % générer un signal nul sur 40 ms
  s_vide = zeros(1,length(m1));  
    % le signal comportant 5 slots de durée T = 40 ms contenant m1(t)
  s1 = [s_vide ,m1, s_vide ,s_vide ,s_vide];
  T1 = 0 : Te : (length(s1)-1)*Te;
  figure;
  subplot(3,1,1);
  plot(T1,s1);
  xlabel("temps en secondes");
  ylabel("le 1er signal");
  title("le 1er signal sur le 2 ème slot");
    % le signal comportant 5 slots de durée T = 40 ms contenant m2(t)
  s2 = [s_vide,s_vide,s_vide,s_vide,m2];
  T2 = 0 : Te : (length(s2)-1)*Te;
  subplot(3,1,2);
  plot(T2,s2);
  xlabel("temps en secondes");
  ylabel("le 2eme signal");
  title("le 2eme signal sur le 5 ème slot");
    % le signal comportant 5 slots de durée T = 40 ms contenant m1(t) et
    % m2(t)
  s3 = [s_vide,m1,s_vide,s_vide,m2];
  T2 = 0 : Te : (length(s3)-1)*Te;
  subplot(3,1,3);
  plot(T2,s3);
  xlabel("temps en secondes");
  ylabel("le signal résultant");
  title("le signal comportant 5 slots et portant les messages m1 et m2");
%  3.2.2.1)b
    % plaçons pour chaque utilisateur, le message
% précédemment construit sur la fréquence porteuse alloué
x1 = s1;    % dans ce cas fp1  = 0 Khz
message = 0 : Te : (length(x1)-1)*Te;
fp2 = 46*10^3;
temps = 0: Te: (length(s2)-1)*Te;
x2 = s2.*cos(2*pi*fp2*temps);
message = 0 : Te : (length(x2)-1)*Te;
figure;
subplot(2,1,1);
plot(message,x1);
xlabel("temps en secondes");
ylabel("x1");
title("Le 1er message porté par la fréquence fp1");
subplot(2,1,2);
plot(message,x2);
xlabel("temps en secondes");
ylabel("x2");
title("Le 2ème message porté par la fréquence fp2");
% 3.2.2.2)
    % Sommer les signaux x1(t) et x2(t) et ajouter le bruit gaussien afin pour obtenir le signal MF-
% TDMA
Psignal = mean(abs(x1+x2).^2);
Pbruit = Psignal*(10^-10);
bruit = sqrt(Pbruit)*randn(1,5*length(m1));
trame_MFTDMA = bruit+x1+x2;    % le trame MF-TDMA auquel on a ajouter un bruit gaussien
T3 = 0: Te: (length(trame_MFTDMA)-1)*Te;
figure;
plot(T3,trame_MFTDMA);
title("Signal x en y ajoutant le bruit gaussien");
xlabel("temps en secondes");
ylabel("le signal x");
 
 
%3.2.2.3)
    % la densité spectrale de puissance du signal MF-TDMA
fft_MFTDMA = fft( trame_MFTDMA);
DSP_MFTDMA = (1/length(fft_MFTDMA))*(abs(fft_MFTDMA)).^2;    %la densité spectrale de puissance du signal MF-TDMA
figure;
plot(linspace(-Fe/2,Fe/2,length(DSP_MFTDMA)),fftshift(DSP_MFTDMA));
title("DSP du signal MF-TDMA");
xlabel("frequence en Hz");
ylabel("DSP");
% 4.1.1.1)
    % la réponse impulsionnelle et la réponse en fréquence du filtre passe-bas de type RIF permettant de récupérer le signal x1(t) provenant de
    % l'utilisateur 1
% définir un filtre passe bas
% réponse impulsionnelle
figure;
N = 61;             % ordre du filtre
k = -(N-1)/2:(N-1)/2;
fcoupure = fp2/2;
rep_impu =2*fcoupure*Te*sinc(2*fcoupure*k*Te);
T4 = linspace(-1/fp2,1/fp2,length(rep_impu));
subplot(2,1,1);
plot(T4,rep_impu);
xlabel("temps en s");
ylabel("y(t)");
title("réponse impulsionnelle du filtre passe bas");
% réponse fréquentielle
F4 = linspace(-fp2,fp2,length(fft(rep_impu)));
subplot(2,1,2);
plot(F4,fftshift(abs(fft(rep_impu))));
xlabel("frequence en Hz");
ylabel("Y(f)");
title("réponse fréquencielle du filtre passe bas");
% 4.1.1.2)
 % la densité spectrale de puissance du signal MF-TDMA reçu et le module 
 % de la réponse en fréquences du filtre implanté
F5 = linspace(-fp2,fp2,length(fft(rep_impu,length(DSP_MFTDMA))));
filtre_passebas_normalise = (1/max(fft(rep_impu,length(DSP_MFTDMA))))*abs(fft(rep_impu,length(DSP_MFTDMA)));
plot(F5,fftshift(abs(filtre_passebas_normalise)));
hold on;
figure;
xlabel("frequence en Hz");
ylabel("Réponse en fréquence");
title("superposition du réponse impulsionnelle du filtre Pb et DSP du signal");
% legend("filtre Pb et DSP du signal MF-TDMA");
filtre_passebas_normalise = (1/max(DSP_MFTDMA))*abs(DSP_MFTDMA);
plot(F5,fftshift(filtre_passebas_normalise));
hold off;
% 4.1.2.3)
     % déduction de la synthèse du filtre passe-haut de celle du filtre passe-bas.
% Réponse impulsionnelle du filtre passe-Haut
figure;
rep_impu1 = -rep_impu;
rep_impu1((N-1)/2+1) = 1-rep_impu((N-1)/2+1);
subplot(2,1,1);
plot(linspace((-N+1)/2, (N-1)/2, N), rep_impu1);
xlabel("temps en s");
ylabel("y1(t)");
title("réponse impulsionnelle du filtre passe haut");
% réponse fréquentielle
Y_ph = fft(rep_impu1);
F6 = linspace(-fp2,fp2,length(rep_impu1));
subplot(2,1,2);
plot(F6,abs(fftshift(Y_ph)));
xlabel("frequence en Hz");
ylabel("Y1(f)");
title("réponse en fréquence du filtre passe haut");
% 4.1.2.4)
    %la densité spectrale de puissance du signal MF-TDMA reçu et le module de 
% la réponse en fréquences du filtre implanté.
figure;
ffiltre_passehaut = fft(rep_impu1,length(filtre_passebas_normalise));    
f_passehaut_normalise = (1/max(ffiltre_passehaut))*abs(ffiltre_passehaut);
F7= linspace(-fp2/2,fp2/2,length(f_passehaut_normalise));
plot(F7,fftshift(abs(f_passehaut_normalise)));
xlabel("frequence en Hz");
title("superposition du réponse féquentiellelle du filtre  et DSP du signal");
hold on;
plot(F7,abs(fftshift(filtre_passebas_normalise)));
hold off;
legend("la densité spectrale de puissance du signal MF-TDMA et le module de la réponse en fréquences du filtre implanté");
% 4.1.3)
    % Filtrage
    % Traçage de X_tilde_1 et X_tilde_2
figure;    
% x_tilde_1 = filter(rep_impu, 1, trame_MFTDMA);
x_tilde_1 = conv(trame_MFTDMA,rep_impu,'same');
T3 = linspace(0,200*(10^-3),length(x_tilde_1));
subplot(2,1,1);
plot(T3,x_tilde_1);
xlabel("temps en s");
ylabel("x_tilde_1");
title("filtrage du message 1");
% x_tilde_2 = filter(rep_impu1, 1, trame_MFTDMA);
x_tilde_2 = conv(trame_MFTDMA,rep_impu1,'same');
subplot(2,1,2);
plot(T3,x_tilde_2);
xlabel("temps en s");
ylabel("x_tilde_2");
title("filtrage du message 2");
% 4.2)
    % retard
retard = 30;
    % Retour en bande de base
fp1 = 0;    
Tf = 0:Te:(length(x_tilde_2)-1)*Te;
x_tilde_1_bande = x_tilde_1.*cos(2*pi*fp1*Tf);
x_tilde_2_bande = x_tilde_2.*cos(2*pi*fp2*Tf);
x_tilde_1_bande_1 = [x_tilde_1_bande, zeros(1, retard)];
x_tilde_2_bande_2 = [x_tilde_2_bande, zeros(1, retard)];
x1_final = filter(rep_impu, 1, x_tilde_1_bande_1);
x2_final = filter(rep_impu, 1, x_tilde_2_bande_2);
x1_final = x1_final(retard + 1: end);
x2_final = x2_final(retard + 1 : end);
% 4.3)
    % Détection du slot utile
tranche_1 =reshape(x1_final, length(m1), 5);
tranche_2 =reshape(x2_final, length(m1), 5);
energie_1 = mean(abs(tranche_1).^2)*length(tranche_1);
energie_2 = mean(abs(tranche_2).^2)*length(tranche_2);
[~, slot_1] = max(energie_1);
[~, slot_2] = max(energie_2);
slot1 = tranche_1(:, slot_1);
slot2 = tranche_2(:, slot_2);
subplot(2,1,1);
plot(t, slot1);
xlabel("temps en s");
ylabel("slot1");
title("le slot1 en fonction du temps");
subplot(2,1,2);
plot(t, slot1);
xlabel("temps en s");
ylabel("slot2");
title("le slot2 en fonction du temps");
% 4.4)
    % Démodulation bande de base
SignalFiltre=conv(slot1,ones(1,Ns),'same');
SignalEchantillonne=SignalFiltre(1:Ns:end);
BitsRecuperes=(sign(SignalEchantillonne)+1)/2;
bin2str(BitsRecuperes)
SignalFiltre=conv(slot2,ones(1,Ns),'same');
SignalEchantillonne=SignalFiltre(1:Ns:end);
BitsRecuperes=(sign(SignalEchantillonne)+1)/2;
bin2str(BitsRecuperes)
 