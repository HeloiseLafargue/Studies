% ------------------------------------------------------------------------
% Télécommunications : Etudes de chaines de transmission en bande de base                
% ------------------------------------------------------------------------

clear;
close all;

Fe = 24000; Te = 1 / Fe;
Rb = 3000; Tb = 1 / Rb;
nb_bits = 5000; bits = randi([0,1], 1, nb_bits);

%% __________ Etude de l'impact du bruit, filtrage adapté, ________________
% __________ taux d'erreur binaire, efficacité en puissance _______________


% _______________________ CHAINE DE REFERENCE _____________________________

% _______________ Implantation de la chaîne sans bruit ____________________
%------------------- Séquence 3, 5)2 , initialisation ---------------------

%---------------------------- Modulation ----------------------------------
M = 2; % mapping
n = log2(M);
Ns = n*Tb / Te; Ts = Ns * Te;

Symboles = 2*bits - 1;
Suite_diracs = kron(Symboles, [1 zeros(1, Ns - 1)]);
h = ones(1,Ns);
s = filter(h,1,Suite_diracs);

%-------------------------- Démodulation ----------------------------------
hr = ones(1,Ns);
s_demod = filter(hr,1,s);

%--------------------- Echantillonage avec n0 = Ns-------------------------
sech = s_demod(Ns:Ns:length(s_demod));
s_demap = sech>=0;
TEB = sum(abs(bits - s_demap));
taux_erreur = 100*TEB/nb_bits;
fprintf("5.2) Le TEB obtenu pour la chaine sans bruit est %f \n",TEB);
% on trouve effectivement TEB = 0 pour un signal non bruité;

% _______________ Implantation de la chaîne bruitée _______________________
%------------------------- Séquence 3, 5)2, qst 1 -------------------------
%---------------------------- Modulation ----------------------------------

M = 2; n = log2(M);
Ns = n*Tb / Te; Ts = Ns * Te;
plage_temp = [0 : Te : (nb_bits*Ns-1)*Te];

Symboles = 2*bits - 1;
Suite_diracs = kron(Symboles, [1 zeros(1, Ns - 1)]);
h = ones(1,Ns);
s = filter(h,1,Suite_diracs);

figure(1); sgtitle('Implantation de la chaîne bruitée');subplot(2,1,1);
plot(s); axis([0 Ns*10 -1.5 1.5]); xlabel("Temps (s)"); ylabel("signal");
title("Signal modulé");

%------------------------------ Bruit -------------------------------------
Px = mean(abs(s).^2);
R_signal_bruit_DB = 2;
R_signal_bruit = 10^(R_signal_bruit_DB/10);
var = sqrt( (Px*Ns) / (2*n*R_signal_bruit) );
bruit = var * randn(1, length(s) );

%figure(2);
%plot(bruit); axis([0 nb_bits -2 2]); ylabel('Bruit'); xlabel('Temps (s)');
%title('Bruit en fonction du temps');

%-------------------------- Démodulation ----------------------------------
hr = ones(1,Ns);
s_demod = filter(hr,1,s + bruit);

figure(1); subplot(2,1,2);
plot(s_demod); axis([0 Ns*10 -20 20]);ylabel('Signal');xlabel('Temps (s)');
title('Signal démodulé');

%----------------------- Chaine de transmission ---------------------------
g = conv(h,h);
plage = [0:Te:2*(Ns-1)*Te];
%figure(3);
%plot(g); axis([0 2*Ns 0 10]); ylabel('Réponse impulsionnelle');
%xlabel('Temps (s)');
%title('Réponse impulsionnelle globale de la chaine de transmission');

%----------------------- Diagramme de l'oeil ------------------------------
figure(4); 
plot(reshape(s_demod,Ns,[])); xlabel('Temps (s)');
title("Diagramme de l'oeil pour la chaîne bruitée");

%-------------------- Echantillonage avec n0 = Ns -------------------------
sech = s_demod(Ns:Ns:length(s_demod));
s_demap = sech>=0;
TEB = sum(abs(bits - s_demap));
taux_erreur = 100*TEB/nb_bits;

%------------------- Séquence 3, 5/2 , qst 2 et 3 -------------------------
%taux d'erreur binaire obtenu en fonction du rapport signal à bruit par bit
% à l'entrée du recepteur (Eb/N0) en dB pour des valeurs allant de 0 à 8 dB
for i = 0:8
    R_signal_bruit = 10^(i/10); 
    var = sqrt( (Px*Ns) / (2*n*R_signal_bruit) );
    bruit = var * randn(1, length(s) );
    s_demod = filter(h,1,(s + bruit));
    s_ech = s_demod(Ns:Ns:length(s_demod));
    s_demap = s_ech>=0;
    TEB(i+1) = sum(abs(bits - s_demap));
    TEBsim1(i+1) = TEB(i+1)/nb_bits;
    TEBth1(i+1) = qfunc(sqrt(2*R_signal_bruit));
end

figure(5); 
semilogy(TEBsim1); hold on; semilogy(TEBth1); hold off;
ylabel('TEB'); xlabel('Eb/N0 (dB)'); legend('TEB simulé', 'TEB théorique');
title('Comparaison entre le TEB théorique et le TEB simulé');



% _____________________ 1ere CHAINE A ETUDIER _____________________________

% _______________ Implantation de la chaîne sans bruit ____________________
%-------------------- Séquence 3, 5.3.1, qst 1 ----------------------------
%----------------------- Diagramme de l'oeil ------------------------------
hr = ones(1,Ns/2);
s_demod = filter(hr,1,s);

figure(6);
plot(reshape(s_demod,Ns,[])); xlabel('Temps (s)');
title("Diagramme de l'oeil pour la chaîne sans bruit");

%--------------------- Séquence 3, 5.3.1, qst 2 ---------------------------
%--------- Echantillonage avec n0 = [Ns/2, Ns] et calcul de TEB -----------
for i = 4 : 8 
    sech = s_demod(i:Ns:length(s_demod));
    s_demap = sech>=0;
    TEB = sum(abs(bits - s_demap));
    taux_erreur(i+1) = 100*TEB/nb_bits;
end
fprintf("5.3.1) Le TEB obtenu pour la chaine sans bruit est %f \n",TEB);
% on trouve effectivement TEB = 0 pour un signal non bruité;

% _______________ Implantation de la chaîne bruitée _______________________
%-------------------- Séquence 3, 5.3.2, qst 1 ----------------------------
%------------------------ Rajout du bruit ---------------------------------
Px = mean(abs(s).^2);
R_signal_bruit_DB = 2;
R_signal_bruit = 10^(R_signal_bruit_DB/10); 
var = sqrt( (Px*Ns) / (2*n*R_signal_bruit) );
bruit = var * randn(1, length(s) );
s_demod = filter(hr,1,s + bruit);

figure(7);
plot(reshape(s_demod,Ns,[])); xlabel('Temps (s)');
title("Diagramme de l'oeil pour la chaîne avec bruit");

%------------------- Séquence 3, 5.3.2, qst 2 et 3 ------------------------
% taux d'erreur binaire obtenu en fonction du rapport signal à bruit par
% bit à l'entrée du récepteur (Eb/N0) en décibels
for i = 0:8
    R_signal_bruit = 10^(i/10); 
    var = sqrt( (Px*Ns) / (2*n*R_signal_bruit) );
    bruit = var * randn(1, length(s) );
    s_demod = filter(hr,1,s + bruit);
    s_ech = s_demod(Ns:Ns:length(s_demod));
    s_demap = s_ech>=0;
    TEB(i+1) = sum(abs(bits - s_demap));
    TEBsim2(i+1) = TEB(i+1)/nb_bits;
    TEBth2(i+1) = qfunc(sqrt(R_signal_bruit));
end
figure(8);
semilogy(TEBsim2); hold on; semilogy(TEBth2); hold off; 
ylabel('TEB'); xlabel('décibel'); legend('TEB simulé', 'TEB théorique');
title('Comparaison entre TEB théorique et TEB simulé (chaîne bruitée)');

%---------------------- Séquence 3, 5.3.2, qst 4 --------------------------
%-------- Comparaison des TEB avec filtres de reception différents --------
figure(9);
semilogy(TEBsim2); hold on; semilogy(TEBsim1); hold off;
ylabel('TEB');xlabel('db');
legend('TEB simulé pour la chaîne de transmission ','TEB de la chaine de référence');
title('Comparaison entre TEB de la chaine de référence et TEB simulé');



% _______________________ 2e CHAINE A ETUDIER _____________________________

nb_bits = 10000;
bits = randi([0,1], 1, nb_bits);

% _______________ Implantation de la sans chaîne bruit ____________________
%----------------------- Séquence 3, 5.5, qst 1 ---------------------------
%---------------------------- Modulation ----------------------------------
M = 4; % mapping
n = log2(M);
Ns = n*Tb / Te; Ts= Ns * Te;
plage_temp = [0 : Te : (nb_bits*Ns-1)*Te];

Symboles = (2 * bi2de(reshape(bits, 2, length(bits)/2).') - 3).';
Suite_diracs = kron(Symboles, [1 zeros(1, Ns - 1)]);
h = ones(1,Ns);
s = filter(h,1,Suite_diracs);
figure(10); sgtitle('Implantation de la chaîne sans bruit'); subplot(2,1,1);
plot(s); axis([0 Ns*100 -1.5 1.5]); ylabel('Signal'); xlabel('Temps (s)');
title('Signal modulé');

%-------------------------- Démodulation ----------------------------------
s_demod = filter(h,1,s);
figure(10); subplot(2,1,2);
plot(s_demod); axis([0 Ns*10 -20 20]); ylabel('Signal'); xlabel('Temps (s)');
title('Signal démodulée');

%---------------------- Chaine de transmission- ---------------------------
g = conv(h,h);
plage = [0:Te:2*(Ns-1)*Te];

%figure(12);
%plot(g); axis([0 2*Ns 0 10]); ylabel('Signal'); xlabel('Temps (s)');
%title("Réponse impulsionelle globale de la chaîne de transmission");

%----------------------- Diagramme de l'oeil ------------------------------
figure(13);
plot(reshape(s_demod,Ns,[])); xlabel('Temps (s)');
title("Diagramme de l'oeil de la chaîne sans bruit");

%----------------------- Séquence 3, 5.5, qst 2 ---------------------------
%------------- Echantillonage avec n0 = Ns et calcul du TEB ---------------
s_ech = s_demod(Ns:Ns:length(s_demod));
s_temp = s_ech;
s_temp(s_ech <= -2*Ns) = -3;
s_temp((s_ech > -2*Ns) & (s_ech <= 0)) = -1;
s_temp((s_ech > 0) & (s_ech <= 2*Ns)) = 1;
s_temp(s_ech >= 2*Ns) = 3;
s_demap = reshape(de2bi((s_temp + 3)/2).', 1, length(bits));
TEB = sum(abs(bits - s_demap));
taux_erreur = TEB/nb_bits;
fprintf("5.5.2) Le TEB obtenu pour la chaine sans bruit est %f \n",TEB);

% _______________ Implantation de la chaîne bruitée _______________________

%----------------------- Séquence 3, 5.6, qst 1 et 2 ----------------------

% ---------------------------- Bruit --------------------------------------
Px = mean(abs(s).^2);
R_signal_bruit_DB = 8;
R_signal_bruit = 10^(R_signal_bruit_DB/10);  
var = sqrt( (Px*Ns) / (2*n*R_signal_bruit));
bruit = var * randn(1, length(s) );

%figure(11);
%plot(bruit); axis([0 nb_bits -2 2]); 
%ylabel('Amplitude'); xlabel('Temps (s)'); title('Bruit');

%--------- Tracé de TES en fonction du rapport signal sur bruit -----------

for i = 0:8
    R_signal_bruit = 10^(i/10); 
    var = sqrt( (Px*Ns) / (2*n*R_signal_bruit) );
    bruit = var * randn(1, length(s) );
    s_demod = filter(h,1,(s + bruit));

    s_ech = s_demod(Ns:Ns:length(s_demod));
    s_temp = s_ech;
    s_temp(s_ech <= -2*Ns) = -3;
    s_temp((s_ech > -2*Ns) & (s_ech <= 0)) = -1;
    s_temp((s_ech > 0) & (s_ech <= 2*Ns)) = 1;
    s_temp(s_ech >= 2*Ns) = 3;
    s_demap = reshape(de2bi((s_temp + 3)/2).', 1, length(bits));
    
    TES(i+1) = sum(abs(Symboles - s_temp));
    TESsim(i+1) = TES(i+1)/nb_bits;
    TESth(i+1) = (3/2)*qfunc(sqrt((4/5)*R_signal_bruit));

    TEB(i+1) = sum(abs(bits - s_demap));
    TEBsim(i+1) = TEB(i+1)/nb_bits;
    TEBth(i+1) = (3/4)*qfunc(sqrt((4/5)*R_signal_bruit));
end

figure(14);
semilogy(TESsim); hold on; semilogy(TESth); hold off;
ylabel('TES'); xlabel('Eb/N0 (dB)');legend('TES simulé',' TES théorique')
title('Comparaison des TES simulé et théorique avec un mapping 4-aire');


%------------------- Séquence 3, 5.6, qst 3 et 4 --------------------------
figure(15); 
semilogy(TEBsim); hold on; semilogy(TEBth); hold off;
ylabel('TEB'); xlabel('Eb/N0 (dB)');
title('Comparaison des TEB théorique et simulé mapping 4-aire');
legend('TEB simulé',' TEB Théorique');

%------------------- Séquence 3, 5.3.2, qst 4 -----------------------------
%-------- Comparaison des TEB avec filtres de reception différents --------
figure(16); 
semilogy(TEBsim); hold on; semilogy(TEBsim1); hold off;
ylabel('TEB');xlabel('db');legend('TEB simulé',' TEB de la chaine de référence');
title('Comparaison des TEB avec filtre de reception différent');
