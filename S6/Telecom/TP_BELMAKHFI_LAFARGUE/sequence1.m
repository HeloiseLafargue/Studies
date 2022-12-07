% ------------------------------------------------------------------------
% Télécommunications : Etudes de chaines de transmission en bande de base                
% ------------------------------------------------------------------------

clear
close all

Fe = 24000;
Te = 1 / Fe;
Rb = 3000;
Tb = 1 / Rb;

%% ______________ Etude de modulateurs bande de base ______________________

%----------------------Modulateur 1----------------------------------------

n1 = 1;% car 1 symboles binaire
Ns1 = n1 * Tb / Te;
Ts1 = Ns1 * Te; 

nb_bits=1000;% nb de bits générés
bits=randi([0,1],1,nb_bits); % Génération info binaire
Symboles=2*bits-1;% Mapping binaire à moyenne nulle:0->-1, 1->1
%Génération de la suite de Diracs pondérés par les symboles 
Suite_diracs=kron(Symboles, [1 zeros(1,Ns1-1)]);
h1=ones(1,Ns1); %Génération de la réponse impulsionnelle du filtre de mise en forme
x1=filter(h1,1,Suite_diracs); %Filtrage de mise en forme
% Calcul DSP
Nf = 4096; 
freqcentered = linspace(-Fe/2, Fe/2, Nf);
DSP1 = pwelch(x1,[],[],Nf,Fe,"centered");
taille = length(DSP1);
DSP1_theo = Ts1*sinc(freqcentered*Ts1).^2;

figure(1); sgtitle('Modulateur 1'); subplot(3,1,1);
plot(x1); axis([0 nb_bits-1 -1.5 1.5]); 
title("Signal temporel"); xlabel("Temps (s)");
figure(1); subplot(3,1,2); 
semilogy(freqcentered,DSP1); title("DSP");
xlabel("Fréquence (Hz)"); ylabel("DSP");
figure(1); subplot(3,1,3); 
semilogy(freqcentered,DSP1); title("Comparaison DSP et DSP théorique");
xlabel("Fréquence (Hz)"); ylabel("DSP");
hold on;
semilogy(freqcentered,DSP1_theo); legend('DSP','DSP théo');
hold off;

%----------------------Modulateur 2----------------------------------------

n2 = 2;  % car symboles 4-aires
Ns2 = n2 * Tb / Te;
Ts2 = Ns2 * Te; 

Symboles=bit2int(reshape(bits, 2, length(bits)/2), 2) - 1.5; % -1.5 -> moyenne nulle
Suite_diracs=kron(Symboles, [1 zeros(1,Ns2-1)]);
h2=ones(1,Ns2);
x2=filter(h2,1,Suite_diracs);

DSP2 = pwelch(x2,[],[],Nf,Fe,"centered");
DSP2_theo = (5/Ts2)*(sinc(Ts2*freqcentered)).^2;

figure(2); sgtitle('Modulateur 2');subplot(3,1,1); 
plot(x2); axis([0 nb_bits-1 -1.5 1.5]);
title("Signal temporel");xlabel("Temps (s)");
figure(2); subplot(3,1,2); 
semilogy(freqcentered,DSP2); title("DSP");
xlabel("Fréquence (Hz)"); ylabel("DSP");
figure(2); subplot(3,1,3); 
semilogy(freqcentered,DSP2/max(DSP2)); title("Comparaison DSP et DSP théorique");
xlabel("Fréquence (Hz)"); ylabel("DSP");
hold on;
semilogy(freqcentered,DSP2_theo/max(DSP2_theo)); legend('DSP','DSP théo');
hold off;

%----------------------Modulateur 3----------------------------------------

n3 = 1;  % car symboles binaires
Ns3 = n3 * Tb / Te;
Ts3 = Ns3 * Te; 

bits=randi([0,1],1,nb_bits);
Symboles=2*bits-1;
Suite_diracs=kron(Symboles, [1 zeros(1,Ns1-1)]);
alpha = 0.5; L = 5;
h3 = rcosdesign(alpha, L, Ns3);
x3=filter(h3,1,Suite_diracs);

DSP3 = pwelch(x3,[],[],Nf,Fe,"centered");

ak3 = ((bits(1:2:end)*2 + bits(2:2:end))-1.5)*2; % car M=4
condition = (1-alpha)/ (2*Ts3);
sigma1 = sqrt(var(ak3));
f3 = linspace(-Fe/2, Fe/2, length(DSP3));
C1 = sigma1^2 * Ts3;
C2 = sigma1^2 * Ts3/4 * (1+cos(pi*Ts3/alpha* (abs(f3) - condition))).^2;
DSP3_theo = C1.*(abs(f3) <= condition) + C2.*(abs(f3) >= condition & abs(f3) <= (1 + alpha)/(2*Ts3));

figure(3); sgtitle('Modulateur 3');subplot(3,1,1);
plot(x3); axis([0 nb_bits-1 -1.5 1.5]);
title("Signal temporel"); xlabel("Temps (s)");
figure(3); subplot(3,1,2); 
semilogy(freqcentered,DSP3);title("DSP"); 
xlabel("Fréquence (Hz)"); ylabel("DSP");
figure(3); subplot(3,1,3);
semilogy(freqcentered,DSP3/max(DSP3)); title("Comparaison DSP et DSP théorique");
xlabel("Fréquence (Hz)"); ylabel("DSP");
hold on;
semilogy(freqcentered,DSP3_theo/max(DSP3_theo)); legend('DSP','DSP théo');
hold off;

%---------------------- DSP des signaux -----------------------------------

figure(4);
semilogy(freqcentered,DSP1);
xlabel("Fréquence (Hz)"); ylabel("DSP");
title("Comparaison des DSP des modulateurs");
hold on;
semilogy(freqcentered,DSP2);
semilogy(freqcentered,DSP3);
legend('modulateur 1','modulateur 2', 'modulateur 3');
hold off;
