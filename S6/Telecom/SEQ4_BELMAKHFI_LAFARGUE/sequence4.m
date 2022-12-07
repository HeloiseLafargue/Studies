%------------------------------------------------------------------------%
% Première année : Sciences du numérique
% Etudes de chaines de transmission sur fréquence porteuse
% Auteurs : LAFARGUE Héloise et BELMAKHFI Nihal
%------------------------------------------------------------------------%
%--------3/1 Implantation de la chaine passe-bas équivalente------------

clear;
close all;

alpha = 0.35; % roll-off
fp = 2000;
Fe = 10000;
Te = 1/Fe;
M = 4;
n = log2(M);
Rb = 2000;
Tb = 1 / Rb;
Ns = n*Tb / Te;
nb_bits = 3000;
L = 5;
h = rcosdesign(alpha,L ,Ns);
hr = h;
h_passe_bas = ones(1,3);
bits = randi([0, 1], 1, nb_bits);

%Mapping complexe
symboles_a=2*bits(1: 2: end)-1; 
symboles_b=2*bits(2: 2: end)-1;
symboles_d = symboles_a + 1i*symboles_b;

%Suite de dirac pondérés par les symboles
suite_diracs_a = kron(symboles_a,[1 zeros(1, Ns-1)]);
suite_diracs_b = kron(symboles_b,[1 zeros(1, Ns-1)]);
suite_diracs_d = kron(symboles_d,[1 zeros(1, Ns-1)]);

%Mise en forme
s = filter(h,1,[suite_diracs_d zeros(1,L*Ns/2)]);
s = s(L*Ns/2 +1 : end);

%----3/1, qst1 Tracage des signaux générés sur les voies en phase et en quadrature ------
figure(1); sgtitle('Signaux'); subplot(3,1,1);
%extraire le signal réel
s_real = real(s);
plot(s_real)
axis([0 nb_bits-1 -1 1]);
title("Signal généré sur les voies en phase");
ylabel("Signal généré(en phase)"); xlabel("Temps (s)");
figure(1); subplot(3,1,2); 
s_imaginaire = imag(s);
plot(s_imaginaire)
axis([0 nb_bits-1 -1 1])
title("Signal généré sur les voies en quadrature")
ylabel("Signal généré(quadrature)"); xlabel("Temps (s)");

%----3/1, qst1 Tracage du signal transmis sur fréquence porteuse ------
figure(1); subplot(3,1,3); 
x = real(s.*exp(2*i*pi*fp*[1:length(s)]/Fe));
plot(x)
axis([0 nb_bits-1 -1 1])
title("Signal tranmis sur la fréquence porteuse")
ylabel("Signal transmis"); xlabel("Temps (s)");

%----3/1, qst2 densité spectrale de puissance du signal modulé sur fréquence porteuse ------
figure(2);
N = 2^nextpow2(length(x));
DSP = 1/length(x) * abs(fft(x,N)).^2;
semilogy(linspace(-1/Ns,1/Ns,N),fftshift(DSP));
title("Densité spectrale de puissance du signal modulé sur fréquence porteuse");
xlabel("Fréquence (Hz)");
ylabel("DSP");

%----3/1, qst3 Implantation de la chaine complète sans bruit et vérification que TEB est nul-------

%bande de base
s1 = x.*cos(2*pi*fp*[1:length(s)]/Fe);
s2 = x.*sin(2*pi*fp*[1:length(s)]/Fe);

%filtrer 
s1_filtre = filter(h_passe_bas,1,s1);
s2_filtre = filter(h_passe_bas,1,s2);
s_filtre    = s1_filtre -1i* s2_filtre;
xr = filter(hr,1,[s_filtre zeros(1,L*Ns/2)]);
xr = xr(L*Ns/2 +1:end);

%Echantillonnage
x_ech = xr(1:Ns:end);

%Demapping
bits_recu_bruit = zeros(1,nb_bits);
symboles_ar = (sign(real(x_ech))+1)/2;
symboles_br = (sign(imag(x_ech))+1)/2; 
bits_recu_bruit(1:2:end) = symboles_ar;
bits_recu_bruit(2:2:end) = symboles_br;

%TEB 
nb_erreurs = length(find(bits ~= bits_recu_bruit));
TEB_sim = nb_erreurs/length(bits);
fprintf("Le TEB obtenu sans bruit est %f \n",TEB_sim)

%--------3/1, qst4 tracons le taux d'erreur binaire aprés ajout du bruit--------
Eb_div_N0=[0:6];
figure(3);
for i = 1:length(Eb_div_N0)
    %Calcul de sigma
    sigma_bruit = mean(abs(x).^2)*Ns/(2*n*10^(Eb_div_N0(i)/10));
    
    %Génération et ajout du bruit
    bruit = sqrt(sigma_bruit)*randn(1,length(x));
    x_new = x + bruit;
    
    %bande de base
    xi1 = x_new.*cos(2*pi*fp*[1:length(s)]/Fe);
    xi2 = x_new.*sin(2*pi*fp*[1:length(s)]/Fe);
    
    %Passage par un filtre passe bas.
    xi1_filtre = filter(h_passe_bas,1,xi1);
    xi2_filtre = filter(h_passe_bas,1,xi2);
    xi_filtre    = xi1_filtre -1i* xi2_filtre;
    
    %Passage par filtre de réception
    xi_r = filter(hr,1,[xi_filtre zeros(1,L*Ns/2)]);
    xi_r = xi_r(L*Ns/2+1:end);
    
    %Echantillonnage
    xi_ech = xi_r(1:Ns:end);
    
    %Demapping
    bits_recu_bruit = zeros(1,nb_bits);
    symboles_ar = (sign(real(xi_ech))+1)/2;
    symboles_br = (sign(imag(xi_ech))+1)/2; 
    bits_recu_bruit(1:2:end) = symboles_ar;
    bits_recu_bruit(2:2:end) = symboles_br;
    
    %Calcul du TEB
    nb_erreurs_bruit = length(find(bits ~= bits_recu_bruit));
    TEB_sim(i) = nb_erreurs_bruit/length(bits);
    
end
semilogy(Eb_div_N0,TEB_sim);
title("Taux d'erreur binaire obtenu avec bruit en fonction du rapport signal bruit");
xlabel("Eb/N0 (dB)");
ylabel("TEB");
grid on;


%-------------3/1, Comparaison entre TEB simulé et TEB théorique--------------------
figure(4);
TEB_th = qfunc(sqrt(2*10.^(Eb_div_N0/10)));
semilogy(Eb_div_N0,TEB_sim)
hold on
semilogy(Eb_div_N0,TEB_th);
title("Comparaison entre TEB simulé et TEB théorique");
xlabel("Eb/N0 (dB)");
ylabel("TEB");
legend("TEB simulé","TEB théorique");
grid on;

%---------3/1/1, Implantation de la chaine passe-bas equivalente---------
%----3/1/1, qst1 Tracage des signaux générés sur les voies en phase et en quadrature ------
figure(5); sgtitle('Signaux'); subplot(2,1,1);
plot(s_real);
axis([0 nb_bits-1 -1 1])
title("Signal généré sur la voie en phase");
ylabel("Signal généré(en phase)"); xlabel("Temps (s)");

figure(5);subplot(2,1,2);
plot(s_imaginaire);
axis([0 nb_bits-1 -1 1])
title("Signal généré sur la voie en quadrature");
ylabel("Signal généré(quadrature)"); xlabel("Temps (s)");

%----3/1/1, qst2 DSP de l'enveloppe complexe associée au signal modulé sur fréquence porteuse
x = s;
figure(6); subplot(1,2,1);
N = 2^nextpow2(length(x));
dsp_x_1 = 1/length(x) * abs(fft(x,N)).^2;
semilogy(linspace(-1/Ns,1/Ns,N),fftshift(dsp_x_1));
title("DSP de l'enveloppe complexe associée au signal modulé sur fréquence porteuse");
xlabel("Fréquence (Hz)");
ylabel("DSP");

figure(6); subplot(1,2,2);
semilogy(linspace(-1/Ns,1/Ns,N),fftshift(dsp_x_1));
hold on;
semilogy(linspace(-1/Ns,1/Ns,N),fftshift(DSP));
title("Comparaison entre les deux DSPs des deux parties");
xlabel("Fréquence (Hz)");
ylabel("DSP");
legend("Chaine passe-bas équivalente","Chaine sur fréquence porteuse")

%----3/1/1, qst3, Implantation de la chaine complète sans bruit pour
%vérifier que TEB=0

%filtre de réception
xr = filter(hr,1,[x zeros(1,L*Ns/2)]);
xr = xr(L*Ns/2 +1:end);

%echantillonnage
x_ech = xr(1:Ns:end);

%Demapping
received_bits = zeros(1,nb_bits);
symboles_ar = (sign(real(x_ech))+1)/2;
symboles_br = (sign(imag(x_ech))+1)/2; 
received_bits(1:2:end) = symboles_ar;
received_bits(2:2:end) = symboles_br;

%TEB 
nb_erreurs = length(find(bits ~= received_bits));
TEB = nb_erreurs/length(bits);
fprintf("Le TEB obtenu sans bruit est bien %f \n",TEB)

%----3/1/1, qst4, ajout du bruit et tracage du taux d'erreur 
figure(7);
for i = 1:length(Eb_div_N0)
    %sigma
    sigma1 = mean(abs(x).^2)*Ns/(2*log2(M)*10^(Eb_div_N0(i)/10));
    
    %bruit
    b_real = sqrt(sigma1)*randn(1,length(real(x)));
    b_img = sqrt(sigma1)*randn(1,length(imag(x)));
    b_complex = b_real + 1i * b_img;
    r = x + b_complex;
    
    %Passage par filtre de réception
    xi_r = filter(hr,1,[r zeros(1,L*Ns/2)]);
    xi_r = xi_r(L*Ns/2 +1:end);
    
    %Echantillonnage
    xi_ech = xi_r(1:Ns:end);
    
    %Demapping
    bits_recu_i = zeros(1,nb_bits);
    symboles_ai_r = (sign(real(xi_ech))+1)/2;
    symboles_bi_r = (sign(imag(xi_ech))+1)/2; 
    bits_recu_i(1:2:end) = symboles_ai_r;
    bits_recu_i(2:2:end) = symboles_bi_r;
    
    %TEB
    nb_erreurs = length(find(bits ~= bits_recu_i));
    TEB2(i) = nb_erreurs/length(bits);
    
end
semilogy(Eb_div_N0,TEB2,'x-');
title("Taux d'erreur binaire TEB aprés ajout du bruit");
xlabel("Eb/N0 (dB)");
ylabel("TEB");
grid on;

%----3/1/1, qst5, tracage des constellations en sortie du mapping et en sortie de l'échantillonneur 
% l'échantillonneur pour valeur donnée de Eb/N0.
scatterplot(symboles_d);
title("Constellations en sortie du mapping");
%Pour une valeur de Eb/N0 égale à 6dB.
scatterplot(xi_ech);
title("Constellations en sortie de l'échantillonneur pour différentes valeurs de Eb/N0");

%---3/1/1, qst 6, Comparaison TEB de la chaine passe-bas équivalente et de la chaine
% sur fréquence porteuse.
figure();
semilogy(Eb_div_N0,TEB2);
hold on;
semilogy(Eb_div_N0,TEB_sim);
title("Taux d'erreur binaire pour les deux chaines: simulée sur fréquence porteuse et chaine passe bas équivalente");
xlabel("Eb/N0 en dB");
ylabel("TEB");
legend("Chaine passe-bas équivalente","Chaine sur fréquence porteuse");
grid on;


% -------4.2.1 - Etude de chaque chaine de transmission----------------
%Roll off du filtre de mise en forme
alpha = 0.5; 
% Cosinus sureleveh 
h=  rcosdesign(alpha,L, Ns); 
hr = fliplr(h);

%----4.2.1/, qst1, Implantation de la chaine complète sans bruit afin de vérifier que TEB est nul
dirac = [1 zeros(1, Ns-1)];
%Modulation 4-ASK
    %Mapping 
    symboles_4ASK = (2*bi2de(reshape(bits,2,length(bits)/2).') - 3).';
    
    %Suite de Dirac spondérés par les symboles 4ASK.
    suite_diracs_4ASK = kron(symboles_4ASK,dirac);

    %Mise en forme du signal
    xe_4ASK = filter(h,1,[suite_diracs_4ASK zeros(1,L*Ns/2)]);
    xe_4ASK = xe_4ASK(L*Ns/2 +1 : end);

    %Passage par filtre de réception
    x_4ASK_r = filter(hr,1,[xe_4ASK zeros(1,L*Ns/2)]);
    x_4ASK_r = x_4ASK_r(L*Ns/2 +1:end);

    %Echantillonnage
    x_4ASK_ech = x_4ASK_r(1:Ns:end);
    
    %Demapping
    symb_4ASK = zeros(1,length(x_4ASK_ech));
    for i=1:length(symb_4ASK)
        if (x_4ASK_ech(i)>=2)
            symb_4ASK(i) = 3;
        elseif (x_4ASK_ech(i) >= 0)
            symb_4ASK(i) = 1;
        elseif (x_4ASK_ech(i) <= -2)
            symb_4ASK(i) = -3;
        else
            symb_4ASK(i) = -1;
        end
    end
    demap_4ASK = reshape(de2bi((symb_4ASK+3)/2).',1,nb_bits);
    nb_erreurs_4ASK = length(find(bits ~= demap_4ASK));
    TEB_obtenu_4ASK = nb_erreurs_4ASK/length(bits);
    
fprintf(" Le TEB obtenu sans bruit pour la modulation 4ASK est bien :%f \n",TEB_obtenu_4ASK);

%Modulation QPSK
bits_QPSK = reshape(bits,length(bits)/2,2);
bits_QPSK = bi2de(bits_QPSK);
bits_QPSK = bits_QPSK';
map_QPSK = pskmod(bits_QPSK,4,pi/4,'gray');
demap_QPSK = pskdemod(map_QPSK,4,pi/4,'gray');
bits_estimes = de2bi(demap_QPSK,3);
bits_estimes = bits_estimes(:)';
nb_erreurs_QPSK = length(find(bits_QPSK ~= demap_QPSK));
TEB_obtenu_QPSK = nb_erreurs_QPSK/length(bits);
fprintf("Le TEB obtenu sans bruit pour la modulation QPSK est : %f \n",TEB_obtenu_QPSK);

%Modulation : 8-PSK
%On ajoute deux zeros pour les bits transférés pour pouvoir
%faire une modulation 8-PSK.
bits_8PSK = reshape([bits 0 0 0],length([bits 0 0 0])/3,3);
bits_8PSK = bi2de(bits_8PSK);
bits_8PSK = bits_8PSK';
map_8PSK = pskmod(bits_8PSK,8,pi/8,'gray');
demap_8PSK = pskdemod(map_8PSK,8,pi/8,'gray');
nb_erreurs_8PSK = length(find(bits_8PSK ~= demap_8PSK));
TEB_obtenu_8PSK = nb_erreurs_8PSK/length(bits);
fprintf("Le TEB obtenu sans bruit pour la modulation 8-PSK est : %f \n",TEB_obtenu_8PSK);

%Modulation 16-QAM
bits_16QAM = reshape(bits,length(bits)/4,4);
bits_16QAM = bi2de(bits_16QAM);
bits_16QAM = bits_16QAM';
map_16QAM = qammod(bits_16QAM,16,'gray');
demap_16QAM = qamdemod(map_16QAM,16,'gray');
nb_erreurs_16QAM = length(find(bits_16QAM ~= demap_16QAM));
TEB_obtenu_16QAM = nb_erreurs_16QAM/length(bits);
fprintf("Le TEB obtenu sans bruit pour la modulation 16-QAM est : %f \n",TEB_obtenu_16QAM);





%-------------------4.2.1, qst 2 Rajout de bruit--------------------------------
% tracé des constellations en sortie du mapping et sortie
% d'échantillonneur pour différentes valeur de Eb/N0

scatterplot(symboles_4ASK);
title("Les constellations en sortie du mapping : 4ASK");
scatterplot(map_QPSK);
title("Les constellations en sortie du mapping : QPSK");
scatterplot(map_8PSK);
title("Les constellations en sortie du mapping : 8-PSK ");
scatterplot(map_16QAM);
title("Les constellations en sortie du mapping : 16-QAM");

TEB_4ASK = zeros(1,length(Eb_div_N0));
TEB_QPSK = zeros(1,length(Eb_div_N0));
TEB_8PSK = zeros(1,length(Eb_div_N0));
TEB_16QAM = zeros(1,length(Eb_div_N0));

for i = 1:length(Eb_div_N0)
    
    %Génération de la suite de Dirac spondérés par les symboles d.
    suite_diracs_QPSK = kron(map_QPSK,dirac);
    suite_diracs_8PSK = kron(map_8PSK,dirac);
    suite_diracs_16QAM = kron(map_16QAM,dirac);
    suite_diracs_4ASK = kron(symboles_4ASK,dirac);
    
    %Mise en forme du signal
    xe_4ASK = filter(h,1,[suite_diracs_4ASK zeros(1,L*Ns/2)]);
    xe_4ASK = xe_4ASK(L*Ns/2 +1 : end);
    
    xe_QPSK = filter(h,1,[suite_diracs_QPSK zeros(1,L*Ns/2)]);
    xe_QPSK = xe_QPSK(L*Ns/2 +1 : end);
    
    xe_8PSK = filter(h,1,[suite_diracs_8PSK zeros(1,L*Ns/2)]);
    xe_8PSK = xe_8PSK(L*Ns/2 +1 : end);
    
    xe_16QAM = filter(h,1,[suite_diracs_16QAM zeros(1,L*Ns/2)]);
    xe_16QAM = xe_16QAM(L*Ns/2 +1 : end);
    
    %puissance
    sigma2_4ASK = mean(abs(xe_4ASK).^2)*Ns/(2*log2(4)*10^(Eb_div_N0(i)/10));
    sigma2_QPSK = mean(abs(xe_QPSK).^2)*Ns/(2*log2(4)*10^(Eb_div_N0(i)/10));
    sigma2_8PSK = mean(abs(xe_8PSK).^2)*Ns/(2*log2(8)*10^(Eb_div_N0(i)/10));
    sigma2_16QAM = mean(abs(xe_16QAM).^2)*Ns/(2*log2(16)*10^(Eb_div_N0(i)/10));
    
    %u bruit
    br = randn(1,length(real(x)));
    bi = randn(1,length(imag(x)));
    
    b_4ASK = sqrt(sigma2_4ASK)*(br + 1i * bi);
    b_QPSK = sqrt(sigma2_QPSK)*(randn(1,length(real(xe_QPSK))) + 1i * randn(1,length(imag(xe_QPSK))));
    b_8PSK = sqrt(sigma2_8PSK)*(randn(1,length(real(xe_8PSK))) + 1i * randn(1,length(imag(xe_8PSK))));
    b_16QAM = sqrt(sigma2_16QAM)*(randn(1,length(real(xe_16QAM))) + 1i * randn(1,length(imag(xe_16QAM))));
    
    r_4ASK = xe_4ASK + b_4ASK;
    r_QPSK = xe_QPSK + b_QPSK;
    r_8PSK = xe_8PSK + b_8PSK;
    r_16QAM = xe_16QAM + b_16QAM;

    
    %filtre de réception
    xi_r_4ASK = filter(hr,1,[r_4ASK zeros(1,L*Ns/2)]);
    xi_r_4ASK = xi_r_4ASK(L*Ns/2 +1:end);
    
    xi_r_QPSK = filter(hr,1,[r_QPSK zeros(1,L*Ns/2)]);
    xi_r_QPSK = xi_r_QPSK(L*Ns/2 +1 : end);
    
    xi_r_8PSK = filter(hr,1,[r_8PSK zeros(1,L*Ns/2)]);
    xi_r_8PSK = xi_r_8PSK(L*Ns/2 +1 : end);
    
    xi_r_16QAM = filter(hr,1,[r_16QAM zeros(1,L*Ns/2)]);
    xi_r_16QAM = xi_r_16QAM(L*Ns/2 +1 : end);

    %Echantillonnage
    xi_ech_4ASK = xi_r_4ASK(1:Ns:end);
    xi_ech_QPSK = xi_r_QPSK(1:Ns:end);
    xi_ech_8PSK = xi_r_8PSK(1:Ns:end);
    xi_ech_16QAM = xi_r_16QAM(1:Ns:end);
    
    %Les constellations en sortie de l'échantillonneur pour Eb/N0 = 4dB
    if (i == 6)
        scatterplot(xi_ech_4ASK);
        title("Les constellations en sortie de l'échantillonneur pour Eb/N0 = " + Eb_div_N0(i) + " : 4ASK");
        scatterplot(xi_ech_QPSK);
        title("Les constellations en sortie de l'échantillonneur pour Eb/N0 = " + Eb_div_N0(i) + " : QPSK");
        scatterplot(xi_ech_8PSK);
        title("Les constellations en sortie de l'échantillonneur pour Eb/N0 = " + Eb_div_N0(i) + " : 8-PSK ");
        scatterplot(xi_ech_16QAM);
        title("Les constellations en sortie de l'échantillonneur pour Eb/N0 = " + Eb_div_N0(i) + " : 16-QAM");  
    end
    
    %Demapping (4ASK)
    symb_i_4ASK = zeros(1,length(xi_ech_4ASK));
    for j=1:length(symb_i_4ASK)
        if (xi_ech_4ASK(j)>=2)
            symb_i_4ASK(j) = 3;
        elseif (xi_ech_4ASK(j) >= 0)
            symb_i_4ASK(j) = 1;
        elseif (xi_ech_4ASK(j) <= -2)
            symb_i_4ASK(j) = -3;
        else
            symb_i_4ASK(j) = -1;
        end
    end
    demap_i_4ASK = reshape(de2bi((symb_i_4ASK+3)/2).',1,nb_bits);
    
    %Demapping (QPSK, 8-PSK, 16-QAM)
    demap_i_QPSK = de2bi(pskdemod(xi_ech_QPSK,4,pi/4,'gray'),2);
    demap_i_QPSK = demap_i_QPSK(:)';
    
    demap_i_8PSK = de2bi(pskdemod(xi_ech_8PSK,8,pi/8,'gray'),3);
    demap_i_8PSK = demap_i_8PSK(:)';
    
    demap_i_16QAM = de2bi(qamdemod(xi_ech_16QAM,16,'gray'),4);
    demap_i_16QAM = demap_i_16QAM(:)';
    
    %Calcul du TEB
    nb_erreurs_i_4ASK = length(find(bits ~= demap_i_4ASK));
    TEB_4ASK(i) = nb_erreurs_i_4ASK/length(bits);

    nb_erreurs_QPSK = length(find(bits ~= demap_i_QPSK));
    TEB_QPSK(i) = nb_erreurs_QPSK/length(bits);
    
    nb_erreurs_8PSK = length(find([bits 0 0 0] ~= demap_i_8PSK));
    TEB_8PSK(i) = nb_erreurs_8PSK/length(bits);
    
    nb_erreurs_16QAM = length(find(bits ~= demap_i_16QAM));
    TEB_16QAM(i) = nb_erreurs_16QAM/length(bits);
    
end


%------------ comparaison entre TEB théorique et simulés ---------
figure();
subplot(2,2,1),semilogy(Eb_div_N0,TEB_4ASK,'*-');
hold on;
TEB_4ASK_Theorique = (1-1/4)*qfunc(sqrt((12/15)*10.^(Eb_div_N0/10)));
semilogy(Eb_div_N0,TEB_4ASK_Theorique,'o-');
legend("TEB Simulé","TEB Théorique");
title("TEB : Modulation 4ASK");
grid on;

subplot(2,2,2),semilogy(Eb_div_N0,TEB_QPSK,'*-');
hold on;
TEB_QPSK_Theorique = qfunc(sqrt(2*10.^(Eb_div_N0/10)));
semilogy(Eb_div_N0,TEB_QPSK_Theorique,'o-');
legend("TEB Simulé","TEB Théorique");
title("TEB : Modulation QPSK");
grid on;

subplot(2,2,3),semilogy(Eb_div_N0,TEB_8PSK,'*-');
hold on;
TEB_8PSK_Theorique = 2/3*qfunc(sqrt(2*sin(pi/8)*10.^(Eb_div_N0/10)));
semilogy(Eb_div_N0,TEB_8PSK_Theorique,'o-');
legend("TEB Simulé","TEB Théorique");
title("TEB : Modulation 8PSK");
grid on;

subplot(2,2,4),semilogy(Eb_div_N0,TEB_16QAM,'*-');
hold on;
TEB_16QAM_Theorique = (1-1/4)*qfunc(sqrt((12/15)*10.^(Eb_div_N0/10)));
semilogy(Eb_div_N0,TEB_16QAM_Theorique,'o-');
legend("TEB Simulé","TEB Théorique");
title("TEB : Modulation 16QAM");
grid on;

% % -----------------4.2.2, qst 1 Comparaison des chaines de
% % transmission-----
% (1) - Superposition des 4 tracés des TEBs obtenus
figure();
semilogy(Eb_div_N0,TEB_4ASK);
hold on;
semilogy(Eb_div_N0,TEB_QPSK);
semilogy(Eb_div_N0,TEB_8PSK);
semilogy(Eb_div_N0,TEB_16QAM);
legend("TEB simulé 4-ASK","TEB simulé QPSK","TEB simulé 8-PSK","TEB simulé 16-QAM");
title("Comparaison des TEB simulés pour les modulations ASK, PSK et QAM");
grid on;
xlabel("Eb/N0 (en dB)");
ylabel("TEB");

figure();
semilogy(Eb_div_N0,TEB_4ASK_Theorique,'*-');
hold on;
semilogy(Eb_div_N0,TEB_QPSK_Theorique,'o-');
semilogy(Eb_div_N0,TEB_8PSK_Theorique,'v-');
semilogy(Eb_div_N0,TEB_16QAM_Theorique,'x-');
legend("TEB théorique 4-ASK","TEB théorique QPSK","TEB théorique 8-PSK","TEB théorique 16-QAM");
title("Comparaison des TEB théoriques pour les modulations ASK, PSK et QAM");
grid on;
xlabel("Eb/N0 (en dB)");
ylabel("TEB");

%------4.2.2, qst 2 Densités spectrales de puissance des signaux émis dans les
% différentes chaines de transmision étudiées.
figure();
N_sup_2 = 2^nextpow2(length(xe_4ASK));
dsp_x_4ASK = 1/length(xe_4ASK) * abs(fft(xe_4ASK,N_sup_2)).^2;
semilogy(linspace(-1/Ns,1/Ns,N_sup_2),fftshift(dsp_x_4ASK));
hold on;
N_sup_2 = 2^nextpow2(length(xe_QPSK));
dsp_x_QPSK = 1/length(xe_QPSK) * abs(fft(xe_QPSK,N_sup_2)).^2;
semilogy(linspace(-1/Ns,1/Ns,N_sup_2),fftshift(dsp_x_QPSK));

N_sup_2 = 2^nextpow2(length(xe_8PSK));
dsp_x_8PSK = 1/length(xe_8PSK) * abs(fft(xe_8PSK,N_sup_2)).^2;
semilogy(linspace(-1/Ns,1/Ns,N_sup_2),fftshift(dsp_x_8PSK));

N_sup_2 = 2^nextpow2(length(xe_16QAM));
dsp_x_16QAM = 1/length(xe_16QAM) * abs(fft(xe_16QAM,N_sup_2)).^2;
semilogy(linspace(-1/Ns,1/Ns,N_sup_2),fftshift(dsp_x_16QAM));
legend("DSP avec la chaine 4-ASK","DSP avec la chaine QPSK","DSP avec la chaine 8-PSK","DSP avec la chaine 16-QAM");
title("DSPs des signaux émis")
xlabel("Fréquences normalisées")
ylabel("Densité spectrale de puissance(comparaison)")




