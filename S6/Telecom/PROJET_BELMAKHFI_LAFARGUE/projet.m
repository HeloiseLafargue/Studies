close all
clear all

%--------------2.2/Impact d'un canal de propagation multitrajets: Implémentation sur matlab------------
%--------------2.2.2/Implémentation de la chaine de transmission sans canal et vérification que TEB est nul------------

Fe = 24000; Te = 1/Fe; Rb = 3000; N = 10000; 
bits = randi([0,1], 1, N); 
%bits = [0,1,1,0,0,1];
M = 2;
Rs = Rb/log2(M); Ns = Fe/Rs; Ts = Ns*Te;
a0 = 1; a1 = 0.5; t0 = 0; t1 = t0+Ts;

% Mapping
Symboles = 2*bits - 1;
Suite_diracs = kron(Symboles, [1 zeros(1,Ns-1)]);
h = ones(1,Ns);
hc = kron([a0 a1], [1 zeros(1,Ns-1)]);
hr = fliplr(h);

% Filtrage
retard = length(h)/2;
xe = filter(h,1,[Suite_diracs, zeros(1,retard)]);
xe = xe(retard+1:end);

% Canal 
Px=mean(abs(xe).^2);
Eb_div_N0 = 0:10;
TEB_without_canal = [];

for i = 1:length(Eb_div_N0) 
    Eb_div_N0_i = Eb_div_N0(i);
    Eb_div_N0_i = 10^(Eb_div_N0_i/10);
    sigma = sqrt((Px*Ns) / (2*log2(M)*Eb_div_N0_i));
    bruit = sigma * randn(1,length(xe));
    
    % Chaine avec bruit seulement
    x_without_canal = xe + bruit;
    x_with_canal = x_without_canal;
    xr = filter(hr,1,[x_with_canal, zeros(1, retard)]);
    xr = xr(retard+1:end);
    xr_ech_without_canal = xr(1:Ns:end);
    xr_sign = sign(xr_ech_without_canal);
    bits_reconstruits = (xr_sign + 1)/2;
    
    % Calcul du TEB
    TEB_i = length(find(bits ~= bits_reconstruits))/length(bits);
    TEB_without_canal = [TEB_without_canal TEB_i];
    
end
fprintf("2.2.1) Le TEB de liason dans le cas de transmission sans canal est %f \n",min(TEB_without_canal));

%--------------2.2.3/Implémentation de la chaine de transmission avec le filtrage du canal et vérification que TEB est nul------------

% Mapping
Symboles = 2*bits - 1;
Suite_diracs = kron(Symboles, [1 zeros(1,Ns-1)]);
h = ones(1,Ns);
hc = kron([1 0.5], [1 zeros(1,Ns-1)]);
hr = fliplr(h);

% Filtrage
retard = length(h)/2;
xe = filter(h,1,[Suite_diracs, zeros(1,retard)]);
xe = xe(retard+1:end);

% Canal 
Px=mean(abs(xe).^2);
Eb_div_N0 = 0:10;
TEB_with_canal = [];
TEB_Theo = [];

for i = 1:length(Eb_div_N0) 
    Eb_div_N0_i = Eb_div_N0(i);
    Eb_div_N0_i = 10^(Eb_div_N0_i/10);
    sigma = sqrt((Px*Ns) / (2*log2(M)*Eb_div_N0_i));
    bruit = sigma * randn(1,length(xe));
    
    % Chaine avec filtre canal
    x_with_canal = filter(hc,1,xe);
    x_with_canal = x_with_canal + bruit;
    xr_with_canal = filter(hr,1,[x_with_canal, zeros(1, retard)]);
    xr_with_canal = xr_with_canal(retard+1:end);
    xr_ech_canal = xr_with_canal(1:Ns:end);
    xr_sign = sign(xr_ech_canal);
    bits_reconstruits = (xr_sign + 1)/2;
    
    % Calcul du TEB
    TEB_i = length(find(bits ~= bits_reconstruits))/length(bits);
    TEB_with_canal = [TEB_with_canal TEB_i];
    TEB_theo_i = 0.5*qfunc(sqrt((2/5)*Eb_div_N0_i)) + 0.5*qfunc(sqrt((18/5)*Eb_div_N0_i));
    TEB_Theo = [TEB_Theo TEB_theo_i];
    
end

%affichage du signal en sortie du filtre de réception en cas d'utilisation du filtrage du canal
figure();
plot(xr_with_canal); title('Signal en sortie du filtre de reception');
xlabel('Temps (s)'); ylabel('x(t)');

%diagramme d'oeil
figure();
plot(reshape(xr_with_canal,[],length(xr_with_canal)/Ns));
title("Diagramme de l'oeil en sortie du filtre de reception");
xlabel('n'); ylabel('x(n)');

%Constellation 
scatterplot(xr_ech_canal); title('Constellation avec canal');
scatterplot(xr_ech_without_canal); title('Constellation sans canal');

%comparaison entre TEB
figure(); sgtitle("Impact d'un canal de propagation multitrajet");
subplot(2,1,1);
semilogy(Eb_div_N0, TEB_with_canal); hold on ;
semilogy(Eb_div_N0, TEB_Theo);
xlabel('Eb/N0'); legend('TEB canal','TEB théorique');
title('Comparaison des TEB canal et théorique');
subplot(2,1,2);
semilogy(Eb_div_N0, TEB_with_canal); hold on ;
fprintf("2.2.3.c) Le TEB de liaison dans le cas de transmission avec canal est %f \n",TEB_without_canal(end));
semilogy(Eb_div_N0, TEB_without_canal);
xlabel('Eb/N0'); legend('TEB avec canal','TEB sans canal');
title('Comparaison des TEB avec canal et sans canal');
hold off;


%---------------------------3.2 Egalisation ZFE------------------------------

%Implémentation de la chaine avec égalisation 
Suite_diracs_d = [1 zeros(1,N-1)];
retard = length(h)/2;
xe = filter(hc,1,[Suite_diracs_d, zeros(1, retard)]);
xe = xe(retard+1:end);
x_with_canal = filter(hc,1,xe);
xr = filter(hr,1,[x_with_canal, zeros(1, retard)]);
xr = xr(retard+1:end);
x_ech = xr(1:Ns:end);

Z = x_ech(1:2*Ns); 
Zt = toeplitz(Z, [x_ech(1) zeros(1,2*Ns-1)]);
Y = [1 zeros(1,size(Zt,2)-1)].';
C = Zt\Y;
C = -C(2:end);
C = C.';

% Reponses en frequence du canal de propagation
hc0 = [a0 a1];
figure(); sgtitle('Egalisation ZFE');
subplot(2,2,1);
semilogy(linspace(-Fe/2,Fe/2,2^8),fftshift(abs(fft(hc0,2^8))));
title('Reponse en frequence canal propagation');
xlabel('Frequence (Hz)'); ylabel('Hc(f)');
subplot(2,2,2);
semilogy(linspace(-Fe/2,Fe/2,2^8),fftshift(abs(fft(C,2^8))));
title('Reponse en frequence egalisateur');
xlabel('Frequence (Hz)'); ylabel('C(f)');
subplot(2,2,3);
semilogy(linspace(-Fe/2,Fe/2,2^8),fftshift(abs(fft(hc0,2^8).*fft(C,2^8))));
title('Reponse en frequence produit');
xlabel('frequence (Hz)'); ylabel('Hc(f)C(f)');

% Reponse impulsionnelle de la chiane de transmission 
g = conv(conv(h,hc),hr);
g_eg = conv(g,C);

figure(); sgtitle('Egalisation ZFE');
subplot(1,2,1); plot(g(1:Ns:end));
title('Réponse impulsionnelle sans egalisation');
xlabel('n'); ylabel('g(n)');
subplot(1,2,2); plot(g_eg(1:Ns:end));
title('Réponse impulsionnelle avec egalisation'); 
xlabel('n'); ylabel('g(n)');

% Filtrage du signal 
retard = length(h)/2;
xe = filter(h,1,[Suite_diracs, zeros(1,retard)]);
xe = xe(retard+1:end);

% Canal 
Px=mean(abs(xe).^2);
Eb_div_N0 = 0:10;
TEB_canal_egalisation = [];

for i = 1:length(Eb_div_N0) 
    Eb_div_N0_i = Eb_div_N0(i);
    Eb_div_N0_i = 10^(Eb_div_N0_i/10);
    sigma = sqrt((Px*Ns) / (2*log2(M)*Eb_div_N0_i)) ;
    bruit = sigma * randn(1,length(xe));
    
    x_with_canal = filter(hc,1,xe);
    x_with_canal = x_with_canal + bruit;
    x_with_canal = x_with_canal;
    xr = filter(hr,1,[x_with_canal, zeros(1, retard)]);
    xr = xr(retard+1:end);
    x_eg = filter(C,1,xr);
    xr_ech_canal = x_eg(1:Ns:end);
    xr_sign = sign(xr_ech_canal);
    bits_reconstruits = (xr_sign + 1)/2;
    
    % Calcul du TEB
    TEB_i = length(find(bits ~= bits_reconstruits))/length(bits);
    TEB_canal_egalisation = [TEB_canal_egalisation TEB_i];
end

%comparaison des constellations obtenues avant et aprés égalisation
scatterplot(xr_ech_canal); title('Constellation avant egalisation');
scatterplot(xr(1:Ns:end)); title('Constellation aprés egalisation');


%comparaison des constellations obtenues avec et sans égalisation
figure();
semilogy(Eb_div_N0, TEB_canal_egalisation); hold on ;
semilogy(Eb_div_N0, TEB_with_canal);
xlabel('Eb/N0'); legend('TEB canal avec egalisateur','TEB canal');
title('Comparaison des TEB pour un canal avec et sans egalisateur');
