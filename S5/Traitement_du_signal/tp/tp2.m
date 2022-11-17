clear
close all

%% Génération du signal à filtrer
tiledlayout(2,3);

% Exo 3.1

x = [];
T = [];
N = 90;
Fe = 10000;
f1 = 1000;
f2 = 3000;
A = 1;

for i = 1:N
    x = [x; A*(cos(2*pi*f1*i/Fe)+cos(2*pi*f1*i/Fe))];
    T = [T; i/Fe];
end

nexttile;
plot(T,x);
title("Signal temporel");
xlabel("Temps (s)");
ylabel("Signal (V)");

%------------------------------------------
h1 = [];
h2 = [];
Fe = 10000;
N = 100;
Nf = 2048;
f1 = 1000;
f2 = 3000;
Fc = 100;
f_tilde = Fc / Fe;
N01 = 5;
N02 = 30;
ordre = 2*N01 + 1;

for i = -N01:N01
    h1 = [h1; 2*f_tilde*sinc(2*f_tilde*i)];
end

for i = -N02:N02
    h2 = [h2; 2*f_tilde*sinc(2*f_tilde*i)];
end

nexttile;
H1 = fftshift( fft(h1,Nf));
H2 = fftshift( fft(h2,Nf));
freq = linspace(-Fe/2,Fe/2,Nf);
semilogy(freq, abs(H1));
title("Porte ordre 11");
xlabel("Fréquence (Hz)");
ylabel("Signal (V)");

nexttile;
semilogy(freq, abs(H2));
title("Porte ordre 61");
xlabel("Fréquence (Hz)");
ylabel("Signal (V)");

% Exo 3.2

nexttile;
FX = fftshift(fft(x,Nf));
semilogy(freq,abs(FX));
hold on
semilogy(freq, abs(H1));
hold on
semilogy(freq, abs(H2));
title("Passe-bas");
xlabel("Fréquence (Hz)");
ylabel("Signal (V)");

% Exo 3.4 - Filtrage

nexttile;
Y = filter(h1,1,x);
plot(T, Y);
hold on
plot(T,x);

title("Filtrage");
xlabel("Temps (s)");
ylabel("Signal (V)");