close all
clear all
%% Représentation temporelle

tiledlayout(3,2);

% Cas 1.1
x = [];
T = [];
N = 90;
Fe = 10000;
f0 = 1100;
A = 1;

for i = 1:N
    x = [x;A*cos(2*pi*f0*i/Fe)];
    T = [T;i/Fe];
end

nexttile;
plot(T,x);
title("Cas temporel Fe = 10 000 Hz");
xlabel("Temps (s)");
ylabel("Signal (V)");

% Cas 1.2
x = [];
T = [];
N = 90;
Fe = 1000;
f0 = 1100;
A = 1;

for i = 1:N
    x = [x; A*cos(2*pi*f0*i/Fe)];
    T = [T; i/Fe];
end

nexttile;
plot(T,x);
title("Cas temporel Fe = 1000 Hz");


%% Représentation fréquentielle

% Cas 2.1
FX = fft(x,N);
F = [];
N = 90;
Fe = 10000;

for i = 1:N
    F = [F; i*Fe/N];
end

nexttile;
semilogy(F, abs(FX));
title("FFT Fe = 10 000 Hz");

% Cas 2.2
FX = fft(x,N);
F = [];
N = 90;
Fe = 1000;

for i = 1:N
    F = [F; i*Fe/N];
end

nexttile;
semilogy(F, abs(FX));
title("FFT Fe = 1000 Hz");


% Cas 2.3
N = 256;
FX = fft(x, N);
F = [];
Fe = 1000;

for i = 1:N
    F = [F; i*Fe/N];
end

nexttile;
semilogy(F, abs(FX));
title("FFT zero padding Fe = 1000 Hz");

% Cas 2.4

nexttile;
pwelch(x, [], [], [], Fe, 'twosided');
title("DSP Welch Fe = 1000Hz")
