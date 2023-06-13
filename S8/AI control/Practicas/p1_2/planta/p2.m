%% Prácticas del Tema 1: MODELADO, IDENTIFICACIÓN Y CONTROL

%% Práctica 2

% 2.1.B)
K = 15.1515; % ganancia
Tp = 16.8989; % constante de tiempo, 0.632K
To = 1.25; % tiempo de retardo

% 2.1.C)
% No se puede aplicar un modelo de primer orden porque en la 
% respuesta al escalon hay oscilaciones.

% 2.2.1) Sistema de 1° orden en lazo abierto
Kp_P = Tp/(K*To);
Kp_PI = 0.9*Tp/(K*To);
Kp_PID = 1.2*Tp/(K*To);
Ti_PI = 3.33*To;
Ti_PID = 2*To;
Td_PID = 0.5*To;

% Control P -> mas rapido y estable, mejora el transitorio
% Control PI -> elimina error estacionario, un poco mas inestable
% Control PID -> mejora la estabilidad, el error permanece eliminado

% El saturador permite cortar el senal con limites de amplitud

% 2.2.2) Sistema de 2° orden en lazo cerrado
Kc = 6.16; % ganancia crítica
Tu = 22.230/4; % tiempo crítico
Kp_P_2 = 0.5*Kc;
Kp_PI_2 = 0.45*Kc;
Kp_PID_2 = 0.6*Kc;
Ti_PI_2 = Tu/1.2;
Ti_PID_2 = Tu/2;
Td_2 = Tu/8;

% 2.2.3) Modificar de forma cualitativa los parámetros del PID
% si Kp aumenta -> menos estabilidad y mas sobreelongacion
% si Ti disminuye -> mucha inestabilidad
% si Td aumenta -> menor sobreelongacion, mas estabilidad


% 2.2.4) Entrada Variable

% 2.3) Con ruido gaussiano
% Cuando la accion derivativa aumenta (Td aumenta), hay una amplificacion
% del ruido, inestable

