% SISTEMA ORDEN 1
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