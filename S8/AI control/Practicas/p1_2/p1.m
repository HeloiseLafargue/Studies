%% Prácticas del Tema 1: MODELADO, IDENTIFICACIÓN Y CONTROL

%% Práctica 1

% Extract Spain data
% Source: https://ourworldindata.org/co2-emissions
datos_es = annualcoemissionsbyregion;
aGuardar = (annualcoemissionsbyregion.Entity == "Spain");
datos_es = datos_es(aGuardar,:);
x = datos_es.Year;
y = datos_es.AnnualCOEmissionszeroFilled;

% Plot Spain CO2 emissions from 1750 until 2021 (data1)
plot(x,y);
hold on;
title("Spain CO2 emission");
xlabel("Year");
ylabel("Annual CO2 Emissions tonnes");

% Approximation
% Plot Spain CO2 emissions from 1750 until 2017 (data2)
x_reg = x(1:end-5);
y_reg = y(1:end-5);
plot(x_reg,y_reg);

% Plot the approximations for 2018 to 2021
x_test = [2017 2018 2019 2020 2021];
y_lin_test = linear(x_test);
y_quad_test = quad(x_test);
y_cubic_test = cubic(x_test);

plot(x_test,y_lin_test, "r.", "MarkerSize", 4);
plot(x_test,y_quad_test, "b.", "MarkerSize", 4);
plot(x_test,y_cubic_test, "magenta.", "MarkerSize", 4);
legend("data1", "data2", "linear","quadratic","cubic", 'Location','northwest');
hold off;

% Approximations made with data2
% Linear Model
function y = linear(x)
    y =9.068e+05*x - 1.656e+09;
end

% Quadratic Model
function y = quad(x)
    y = 1.012e+04*x.^2 - 3.721e+07*x + 3.417e+10;
end

% Cubic Model
function y = cubic(x)
    y = 74.78*x.^3 - 4.123e+05*x.^2 + 7.574e+08*x - 4.636e+11;
end