% Neural Network for Robot
%   input - input data.
%   output - target data.

X = datos.efector(:,1)';
Y = datos.efector(:,2)';
Z = datos.efector(:,3)';
u = repmat( ultPos(1) , [1,530] );
v = repmat( ultPos(2) , [1,530] );
w = repmat( ultPos(3) , [1,530] );

x = [X;Y;Z;u;v;w]; % input = posiciones de la pinza
t = datos.angulo(:,3)'; % output = angulos de la base

% x = input;
% t = output;

% Choose a Training Function
% For a list of all training functions type: help nntrain
% 'trainlm' is usually fastest.
% 'trainbr' takes longer but may be better for challenging problems.
% 'trainscg' uses less memory. Suitable in low memory situations.
trainFcn = 'trainlm';  % Levenberg-Marquardt backpropagation.

% Create a Fitting Network
hiddenLayer1Size = 25;
hiddenLayer2Size = 16;
net_codo = fitnet([hiddenLayer1Size hiddenLayer2Size],trainFcn);

% Setup Division of Data for Training, Validation, Testing
net_codo.divideParam.trainRatio = 70/100;
net_codo.divideParam.valRatio = 15/100;
net_codo.divideParam.testRatio = 15/100;

% Train the Network
[net_codo,tr] = train(net_codo,x,t);

% Test the Network
y = net_codo(x);
e = gsubtract(t,y);
performance = perform(net_codo,t,y)

% View the Network
view(net_codo)

% Plots
% Uncomment these lines to enable various plots.
%figure, plotperform(tr)
%figure, plottrainstate(tr)
%figure, ploterrhist(e)
%figure, plotregression(t,y)
%figure, plotfit(net,x,t)

