%% APARTADO 1

% https://es.mathworks.com/help/deeplearning/ref/neuralnetfitting-app.html
% https://es.mathworks.com/help/deeplearning/gs/fit-data-with-a-neural-network.html

% "Import" the input and output from workspace
input = [0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1;0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1];
output = [0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0];

% create the network (modify layers size)
% "Train" with the algorithm that you want and see the training results in 
% the training view and model summary view

% generate code with "Generate simple training script" to have the script
% of the trained network

% "Export Model to Workspace" to have your trained network in the workspace


% Acces to the network named "network1" info: network1.Network
% Acces to the weights and bias values : network1.Network.LW,
% network1.Network.b
% Simulate with an input sim(network1.Network, [1;0]))
% Get the int value : round(...)

input2 = [0 1 0 1; 0 1 1 0];
output2 = [0 0 1 0] ;
