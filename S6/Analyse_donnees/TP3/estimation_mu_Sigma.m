function [mu, sigma] = estimation_mu_Sigma(X)
    mu = mean(X)';
    Xc = X - mean(X);
    sigma = 1/length(X) * Xc' * Xc;
end

