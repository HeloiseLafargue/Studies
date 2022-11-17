function L = laplacian(nu,dx1,dx2,N1,N2)
%
%  Cette fonction construit la matrice de l'opérateur Laplacien 2D anisotrope
%
%  Inputs
%  ------
%
%  nu : nu=[nu1;nu2], coefficients de diffusivité dans les dierctions x1 et x2. 
%
%  dx1 : pas d'espace dans la direction x1.
%
%  dx2 : pas d'espace dans la direction x2.
%
%  N1 : nombre de points de grille dans la direction x1.
%
%  N2 : nombre de points de grilles dans la direction x2.
%
%  Outputs:
%  -------
%
%  L      : Matrice de l'opérateur Laplacien (dimension N1N2 x N1N2)
%
% 

% Initialisation
L=sparse([]);

n = N1*N2;
dx2inv = nu(1)/dx1^2;
dy2inv  = nu(2)/dx2^2;

A = spdiag( [dyv ddxv dyv] , [-1 0 1] , N2 , N2);
D = spdiags(dxv, 0 , N2, N2);
J = spdiags([e e], [-1 1], N1, N1);
Ah = kron(speye(N1),A) + kron(J,0);

end    
