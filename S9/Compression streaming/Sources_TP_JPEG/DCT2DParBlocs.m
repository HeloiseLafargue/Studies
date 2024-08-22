
% TP Codages JPEG et MPEG-2 - 3SN-M - 2022
                                               
%--------------------------------------------------------------------------
% Fonction de transformee (directe et inverse) en cosinus discrete par blocs
%--------------------------------------------------------------------------
% I_DCT = DCT2DParBlocs(sens,I,methode,taille_bloc)
%
% sortie  : I_DCT = image de la DCT ou IDCT par blocs
% 
% entrees : sens = sens pour la DCT : 'Direct' ou 'Inverse'
%           I = image avant DCT ou IDCT par blocs
%           methode = methode de calcul de la DCT : 'Matlab' ou 'Rapide'
%           taille_bloc = taille des blocs pour la DCT (ici 8x8)
%--------------------------------------------------------------------------

function I_DCT = DCT2DParBlocs(sens,I,methode,taille_bloc)
    
    [n,p] = size(I);
    I_DCT = zeros(n,p);

    if strcmp(sens, 'Direct')
        for i = 1:n/taille_bloc
                for j = 1:p/taille_bloc
                    bloc = I((i-1)*taille_bloc+1 : i*taille_bloc, (j-1)*taille_bloc+1 : j*taille_bloc);
                    if strcmp(methode, 'Matlab')
                        I_DCT((i-1)*taille_bloc+1 : i*taille_bloc, (j-1)*taille_bloc+1 : j*taille_bloc) = dct2(bloc, [taille_bloc taille_bloc]);
                    elseif strcmp(methode, 'Rapide')
                        I_DCT((i-1)*taille_bloc+1 : i*taille_bloc, (j-1)*taille_bloc+1 : j*taille_bloc) = DCT2Rapide(bloc,taille_bloc);
                    end
                end
        end
    end
    if strcmp(sens, 'Inverse')
        for i = 1:n/taille_bloc
                for j = 1:p/taille_bloc
                    bloc = I((i-1)*taille_bloc+1 : i*taille_bloc, (j-1)*taille_bloc+1 : j*taille_bloc);
                    if strcmp(methode, 'Matlab')
                        I_DCT((i-1)*taille_bloc+1 : i*taille_bloc, (j-1)*taille_bloc+1 : j*taille_bloc) = idct2(bloc, [taille_bloc taille_bloc]);
                    elseif strcmp(methode, 'Rapide')
                        I_DCT((i-1)*taille_bloc+1 : i*taille_bloc, (j-1)*taille_bloc+1 : j*taille_bloc) = IDCT2Rapide(bloc, taille_bloc);
                    end
                end
        end
    end
   
end
  
%--------------------------------------------------------------------------
% Fonction de calcul de transformee en cosinus discrete rapide 
% pour un bloc de taille 8x8
%--------------------------------------------------------------------------
% Bloc_DCT2 = DCT2Rapide(Bloc_Origine, taille_bloc)
%
% sortie  : Bloc_DCT2 = DCT du bloc
% 
% entrees : Bloc_Origine = Bloc d'origine
%           taille_bloc = taille des blocs pour la DCT (ici 8x8)
%--------------------------------------------------------------------------
function Bloc_DCT2 = DCT2Rapide(Bloc_Origine,taille_bloc)
    
    [n,p] = size(Bloc_Origine);
    Bloc_DCT2 = zeros(n,p);

    C = ones(taille_bloc, 1);
    C(1) = 1 / sqrt(2);
   
    for i = 1:taille_bloc: n
        for j = 1:taille_bloc: p
            for u = 0:taille_bloc-1
                for v = 0:taille_bloc-1
                    somme = 0;
                    for x = 0:taille_bloc-1
                        for y = 0:taille_bloc-1
                            somme = somme + Bloc_Origine(i + x, j + y) * ...
                                cos((2 * x + 1) * u * pi / (2 * taille_bloc)) * ...
                                cos((2 * y + 1) * v * pi / (2 * taille_bloc));
                        end
                    end
                    somme = somme * (C(u+1) * C(v+1) / 4);
                    Bloc_DCT2(i + u, j + v) = somme;
                end
            end
        end
    end
end


%--------------------------------------------------------------------------
% Fonction de calcul de transformee en cosinus discrete inverse rapide
% pour un bloc de taille 8x8
%--------------------------------------------------------------------------
% Bloc_IDCT2 = IDCT2Rapide(Bloc_DCT2,taille_bloc)
%
% sortie  : Bloc_IDCT2 = Bloc reconstruit par DCT inverse
% 
% entrees : Bloc_DCT2 = DCT du bloc 
%           taille_bloc = taille des blocs pour la DCT (ici 8x8)
%--------------------------------------------------------------------------

function Bloc_IDCT2 = IDCT2Rapide(Bloc_DCT2, taille_bloc)
    [n, p] = size(Bloc_DCT2);
    Bloc_IDCT2 = zeros(n, p);

    C = ones(taille_bloc, 1);
    C(1) = 1 / sqrt(2);

    for i = 1:taille_bloc: n
        for j = 1:taille_bloc: p
            for u = 0:taille_bloc-1
                for v = 0:taille_bloc-1
                    somme = 0;
                    for x = 0:taille_bloc-1
                        for y = 0:taille_bloc-1
                            somme = somme +  ...
                                Bloc_DCT2(x + i, y + j) * ...
                                cos((2 * x + 1) * u * pi / (2 * taille_bloc)) * ...
                                cos((2 * y + 1) * v * pi / (2 * taille_bloc));
                        end
                    end
                    somme = somme * (C(u+1) * C(v+1) / 4);
                    Bloc_IDCT2(i + u, j + v) = somme;
                end
            end
        end
    end
end
