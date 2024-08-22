% TP Codages JPEG et MPEG-2 - 3SN-M - 2022

%--------------------------------------------------------------------------
% Fonction d'estimation du mouvement par "block-matching"
%--------------------------------------------------------------------------
% MVr = EstimationMouvement(Ic,Ir)
%
% sortie  : MVdr = matrice des vecteurs de deplacements relatifs
% 
% entrees : Ic = image courante
%           Ir = image de reference
%--------------------------------------------------------------------------

function MVr = EstimationMouvement(Ic,Ir)
    Taille = 16;
    [l,c]=size(Ic);
    MVr=zeros(l/Taille,c/Taille,2);
    
    for i=1:Taille:l
      for j=1:Taille:c
          Vm = CSAMacroBloc(Ic(i:i+Taille-1,j:j+Taille-1), [j,i], Ir);
          MVr((i-1)/Taille + 1,(j-1)/Taille + 1, 1) = Vm(2);
          MVr((i-1)/Taille + 1,(j-1)/Taille + 1, 2) = Vm(1);
      end
    end
end

%--------------------------------------------------------------------------
% Fonction de recherche par 'Cross Search Algorithm' :         
%   - Recherche pour un macro-bloc de l'image courante
%--------------------------------------------------------------------------
% Vm = CSAMacroBloc(MBc, Vp, Iref)
%
% sorties : Vm = vecteur de mouvement 
% 
% entrées : Mbc = macro-bloc dans l'image courante Ic
%           Vp = vecteur de prediction (point de depart du MacroBloc)
%           Iref = image de reference (qui sera conservee dans le GOP)
%--------------------------------------------------------------------------

function Vm = CSAMacroBloc(MB_IC, Vp, IRef)
    [size_y_Ir, size_x_Ir] = size(IRef);
    coordx = Vp(1):Vp(1) + 15;
    coordy = Vp(2):Vp(2) + 15;
    Vm = [0, 0];
    MB_IC_V = MB_IC(:);

    directions = ["centre", "haut", "bas", "droite", "gauche"];
    EQM = zeros(1, 5);

    for i = 1:5
        EQM(i) = EQMMacrocBlocVoisin(MB_IC_V, IRef, size_x_Ir, size_y_Ir, coordx, coordy, directions(i));
    end

    while true
        [~, minIdx] = min(EQM);

        if minIdx == 1
            break;
        end

        % Déplacement en fonction de la direction minimisant l'EQM
        move = directions(minIdx);
        switch move
            case "haut"
                Vm(2) = Vm(2) - 1;
                coordy = coordy - 1;
            case "bas"
                Vm(2) = Vm(2) + 1;
                coordy = coordy + 1;
            case "gauche"
                Vm(1) = Vm(1) - 1;
                coordx = coordx - 1;
            case "droite"
                Vm(1) = Vm(1) + 1;
                coordx = coordx + 1;
        end

        % Mettre à jour les EQM
        for i = 1:5
            EQM(i) = EQMMacrocBlocVoisin(MB_IC_V, IRef, size_x_Ir, size_y_Ir, coordx, coordy, directions(i));
        end
    end
end


%--------------------------------------------------------------------------
% Fonction de calcul de l'EQM avec differents voisins 
% dans l'image de reference
%--------------------------------------------------------------------------
% EQM = EQMMacrocBlocVoisin(MB_IC_V,IRef,size_x_Ref,size_y_Ref,coordx,coordy,voisin)
%
% sortie  : EQM = erreur quadratique moyenne entre macro-blocs
% 
% entrées : MB_IC_V = macro-bloc dans l'image courante (vectorise)
%           Ir = Image de reference
%           size_x_Ir = nombre de lignes de Ir (pour effets de bords)
%           size_y_Ir = nombre de colonnes de Ir (pour effets de bords)
%           coordx = les 16 coordonnees du bloc suivant x
%           coordy = les 16 coordonnees du bloc suivant y
%           voisin = choix du voisin pour decaler le macro-bloc dans Ir
%                    ('haut', 'gauche', 'centre', 'droite', bas', ...)
%--------------------------------------------------------------------------

function EQM = EQMMacrocBlocVoisin(MB_IC_V, Ir, size_x_Ir, size_y_Ir, coordx, coordy, voisin)
    switch voisin
        case 'centre'
            Ir_bloc = Ir(coordy, coordx);
        case 'haut'
            if coordy(1) > 1
                Ir_bloc = Ir(coordy - 1, coordx);
            else
                EQM = Inf;
                return;
            end
        case 'bas'
            if coordy(16) < size_y_Ir
                Ir_bloc = Ir(coordy + 1, coordx);
            else
                EQM = Inf;
                return;
            end
        case 'droite'
            if coordx(16) < size_x_Ir
                Ir_bloc = Ir(coordy, coordx + 1);
            else
                EQM = Inf;
                return;
            end
        case 'gauche'
            if coordx(1) > 1
                Ir_bloc = Ir(coordy, coordx - 1);
            else
                EQM = Inf;
                return;
            end
        otherwise
            EQM = Inf;
            return;
    end
    
    EQM = sum((Ir_bloc(:) - MB_IC_V).^2);
end




