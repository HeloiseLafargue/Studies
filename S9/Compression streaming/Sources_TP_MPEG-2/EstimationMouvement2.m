%--------------------------------------------------------------------------
% Version 2 avec 8 voisins
%--------------------------------------------------------------------------

function MVr = EstimationMouvement2(Ic,Ir)
    Taille = 16;
    [l,c]=size(Ic);
    MVr=zeros(l/Taille,c/Taille,2);
    
    for i=1:Taille:l
      for j=1:Taille:c
          Vm = CSAMacroBloc2(Ic(i:i+Taille-1,j:j+Taille-1), [j,i], Ir);
          MVr((i-1)/Taille + 1,(j-1)/Taille + 1, 1) = Vm(2);
          MVr((i-1)/Taille + 1,(j-1)/Taille + 1, 2) = Vm(1);
      end
    end
end

%--------------------------------------------------------------------------

function Vm = CSAMacroBloc2(MB_IC, Vp, IRef)
    [size_y_Ir, size_x_Ir] = size(IRef);
    coordx = Vp(1):Vp(1) + 15;
    coordy = Vp(2):Vp(2) + 15;
    Vm = [0, 0];
    MB_IC_V = MB_IC(:);

    directions = ["centre", "haut", "bas", "droite", "gauche", "nord_est", "nord_ouest", "sud_est", "sud_ouest"];
    EQM = zeros(1, 9);

    for i = 1:9
        EQM(i) = EQMMacrocBlocVoisin2(MB_IC_V, IRef, size_x_Ir, size_y_Ir, coordx, coordy, directions(i));
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
            case "nord_est"
                Vm = Vm + [-1, -1];
                coordx = coordx + 1;
                coordy = coordy - 1;
            case "nord_ouest"
                Vm = Vm + [-1, 1];
                coordx = coordx - 1;
                coordy = coordy - 1;
            case "sud_est"
                Vm = Vm + [1, -1];
                coordx = coordx + 1;
                coordy = coordy + 1;
            case "sud_ouest"
                Vm = Vm + [1, 1];
                coordx = coordx - 1;
                coordy = coordy + 1;
        end

        % Mettre à jour les EQM
        for i = 1:9
            EQM(i) = EQMMacrocBlocVoisin2(MB_IC_V, IRef, size_x_Ir, size_y_Ir, coordx, coordy, directions(i));
        end
    end
end


%--------------------------------------------------------------------------

function EQM = EQMMacrocBlocVoisin2(MB_IC_V, Ir, size_x_Ir, size_y_Ir, coordx, coordy, voisin)
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
        case 'nord_est'
            if coordy(1) > 1 && coordx(16) < size_x_Ir
                Ir_bloc = Ir(coordy - 1, coordx + 1);
            else
                EQM = Inf;
                return;
            end
        case 'nord_ouest'
            if coordy(1) > 1 && coordx(1) > 1
                Ir_bloc = Ir(coordy - 1, coordx - 1);
            else
                EQM = Inf;
                return;
            end
        case 'sud_est'
            if coordy(16) < size_y_Ir && coordx(16) < size_x_Ir
                Ir_bloc = Ir(coordy + 1, coordx + 1);
            else
                EQM = Inf;
                return;
            end
        case 'sud_ouest'
            if coordy(16) < size_y_Ir && coordx(1) > 1
                Ir_bloc = Ir(coordy + 1, coordx - 1);
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