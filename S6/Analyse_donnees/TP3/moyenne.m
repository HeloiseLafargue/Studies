% Fonction moyenne, qui calcule la couleur moyenne d'une image.

function [col_moy] = moyenne(image)
    
    R = single(image(:,:,1));
    V = single(image(:,:,2));
    B = single(image(:,:,3));
    denom = max(1, R + V + B);
    r = R ./ denom;
    v = V ./ denom;
    col_moy = [mean(mean(r)) mean(mean(v))];

end

