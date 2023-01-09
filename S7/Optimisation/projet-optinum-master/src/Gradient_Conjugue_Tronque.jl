@doc doc"""
#### Objet
Cette fonction calcule une solution approchée du problème

```math
\min_{||s||< \Delta}  q(s) = s^{t} g + \frac{1}{2} s^{t}Hs
```

par l'algorithme du gradient conjugué tronqué

#### Syntaxe
```julia
s = Gradient_Conjugue_Tronque(g,H,option)
```

#### Entrées :   
   - g : (Array{Float,1}) un vecteur de ``\mathbb{R}^n``
   - H : (Array{Float,2}) une matrice symétrique de ``\mathbb{R}^{n\times n}``
   - options          : (Array{Float,1})
      - delta    : le rayon de la région de confiance
      - max_iter : le nombre maximal d'iterations
      - tol      : la tolérance pour la condition d'arrêt sur le gradient

#### Sorties:
   - s : (Array{Float,1}) le pas s qui approche la solution du problème : ``min_{||s||< \Delta} q(s)``

#### Exemple d'appel:
```julia
gradf(x)=[-400*x[1]*(x[2]-x[1]^2)-2*(1-x[1]) ; 200*(x[2]-x[1]^2)]
hessf(x)=[-400*(x[2]-3*x[1]^2)+2  -400*x[1];-400*x[1]  200]
xk = [1; 0]
options = []
s = Gradient_Conjugue_Tronque(gradf(xk),hessf(xk),options)
```
"""
function Gradient_Conjugue_Tronque(g,H,options)

    "# Si option est vide on initialise les 3 paramètres par défaut"
    if options == []
        delta = 2
        max_iter = 100
        tol = 1e-6
    else
        delta = options[1]
        max_iter = options[2]
        tol = options[3]
    end
    
   ######################### Initialisation des variables ################################
   n = length(g)
   j = 0
   g0 = g
   normg0 = norm(g0)
   s0 = zeros(n)
   p0 = -g
   
   sj = s0
   pj = p0
   gj = g0
   cond = max(normg0*tol, tol)
    
   function q(s)
        return g'*s + 0.5*s'*H*s
   end
   
   # fonction calculant la racine de ∥sj + σpj ∥ = ∆ pour laquelle q(sj + σpj ) est la plus petite
   function sigma_min(sj,pj,delta)  
        a = norm(pj)^2
        b = 2*sj'*pj
        c = norm(sj)^2 - delta^2
        disc = b^2 - 4*a*c
        if disc>0
            sigma1 = (-b - sqrt(disc))/ (2*a)
            sigma2 = (-b + sqrt(disc))/ (2*a)
            if q(sj + sigma1*pj) < q(sj + sigma2*pj)
                return sigma1
            else
                return sigma2
            end
        elseif disc == 0
            return (-b/(2*a))
        end
   end
   
  # fonction calculant la racine positive de ∥sj + σpj ∥ = ∆
  function sigma_pos(sj,pj,delta)
    a = norm(pj)^2
    b = 2*sj'*pj
    c = norm(sj)^2 - delta^2
    disc = b^2 - 4*a*c
    sigma1 = (-b - sqrt(disc))/ (2*a)
    sigma2 = (-b + sqrt(disc))/ (2*a)
    return max(sigma1, sigma2)
   end
    
   ############################### Itération des variables #################################
   while (j<2*n) && norm(gj) > cond
        
        kj = transpose(pj)*H*pj
        
        if kj <= 0
            sigma_j = sigma_min(sj,pj,delta)
            return sj + sigma_j*pj
        end
        aj = transpose(gj)*gj/kj
        
        if norm(sj + aj*pj) >= delta
            sigma_j = sigma_pos(sj,pj,delta)
            return sj + sigma_j*pj
        end
        
        sj = sj + aj*pj
        gj_old = gj
        gj = gj + aj*H*pj
        bj = transpose(gj)gj/ (transpose(gj_old)*gj_old)
        pj = -gj + bj*pj
        j = j + 1
        
   end
   return sj
end
