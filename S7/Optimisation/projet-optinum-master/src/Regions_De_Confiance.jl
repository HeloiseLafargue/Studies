@doc doc"""

#### Objet

Minimise une fonction de ``\mathbb{R}^{n}`` à valeurs dans ``\mathbb{R}`` en utilisant l'algorithme des régions de confiance. 

La solution approchées des sous-problèmes quadratiques est calculé 
par le pas de Cauchy ou le pas issu de l'algorithme du gradient conjugue tronqué

#### Syntaxe
```julia
xmin, fxmin, flag, nb_iters = Regions_De_Confiance(algo,f,gradf,hessf,x0,option)
```

#### Entrées :

   - algo        : (String) string indicant la méthode à utiliser pour calculer le pas
        - "gct"   : pour l'algorithme du gradient conjugué tronqué
        - "cauchy": pour le pas de Cauchy
   - f           : (Function) la fonction à minimiser
   - gradf       : (Function) le gradient de la fonction f
   - hessf       : (Function) la hessiene de la fonction à minimiser
   - x0          : (Array{Float,1}) point de départ
   - options     : (Array{Float,1})
     - deltaMax       : utile pour les m-à-j de la région de confiance
                      ``R_{k}=\left\{x_{k}+s ;\|s\| \leq \Delta_{k}\right\}``
     - gamma1, gamma2 : ``0 < \gamma_{1} < 1 < \gamma_{2}`` pour les m-à-j de ``R_{k}``
     - eta1, eta2     : ``0 < \eta_{1} < \eta_{2} < 1`` pour les m-à-j de ``R_{k}``
     - delta0         : le rayon de départ de la région de confiance
     - max_iter       : le nombre maximale d'iterations
     - Tol_abs        : la tolérence absolue
     - Tol_rel        : la tolérence relative
     - epsilon       : epsilon pour les tests de stagnation

#### Sorties:

   - xmin    : (Array{Float,1}) une approximation de la solution du problème : 
               ``\min_{x \in \mathbb{R}^{n}} f(x)``
   - fxmin   : (Float) ``f(x_{min})``
   - flag    : (Integer) un entier indiquant le critère sur lequel le programme s'est arrêté (en respectant cet ordre de priorité si plusieurs critères sont vérifiés)
      - 0    : CN1
      - 1    : stagnation du ``x``
      - 2    : stagnation du ``f``
      - 3    : nombre maximal d'itération dépassé
   - nb_iters : (Integer)le nombre d'iteration qu'à fait le programme

#### Exemple d'appel
```julia
algo="gct"
f(x)=100*(x[2]-x[1]^2)^2+(1-x[1])^2
gradf(x)=[-400*x[1]*(x[2]-x[1]^2)-2*(1-x[1]) ; 200*(x[2]-x[1]^2)]
hessf(x)=[-400*(x[2]-3*x[1]^2)+2  -400*x[1];-400*x[1]  200]
x0 = [1; 0]
options = []
xmin, fxmin, flag, nb_iters = Regions_De_Confiance(algo,f,gradf,hessf,x0,options)
```
"""
function Regions_De_Confiance(algo,f::Function,gradf::Function,hessf::Function,x0,options)

    if options == []
        deltaMax = 10
        gamma1 = 0.5
        gamma2 = 2.00
        eta1 = 0.25
        eta2 = 0.75
        delta0 = 2
        max_iter = 1000
        Tol_abs = sqrt(eps())
        Tol_rel = 1e-15
        epsilon = 1.e-2
    else
        deltaMax = options[1]
        gamma1 = options[2]
        gamma2 = options[3]
        eta1 = options[4]
        eta2 = options[5]
        delta0 = options[6]
        max_iter = options[7]
        Tol_abs = options[8]
        Tol_rel = options[9]
        epsilon = options[10]
    end
    
    ######################### Initialisation des variables ################################
    n = length(x0)
    k = 0
    xk = x0
    fk = f(x0)
    norme_grad_x0 = norm(gradf(x0))
    deltak = delta0
    Arret = false
    
    
    ### CN1    
    if (norme_grad_x0 <= max(Tol_rel*norme_grad_x0, Tol_abs))
        flag = 0
        Arret = true
    ### Nombre maximal d'itération dépassé
    elseif (k >= max_iter)
        flag = 3
        Arret = true
    end
    
        
    ### Fonction m
    function m(fk,gk,s,Hk)
            return (fk + transpose(gk)*s + 0.5*transpose(s)*Hk*s)
        end
            
    ############################### Itération des variables #################################
    
    while !Arret
        
        fk = f(xk)
        gradk = gradf(xk)
        hk = hessf(xk)
        xk_old = xk
        
        ########### Choix de l'algorithme pour calculer le pas
                
        if algo == "cauchy"
            sk,e = Pas_De_Cauchy(gradk,hk,deltak)
        elseif algo == "gct"
            sk = Gradient_Conjugue_Tronque(gradk,hk,[deltak;max_iter;Tol_rel])
        else 
            sk = zeros(n)
            println("Erreur lors du choix de l'algorithme pour calculer le pas")
        end
        
        pk = (fk - f(xk+sk)) / ( m(fk,gradk,zeros(n),hk) - m(fk,gradk,sk,hk) )

        
        ########### Mise à jour de l'itéré

        if pk >= eta1
                xk = xk + sk
        end
            
        ########### Augmentation de la région de confiance

        if pk >= eta2
                deltak = min(gamma2*deltak, deltaMax)
        elseif pk >= eta1
                deltak = deltak
        
        
        ########### Diminituion de la région de confiance
        
        else
            deltak = gamma1*deltak
        end
        
        k = k+1
    
    
        ########### Critères d'arrêt

        ### CN1, si xk a été modifié
        if (xk_old != xk) && (norm(gradf(xk)) <= max(Tol_rel*norme_grad_x0, Tol_abs))
            Arret = true
            flag = 0
        ### Stagnation de xk, si xk a été modifié
        elseif (xk_old != xk) && (norm(xk - xk_old) <= epsilon*max(Tol_rel*norm(xk_old), Tol_abs))
            Arret = true
            flag = 1
        ### Stagnation de f, si xk a été modifié
        elseif (xk_old != xk) && (abs(f(xk)-f(xk_old)) <= epsilon*max(Tol_rel*abs(f(xk_old)),Tol_abs))
            Arret = true
            flag = 2
        ### Nombre maximal d'itération dépassé
        elseif (k == max_iter)
            Arret = true
            flag = 3
        end
        
    end
    
    xmin = xk
    fxmin = f(xmin)
    nb_iters = k

    return xmin, fxmin, flag, nb_iters
end
