
@doc doc"""
#### Objet
Cette fonction implémente l'algorithme de Newton pour résoudre un problème d'optimisation sans contraintes

#### Syntaxe
```julia
xmin,fmin,flag,nb_iters = Algorithme_de_Newton(f,gradf,hessf,x0,option)
```

#### Entrées :
   - f       : (Function) la fonction à minimiser
   - gradf   : (Function) le gradient de la fonction f
   - hessf   : (Function) la hessienne de la fonction f
   - x0      : (Array{Float,1}) première approximation de la solution cherchée
   - options : (Array{Float,1})
       - max_iter      : le nombre maximal d'iterations
       - Tol_abs       : la tolérence absolue
       - Tol_rel       : la tolérence relative
       - epsilon       : epsilon pour les tests de stagnation

#### Sorties:
   - xmin    : (Array{Float,1}) une approximation de la solution du problème  : ``\min_{x \in \mathbb{R}^{n}} f(x)``
   - fmin    : (Float) ``f(x_{min})``
   - flag    : (Integer) indique le critère sur lequel le programme s'est arrêté (en respectant cet ordre de priorité si plusieurs critères sont vérifiés)
      - 0    : CN1 
      - 1    : stagnation du xk
      - 2    : stagnation du f
      - 3    : nombre maximal d'itération dépassé
   - nb_iters : (Integer) le nombre d'itérations faites par le programme

#### Exemple d'appel
```@example
f(x)=100*(x[2]-x[1]^2)^2+(1-x[1])^2
gradf(x)=[-400*x[1]*(x[2]-x[1]^2)-2*(1-x[1]) ; 200*(x[2]-x[1]^2)]
hessf(x)=[-400*(x[2]-3*x[1]^2)+2  -400*x[1];-400*x[1]  200]
x0 = [1; 0]
options = []
xmin,fmin,flag,nb_iters = Algorithme_De_Newton(f,gradf,hessf,x0,options)
```
"""
function Algorithme_De_Newton(f::Function,gradf::Function,hessf::Function,x0,options)

    "# Si options == [] on prends les paramètres par défaut"
    if options == []
        max_iter = 100
        Tol_abs = sqrt(eps())
        Tol_rel = 1e-15
        epsilon = 1.e-2
    else
        max_iter = options[1]
        Tol_abs = options[2]
        Tol_rel = options[3]
        epsilon = options[4]
    end

    ######################### Initialisation des variables ################################

    k = 0
    xk = x0
    Arret = false
    norme_x0 = norm(x0)
    
    if (norm(gradf(x0)) <= max(Tol_rel*norme_x0, Tol_abs))
        flag = 0
        Arret = true
    elseif (k == max_iter)
        flag = 3
        Arret = true
    end
    
        
    ############################### Itération des variables #################################
    
    while !Arret
        dk = - hessf(xk)\gradf(xk)  # Calcul de la direction de Newton
        xk_old = xk
        xk = xk + dk  # Mise à jour de l'itéré
        k = k + 1
        
        norme_gradf = norm(gradf(xk))
        norme_dk = norm(dk)
        norme_xk = norm(xk_old)
        abs_df = abs(f(xk)-f(xk_old))
        abs_xk = abs(f(xk_old))
        
        ########### Critères d'arrêt

        ### CN1
        if (norme_gradf <= max(Tol_rel*norme_x0, Tol_abs))  
            Arret = true
            flag = 0
        ### Stagnation de xk
        elseif (norme_dk <= epsilon*max(Tol_rel*norme_xk, Tol_abs))  
            Arret = true
            flag = 1
        ### Stagnation de fk
        elseif (abs_df <= epsilon*max(Tol_rel*abs_xk,Tol_abs)) 
            Arret = true
            flag = 2
        ## Nombre maximal d'itération dépassé
        elseif (k == max_iter) 
            Arret = true
            flag = 3
        end
    end
    
    xmin = xk
    fmin = f(xk)
    nb_iters = k
    return xmin,fmin,flag,nb_iters
end
