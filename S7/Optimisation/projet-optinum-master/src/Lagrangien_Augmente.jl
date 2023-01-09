@doc doc"""
#### Objet

Résolution des problèmes de minimisation avec une contrainte d'égalité scalaire par l'algorithme du lagrangien augmenté.

#### Syntaxe
```julia
xmin,fxmin,flag,iter,muks,lambdaks = Lagrangien_Augmente(algo,f,gradf,hessf,c,gradc,hessc,x0,options)
```

#### Entrées
  - algo : (String) l'algorithme sans contraintes à utiliser:
    - "newton"  : pour l'algorithme de Newton
    - "cauchy"  : pour le pas de Cauchy
    - "gct"     : pour le gradient conjugué tronqué
  - f : (Function) la fonction à minimiser
  - gradf       : (Function) le gradient de la fonction
  - hessf       : (Function) la hessienne de la fonction
  - c     : (Function) la contrainte [x est dans le domaine des contraintes ssi ``c(x)=0``]
  - gradc : (Function) le gradient de la contrainte
  - hessc : (Function) la hessienne de la contrainte
  - x0 : (Array{Float,1}) la première composante du point de départ du Lagrangien
  - options : (Array{Float,1})
    1. epsilon     : utilisé dans les critères d'arrêt
    2. tol         : la tolérance utilisée dans les critères d'arrêt
    3. itermax     : nombre maximal d'itération dans la boucle principale
    4. lambda0     : la deuxième composante du point de départ du Lagrangien
    5. mu0, tho    : valeurs initiales des variables de l'algorithme

#### Sorties
- xmin : (Array{Float,1}) une approximation de la solution du problème avec contraintes
- fxmin : (Float) ``f(x_{min})``
- flag : (Integer) indicateur du déroulement de l'algorithme
   - 0    : convergence
   - 1    : nombre maximal d'itération atteint
   - (-1) : une erreur s'est produite
- niters : (Integer) nombre d'itérations réalisées
- muks : (Array{Float64,1}) tableau des valeurs prises par mu_k au cours de l'exécution
- lambdaks : (Array{Float64,1}) tableau des valeurs prises par lambda_k au cours de l'exécution

#### Exemple d'appel
```julia
using LinearAlgebra
algo = "gct" # ou newton|gct
f(x)=100*(x[2]-x[1]^2)^2+(1-x[1])^2
gradf(x)=[-400*x[1]*(x[2]-x[1]^2)-2*(1-x[1]) ; 200*(x[2]-x[1]^2)]
hessf(x)=[-400*(x[2]-3*x[1]^2)+2  -400*x[1];-400*x[1]  200]
c(x) =  (x[1]^2) + (x[2]^2) -1.5
gradc(x) = [2*x[1] ;2*x[2]]
hessc(x) = [2 0;0 2]
x0 = [1; 0]
options = []
xmin,fxmin,flag,iter,muks,lambdaks = Lagrangien_Augmente(algo,f,gradf,hessf,c,gradc,hessc,x0,options)
```

#### Tolérances des algorithmes appelés

Pour les tolérances définies dans les algorithmes appelés (Newton et régions de confiance), prendre les tolérances par défaut définies dans ces algorithmes.

"""
function Lagrangien_Augmente(algo,fonc::Function,contrainte::Function,gradfonc::Function,
        hessfonc::Function,grad_contrainte::Function,hess_contrainte::Function,x0,options)

    if options == []
		epsilon = 1e-2
		tol = 1e-5
		itermax = 1000
		lambda0 = 2
		mu0 = 100
		tho = 2
	else
		epsilon = options[1]
		tol = options[2]
		itermax = options[3]
		lambda0 = options[4]
		mu0 = options[5]
		tho = options[6]
	end

  ######################### Initialisation des variables ################################
  n = length(x0)
  xmin = zeros(n)
  fxmin = 0
  flag = 0
  iter = 0
  muk = mu0
  muks = [mu0]
  lambdak = lambda0
  lambdaks = [lambda0]
    
  k = 0
  xk = x0
  beta = 0.9
  eta = 0.1258925
  alpha = 0.1
  epsilon0 = 1/mu0
  epsilonk = epsilon0
  eta0 = eta/(mu0^alpha)
  etak = eta0
  Tol_rel = 1e-15
  Tol_abs = epsilon0
  
  c0 = contrainte(x0)
  gradc0 = grad_contrainte(x0)
  norme_gradLA0 = norm(gradfonc(x0) + lambda0'*gradc0)
  Arret = false
  
  ### CN1 sur le lagrangien non augmenté et la contrainte c
  if (norme_gradLA0 <= max(tol*norme_gradLA0, tol) && (norm(c0) <= max(tol*norm(c0), tol)))
      flag = 0
      Arret = true
  ### Nombre maximal d'itération dépassé
  elseif (k >= itermax)
      flag = 1
      Arret = true
  end
    
  ############################### Itération des variables #################################
    while !Arret
        xk_old = xk
        Tol_abs = epsilonk

        function LAk(xk)
            return fonc(xk) + lambdak' * contrainte(xk) + (muk/2) * norm(contrainte(xk))^2
        end

        function gradLAk(xk)
            return (gradfonc(xk) + lambdak' * grad_contrainte(xk) + muk * (contrainte(xk)') * grad_contrainte(xk))
        end     

        function hLAk(xk)
            return (hessfonc(xk) + lambdak' * hess_contrainte(xk) + muk * (contrainte(xk) * hess_contrainte(xk) + grad_contrainte(xk) * (grad_contrainte(xk)') ))
        end

        ########### Choix de l'algo
        if algo == "newton" 
            options_newton = [itermax, Tol_abs, Tol_rel, epsilon]
            xk,f_min,flag_newton,nb_iters = Algorithme_De_Newton(LAk,gradLAk,hLAk,xk,options_newton)
            ### Gestion des flag pour Newton
            if flag_newton == 3 # itermax atteint
                flag = 1
            elseif flag_newton == 1 || flag == 2 # une erreur s'est produite
                flag = (-1)
            end

        elseif algo == "cauchy" || algo == "gct"
            options_RC = [10, 0.5, 2.0, 0.25, 0.75, 2, itermax, Tol_abs, Tol_rel, epsilon]
            xk,f_min,flag_rc,nb_iters = Regions_De_Confiance(algo,LAk,gradLAk,hLAk,xk,options_RC)
            ### Gestion des flag pour Région de confiance
            if flag_rc == 3 # itermax atteint
                flag = 1
            elseif flag_rc == 1 || flag == 2 # une erreur s'est produite
                flag = (-1)
            end
        else 
            xk = zeros(n)
            println("Erreur lors du choix de l'algorithme")
        end

        ########### Critères d'arrêt
        normGradLAk = norm((gradfonc(xk) + lambdak'*grad_contrainte(xk)))

        ### CN1 sur le lagrangien non augmenté et la contrainte c, si xk a été modifié
        if (xk_old != xk) && (normGradLAk <= max(tol*norme_gradLA0, tol)) && (norm(contrainte(xk)) <= max(tol*norm(c0), tol))
          Arret = true
          flag = 0
        end  

        ########## Mise à jour des mulplicateurs ou paramètre de pénalité
        ck = contrainte(xk)
        if norm(ck) <= etak
         lambdak = lambdak + muk*ck
         push!(lambdaks,lambdak)
         #muk = muk
         epsilonk = epsilonk / muk
         etak = etak / (muk^beta)
        else  
         #lambdak = lambdak
         muk = tho * muk
         push!(muks,muk)
         epsilonk = epsilon0 / muk
         etak = eta / (muk^alpha)
        end


        ### Nombre maximal d'itération dépassé
        if (k == itermax)
            Arret = true
            flag = 1
        end
        k = k+1 

    end
  
    xmin = xk
    fmin = fonc(xmin)
    iter = k
    return xmin,fxmin,flag,iter, muks, lambdaks
end
