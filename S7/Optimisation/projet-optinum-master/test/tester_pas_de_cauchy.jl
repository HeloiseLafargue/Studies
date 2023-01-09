"""
Tester l'algorithme du Pas de Cauchy

# Entrées :
   * afficher : (Bool) affichage ou non des résultats de chaque test

"""
function tester_pas_de_cauchy(afficher::Bool,Pas_De_Cauchy::Function)
    
    tol = 1e-7
    max_iter = 100
    # Tolérance utilisé dans les tests
    tol_test = 1e-3

    @testset "Test Pas de Cauchy" begin
        # le cas de test 1
        grad = [0 ; 0]
        Hess = [7 0 ; 0 2]
        delta = 1
        s, e = Pas_De_Cauchy(grad,Hess,delta)
        @test e == 0
        @test s ≈ [0.0 ; 0.0] atol = tol_test

        # le cas de test 2 H definie positive
        grad = [6 ; 2]
        Hess = [7 0 ; 0 2]
        delta = 0.5       # sol = pas de Cauchy  
        s, e = Pas_De_Cauchy(grad,Hess,delta)
        @test e == -1
        @test s ≈ -delta*grad/norm(grad) atol = tol_test


        # le cas test 2 bis matrice avec 1 vp < 0 et 1 vp > 0
        grad = [1,2]
        Hess = [1 0 ; 0 -1]
        delta = 1.       # g^T H g < 0 première direction concave
        s, e = Pas_De_Cauchy(grad,Hess,delta)
        @test e == -1
        @test  s ≈ -delta*grad/norm(grad) atol = tol_test


        grad = [1,0]
        delta = 0.5       #  g^T H g > 0 sol pas de Cauchy
        s, e = Pas_De_Cauchy(grad,Hess,delta)
        @test e == -1
        @test  s ≈ -delta*grad/norm(grad) atol = tol_test

        # le cas de test 3 e == 1
        grad = [1, 2]
        Hess = [7 0 ; 0 2]
        delta = 1
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  isapprox(s, -1/3*grad, atol = tol_test, atol = tol_test)
        @test e == 1
        
        # le cas de test 4, a<0 et e=-1
        grad = [3, 1]
        Hess = [-2 0 ; 0 10]
        delta = 5
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test e == -1

    end     
end