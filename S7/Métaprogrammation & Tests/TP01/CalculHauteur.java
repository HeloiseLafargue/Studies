import java.lang.UnsupportedOperationException;

public class CalculHauteur implements VisiteurExpression <Integer>{

    public Integer visiterAccesVariable(AccesVariable v) {
        return 1;
    }

    public Integer visiterConstante(Constante c) {
        return 1;
    }

    public Integer visiterExpressionBinaire(ExpressionBinaire e) {
        return (1 + Math.max(e.getOperandeGauche().accepter(this),
        e.getOperandeDroite().accepter(this)));
    }

    public Integer visiterAddition(Addition a) {
        throw new UnsupportedOperationException("Addition non valable pour la hauteur");
    }

    public Integer visiterMultiplication(Multiplication m) {
        throw new UnsupportedOperationException("Multiplication non valable pour la hauteur");
    }

    public Integer visiterExpressionUnaire(ExpressionUnaire e) {
        return (1 + e.getOperande().accepter(this));
    }

    public Integer visiterNegation(Negation n) {
        throw new UnsupportedOperationException("Negation non valable pour la hauteur");
    }

    public Integer visiterSoustraction(Soustraction s) {
        throw new UnsupportedOperationException("Negation non valable pour la hauteur");
    }
}
