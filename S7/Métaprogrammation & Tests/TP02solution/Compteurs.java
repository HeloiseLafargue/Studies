/**
  * Classe utilisée pour compter le nombre de fois où les méthodes de
  * TestElementaire sont appelées.
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.1 $
  */

public class Compteurs {

	// Les attributs suivants sont utilisés pour les tests
	static public int nbPreparer = 0; // nombre de fois où préparer est appelée
	static public int nbNettoyer = 0; // nombre de fois où nettoyer est appelée
	static public int nbTest = 0;	// nombre de fois où tester est appelée

	public static void reset() {
		nbTest = 0;
		nbNettoyer = 0;
		nbPreparer = 0;
	}

}

