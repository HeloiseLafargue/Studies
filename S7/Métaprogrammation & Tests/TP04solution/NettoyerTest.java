/**
  * NettoyerTest :
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.2 $
  */

public class NettoyerTest {

	public void preparer() throws Throwable {
		Compteurs.nbPreparer++;
		// System.out.println("Exécution de préparer !");
	}

	public void nettoyer() throws Throwable {
		Compteurs.nbNettoyer++;
		// System.out.println("Exécution de nettoyer !");
	}

	public void test() throws Throwable {
		Compteurs.nbTest++;
		// System.out.println("Exécution du test !");
	}

}

