/**
  * NettoyerErreurTest : Vérifier que nettoyer est toujours exécutée, même
  * si une méthode de test lève une exception...
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.2 $
  */

public class NettoyerErreurTest extends NettoyerTest {

	public void test() throws Throwable {
		super.test();
		throw new RuntimeException();
	}

}
