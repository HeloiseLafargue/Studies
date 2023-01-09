/**
  * NettoyerErreur2Test :
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.1 $
  */

public class NettoyerErreur2Test extends NettoyerTest {

	public void preparer() throws Throwable {
		super.preparer();
		throw new Throwable();
	}

}

