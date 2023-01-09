/**
  * AnnotationExempleTest
  *
  * @author	Xavier CrÃ©gut <Prenom.Nom@enseeiht.fr>
  */

public class AnnotationExempleTest {

	int x;
	static final int version = 0;

	@Avant
	public void setUp() {
		x = 10;
	}

	@Apres
	public void tearDown() {
	}

	@UnTest
	public void premierTest() {
		// OK
	}


	@UnTest(enabled = version > 2)
	public void deuxiemeTest() {
		// OK
	}

	@UnTest
	public void preparerExecutee() {
		Assert.assertTrue(x == 10);
	}


	@UnTest(enabled = true)
	public void avecEchec() {
		Assert.assertTrue(false);
	}


	@UnTest
	public void avecErreur() {
		"xxx".charAt(10);
	}


	@UnTest(expected=NumberFormatException.class)
	public void avecExceptionNonLevee() {
	}


	@UnTest(expected=IndexOutOfBoundsException.class)
	public void avecMauvaiseException() {
		Integer.parseInt("xxx");
	}




}
