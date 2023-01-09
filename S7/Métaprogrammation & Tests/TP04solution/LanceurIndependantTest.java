/**
  * LanceurIndependantTest :
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.1 $
  */

public class LanceurIndependantTest {

	public void testerOK()
			throws ClassNotFoundException
	{
		Compteurs.reset();
		LanceurIndependant lanceur = new LanceurIndependant(NettoyerTest.class.getName());
		Assert.assertTrue(1 == lanceur.getNbTests());
		Assert.assertTrue(0 == lanceur.getNbErreurs());
		Assert.assertTrue(0 == lanceur.getNbEchecs());
		Assert.assertTrue(1 == Compteurs.nbPreparer);
		Assert.assertTrue(1 == Compteurs.nbTest);
		Assert.assertTrue(1 == Compteurs.nbNettoyer);
	}

	public void testerNettoyerExecuteSiErreurDansTester()
			throws ClassNotFoundException
	{
		Compteurs.reset();
		LanceurIndependant lanceur = new LanceurIndependant(NettoyerErreurTest.class.getName());
		Assert.assertTrue(1 == lanceur.getNbTests());
		Assert.assertTrue(1 == lanceur.getNbErreurs());
		Assert.assertTrue(0 == lanceur.getNbEchecs());
		Assert.assertTrue(1 == Compteurs.nbPreparer);
		Assert.assertTrue(1 == Compteurs.nbTest);
		Assert.assertTrue(1 == Compteurs.nbNettoyer);
	}

	public void testerNettoyerExecuteSiErreurDansPreparer()
			throws ClassNotFoundException
	{
		Compteurs.reset();
		LanceurIndependant lanceur = new LanceurIndependant(NettoyerErreur2Test.class.getName());
		Assert.assertTrue(1 == lanceur.getNbTests());
		Assert.assertTrue(1 == lanceur.getNbErreurs());
		Assert.assertTrue(0 == lanceur.getNbEchecs());
		Assert.assertTrue(1 == Compteurs.nbPreparer);
		Assert.assertTrue(0 == Compteurs.nbTest);
		Assert.assertTrue(1 == Compteurs.nbNettoyer);
	}

	public void testerMonnaieTest() throws ClassNotFoundException
	{
		LanceurIndependant lanceur = new LanceurIndependant(MonnaieTest.class.getName());
		Assert.assertTrue(2 == lanceur.getNbTests());
		Assert.assertTrue(0 == lanceur.getNbErreurs());
		Assert.assertTrue(0 == lanceur.getNbEchecs());
	}

	public void testerMonnaieTest2() throws ClassNotFoundException
	{
		LanceurIndependant lanceur = new LanceurIndependant(MonnaieTest2.class.getName());
		Assert.assertTrue(3 == lanceur.getNbTests());
		Assert.assertTrue(0 == lanceur.getNbErreurs());
		Assert.assertTrue(0 == lanceur.getNbEchecs());
	}

	public void testerMonnaieErreur() throws ClassNotFoundException
	{
		LanceurIndependant lanceur = new LanceurIndependant(MonnaieErreurTest.class.getName());
		Assert.assertTrue(4 == lanceur.getNbTests());
		Assert.assertTrue(1 == lanceur.getNbErreurs());
		Assert.assertTrue(1 == lanceur.getNbEchecs());
	}

	public void testerMonnaieErreur2() throws ClassNotFoundException
	{
		LanceurIndependant lanceur = new LanceurIndependant(MonnaieErreur2Test.class.getName());
		Assert.assertTrue(2 == lanceur.getNbTests());
		Assert.assertTrue(0 == lanceur.getNbErreurs());
		Assert.assertTrue(1 == lanceur.getNbEchecs());
	}

	public void testerCasLimitesTest() throws ClassNotFoundException
	{
		LanceurIndependant lanceur = new LanceurIndependant(CasLimitesTest.class.getName());
		Assert.assertTrue(1 == lanceur.getNbTests());
		Assert.assertTrue(0 == lanceur.getNbErreurs());
		Assert.assertTrue(0 == lanceur.getNbEchecs());
	}

}

