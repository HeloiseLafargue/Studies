import java.lang.reflect.*;
import java.util.*;

/** L'objectif est de faire un lanceur simple sans utiliser toutes les clases
  * de notre architecture JUnit.   Il permet juste de valider la compréhension
  * de l'introspection en Java.
  */
public class LanceurIndependant {
	private int nbTestsLances;
	private int nbErreurs;
	private int nbEchecs;
	private List<Throwable> erreurs = new ArrayList<>();

	public LanceurIndependant(String... nomsClasses) {
	    System.out.println();

		// Lancer les tests pour chaque classe
		for (String nom : nomsClasses) {
			try {
				System.out.print(nom + " : ");
				this.testerUneClasse(nom);
				System.out.println();
			} catch (ClassNotFoundException e) {
				System.out.println(" Classe inconnue !");
			} catch (Exception e) {
				System.out.println(" Problème : " + e);
				e.printStackTrace();
			}
		}

		// Afficher les erreurs
		for (Throwable e : erreurs) {
			System.out.println();
			e.printStackTrace();
		}

		// Afficher un bilan
		System.out.println();
		System.out.printf("%d tests lancés dont %d échecs et %d erreurs.\n",
				nbTestsLances, nbEchecs, nbErreurs);
	}

	public int getNbTests() {
		return this.nbTestsLances;
	}

	public int getNbErreurs() {
		return this.nbErreurs;
	}

	public int getNbEchecs() {
		return this.nbEchecs;
	}

	private void testerUneClasse(String nomClasse)
		throws ClassNotFoundException, InstantiationException,
						  IllegalAccessException
	{
		// Récupérer la classe
		Class<?> myClass = Class.forName(nomClasse);

		// Récupérer les méthodes "preparer" et "nettoyer"
		Method preparer = null;
		Method nettoyer = null;

		// Instancier l'objet qui sera le récepteur des tests
		Object objet = myClass.newInstance();

		// Exécuter les méthods de test
		nbTestsLances = 0;
		nbEchecs = 0;
		nbErreurs = 0;

		Method [] methodes = myClass.getMethods();
		List<Method> methodesTest = new ArrayList<Method>();

		for (Method m : methodes) {
			// Collecter & filter
			if (m.isAnnotationPresent(Avant.class)) {
				if (preparer == null){
					preparer = m;
				} else {
					throw new RuntimeException("Too many prepaper!");
				}
			}
			if (m.isAnnotationPresent(Apres.class)) {
				if (nettoyer == null){
					nettoyer = m;
				} else {
					throw new RuntimeException("Too many nettoyer!");
				}
			}
			if (m.isAnnotationPresent(UnTest.class)) {
				if ( Modifier.isPublic(m.getModifiers()) && 
						! Modifier.isStatic(m.getModifiers()) &&
						  (m.getParameterTypes().length == 0)){
								
					methodesTest.add(m);
				}
			}
		} 

		// Exécuter les tests suivant le protocole JUnit 4
		 System.out.println(methodesTest);
		 for (Method test : methodesTest){
			if (preparer != null){
				try {
					preparer.invoke(objet);
				} catch (InvocationTargetException e) {
					System.out.println("Erreur dans preparer");
				}
			}

			if (nettoyer != null){
				try {
					nettoyer.invoke(objet);
				} catch (InvocationTargetException e) {
					System.out.println("Erreur dans nettoyer");
				}
			}
			
			try {
				nbTestsLances ++;
				test.invoke(objet);
			} catch (InvocationTargetException e) {
				if (e.getCause() instanceof Echec){
					nbEchecs ++;
				} else {
					
					erreurs.add(e.getCause());
					nbErreurs ++;
				}
			} catch (Throwable e) {
				nbErreurs++;
				erreurs.add(e.getCause());
			}
		 }
		
	}

	public static void main(String... args) {
		LanceurIndependant lanceur = new LanceurIndependant(args);
	}

}
