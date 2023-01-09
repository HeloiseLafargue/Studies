import java.lang.reflect.*;
import java.util.*;
import java.lang.annotation.*;

/** L'objectif est de faire un lanceur simple sans utiliser toutes les clases
  * de notre architecture JUnit.   Il permet juste de valider la compréhension
  * de l'introspection en Java.
  */
public class LanceurIndependantAnnotation {
	private int nbTestsLances;
	private int nbErreurs;
	private int nbEchecs;
	private int nbIgnores;
	private List<Throwable> erreurs = new ArrayList<>();

	public LanceurIndependantAnnotation(String... nomsClasses) {
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
				System.out.println(e);
			}
		}

		// Afficher les erreurs
		for (Throwable e : erreurs) {
			System.out.println();
			if (e != null) {
				e.printStackTrace();
			}
		}

		// Afficher un bilan
		System.out.println();
		System.out.printf("%d tests lancés dont %d échecs et %d erreurs, %d ignorés.\n",
				nbTestsLances, nbEchecs, nbErreurs, nbIgnores);
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


	public int getNbIgnores() {
		return this.nbIgnores;
	}



	private void executerUnTest(Object recepteur, Method preparer, Method nettoyer,
			Method tester) throws IllegalAccessException, InvocationTargetException
	{
		try {
			if (preparer != null) {
				preparer.invoke(recepteur);
			}
			tester.invoke(recepteur);
		} finally {
			if (nettoyer != null) {
				nettoyer.invoke(recepteur);
			}
		}
		// XXX : Si une exception se produit lors de l'exécution des méthodes
		// preparer(), tester() ou nettoyer() sont encaptsulée dans
		// InvocationTargetException.  On pourrait la récupérer dans e et faire
		// un throw e.getCause().  Mais il faurait alors dans la signature
		// mettre un throws Throwable.
	}


	private void executerUnTestAvecException(Object recepteur, Method preparer, Method nettoyer,
			Method tester, Class<?> expected) throws IllegalAccessException, InvocationTargetException
	{
			try {
				if (preparer != null) {
					preparer.invoke(recepteur);
				}
				String message = null;	// OK
				try {
					tester.invoke(recepteur);
					message = "No exception but " + expected.getName() + " expected";
				} catch (InvocationTargetException e) {
					if (e.getCause().getClass().equals(expected)) {
						// OK
					} else {
						message = "expected exception: " + expected.getName() + " != raised exception: " + e.getCause().getClass().getName();
					}
				}
				if (message != null) {
					throw new InvocationTargetException(new Echec(message));
				}
			} finally {
				if (nettoyer != null) {
					nettoyer.invoke(recepteur);
				}
			}
	}


	private void testerUneClasse(String nomClasse)
		throws ClassNotFoundException, InstantiationException,
						  IllegalAccessException
	{

		// Récupérer la classe
		Class<?> classe = Class.forName(nomClasse);

		// Récupérer les méthodes "preparer" et "nettoyer"
		Method preparer = null;
		Method nettoyer = null;

		// Instancier l'objet qui sera le récepteur des tests
		Object objet = classe.newInstance();
			// On sait qu'il doit y avoir un constucteur par défaut.
			// Sinon, il faudrait récupérer un constructeur et appliquer
			// newInstance dessus.

		// Exécuter les méthods de test
		ArrayList<Method> testMethods = new ArrayList<>();
		for (Method m : classe.getMethods()) {
			if (m.isAnnotationPresent(Avant.class)) {
				if (preparer != null) {
					// Should be verified at compilation (Annotation Processor)
					throw new RuntimeException("Une seule méthode @Avant possible"
							+ "\n" + " 1. " + preparer
							+ "\n" + " 2. " + m);
				} else {
					preparer = m;
				}
			}

			if (m.isAnnotationPresent(Apres.class)) {
				if (nettoyer != null) {
					// Should be verified at compilation (Annotation Processor)
					throw new RuntimeException("Une seule méthode @Apres possible"
							+ "\n" + " 1. " + nettoyer
							+ "\n" + " 2. " + m);
				} else {
					nettoyer = m;
				}
			}

			if (m.isAnnotationPresent(UnTest.class)) {
				testMethods.add(m);
			}

			// TODO : La même méthode ne devrait pas avoir deux annotations
			// @Avant, @Apres et @UnTest. Ceci devrait être vérifié à la
			// compilation.
		}

		for (Method m: testMethods) {
			UnTest a = m.getAnnotation(UnTest.class);
			assert a != null;
			if (m.getParameterTypes().length != 0) {
				// Remarque : Cette propriété pourrait/devrait être vérifiée à
				// la compilation.
				throw new RuntimeException("Une méthode de test ne doit pas avoir de paramètre : " + m);
			} else if (Modifier.isStatic(m.getModifiers())) {
				// Remarque : Cette propriété pourrait/devrait être vérifiée à
				// la compilation.
				throw new RuntimeException("Une méthode de test doit être une méthode d'instance : " + m);
			} else {
				boolean echec = true;	// échec ou erreur
				boolean erreur = false; /// erreur
				try {
					nbTestsLances++;
					if (a.enabled()) {
						if (a.expected() == UnTest.NoException.class) {
							executerUnTest(objet, preparer, nettoyer, m);
						} else {
							// System.out.println("Not implemented yet!");
							executerUnTestAvecException(objet, preparer, nettoyer, m, a.expected());
						}
					} else {
						System.out.print('-');
						this.nbIgnores++;
					}
					echec = false;
				} catch (InvocationTargetException e) {
					// L'exception qui s'est vraiment produite lors de l'appel
					// des méthodes preparer(), tester() ou nettoyer() est
					// associée à InvocationTargetException.  On la retrouve en
					// faisant getCause().
					if (e.getCause() instanceof Echec) {
						nbEchecs++;
					} else {
						nbErreurs++;
						erreur = true;
					}
					erreurs.add(e.getCause());
				} catch (Throwable e) {
					nbErreurs++;
					erreurs.add(e.getCause());
				} finally {
					if (echec) {
						if (erreur) {
							System.out.print('E');
						} else {
							System.out.print('F');
						}
					} else {
						System.out.print('.');
					}
				}
			}
		}
	}

	public static void main(String... args) {
		LanceurIndependantAnnotation lanceur = new LanceurIndependantAnnotation(args);
	}

}
