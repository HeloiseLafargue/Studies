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

	// Pour traiter getMethod on utilise la programmation défensive
	private Method recupererMethode (Class<?> classe, String nomMethod) {
		try {
			return classe.getMethod(nomMethod);
		} catch (NoSuchMethodException e) {
			return null;
		}
	}

	private void testerUneClasse(String nomClasse)
		throws ClassNotFoundException, InstantiationException,
						  IllegalAccessException
	{
		// Récupérer la classe
		Class<?> myClass = Class.forName(nomClasse);

		// Récupérer les méthodes "preparer" et "nettoyer"
		Method preparer = recupererMethode(myClass, "preparer");
		Method nettoyer = recupererMethode(myClass, "nettoyer");

		// Instancier l'objet qui sera le récepteur des tests
		Object objet = myClass.newInstance();

		// Exécuter les méthods de test
		nbTestsLances = 0;
		for (Method m : myClass.getMethods()) {
			if (m.getName().startsWith("test")) {
				nbTestsLances ++;
				try {
					if (preparer != null) {preparer.invoke(objet);}
					m.invoke(objet);
					if (nettoyer != null) {nettoyer.invoke(objet);}
				} catch (Exception e) {
					System.out.println("Erreur");
					nbErreurs ++;
					e.printStackTrace();
				} catch (Error e) {
					System.out.println("Echec");
					nbEchecs ++;}
			}
		}
	}	

	public static void main(String... args) {
		LanceurIndependant lanceur = new LanceurIndependant(args);
	}

}
