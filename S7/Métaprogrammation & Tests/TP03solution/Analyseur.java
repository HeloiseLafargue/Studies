import java.io.*;
import java.util.*;
import lombok.SneakyThrows;
import lombok.Cleanup;

/** Analyser des donnÃ©es d'un fichier, une donnÃ©e par ligne avec 4 informations
 * sÃ©parÃ©es par des blancs : x, y, ordre (ignorÃ©e), valeur.
 */
public class Analyseur {
	/** Conserve la somme des valeurs associÃ©es Ã  une position. */
	private Map<Position, Double> cumuls;

	/** Construire un analyseur vide. */
	public Analyseur() {
		cumuls = new HashMap<>();
	}

	/** Charger l'analyseur avec les donnÃ©es du fichier "donnees.java". */
	@SneakyThrows public void charger() {
		@Cleanup BufferedReader in = new BufferedReader(new FileReader("donnees.txt"));
		String ligne = null;
		while ((ligne = in.readLine()) != null) {
			String[] mots = ligne.split("\\s+");
			assert mots.length == 4;    // 4 mots sur chaque ligne
			int x = Integer.parseInt(mots[0]);              // l'abscisse
			int y = Integer.parseInt(mots[1]);              // l'ordonnÃ©e
			Position p = new Position(x, y);
			double valeur = Double.parseDouble(mots[3]);    // la valeur
			cumuls.put(p, valeur(p) + valeur);
		}
	}

	/** Obtenir la valeur associÃ©e Ã  une position. */
	public double valeur(Position position) {
		Double valeur = cumuls.get(position);
		return valeur == null ? 0.0 : valeur;
	}

	/** Obtenir toutes les donnÃ©es. */
	public Map<Position, Double> donnees() {
		return Collections.unmodifiableMap(this.cumuls);
	}

	/** Affichier les donnÃ©es. */
	public static void main(String[] args) {
		Analyseur a = new Analyseur();
		a.charger();
		System.out.println(a.donnees());
		System.out.println("Nombres de positions : " + a.donnees().size());
	}
}
