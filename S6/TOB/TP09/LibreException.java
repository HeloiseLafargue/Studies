public class LibreException extends Exception {

	/** Le creneau du rendez-vous. */
	private int creneau;

	/** Problème identifié. */
	private String probleme;

	/** Initialiser LibreException à partir créneau choisi
	 * et du problème identifié.  Par exemple, on peut avoir creneau
	 * qui vaut 10 et le problème "Pas de rendez-vous".
	 * @param creneau le créneau choisi
	 * @param probleme le problème identifié
	 */
	public LibreException(int creneau, String probleme) {
		super("Creneau invalide car " + probleme + " le jour : " + creneau);
		this.creneau = creneau;
		this.probleme = probleme;
	}

	/** Retourner le creneau.
	  * @return le creneau */
	public int getCreneau() {
		return this.creneau;
	}

	/** Indiquer le problème.
	  * @return le problème */
	public String getProbleme() {
		return this.probleme;
	}

}
