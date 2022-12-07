/**
 * CreneauInvalideException indique qu'une date n'est pas valide.
 */
public class CreneauInvalideException extends RuntimeException {

	/** Initaliser une CreneauInvalideException avec le message précisé.
	  * @param message le message explicatif
	  */
	public CreneauInvalideException(String message) {
		super(message);
	}

}