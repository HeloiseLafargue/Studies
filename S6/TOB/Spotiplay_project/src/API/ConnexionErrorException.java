package API;

/**Exception telling when a critic Internet error occur
 * It might be due to your Internet connexion 
 * or because a token expired 
*/

public class ConnexionErrorException extends RuntimeException {
	
	public ConnexionErrorException(String message) {
		super(message);
	}
}