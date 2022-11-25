package API;
/**Exception telling when a critic file error occur
*/

public class FileErrorException extends RuntimeException {
	
	public FileErrorException(String message) {
		super(message);
	}
}