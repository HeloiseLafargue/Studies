public class Sentence_stub extends SharedObject implements Sentence_itf, java.io.Serializable {

	public Sentence_stub(int id, Object obj) {
		super(id, obj);		
}

	public void write(String arg0) {
		Sentence s = (Sentence) obj ;
		s.write(arg0);
	}

	public String read() {
		Sentence s = (Sentence) obj ;
		return s.read();
	}

}