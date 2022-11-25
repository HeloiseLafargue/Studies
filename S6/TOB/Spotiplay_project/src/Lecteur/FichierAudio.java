package Lecteur;

public class FichierAudio implements Saveable {
	
	private static final long serialVersionUID = 1L;
	//Chemin au fichier.
	private String filepath;
	//Nom du morceau.
	private String name;
	//nom de/des artistes.
	private String artist;
	//Genre du morceau
	private String genre;
	//nom de l'album
	private String album;

	public FichierAudio(String filepath) {
		this.filepath = filepath;
		if(this.filepath.split("\\\\").length > 1) {
			this.name = this.filepath.split("\\\\")[1];//.split(".")[0];
		} else {
			this.name = this.filepath.split("//")[1];
		}
		//pour macOS et UNIX en général

		if(this.filepath.split("//").length > 1) {
			System.out.println(this.filepath.split("//")[1]);
			this.filepath = this.filepath.split("//")[1];//.split(".")[0];
		} else {
			this.filepath = this.filepath.split("//")[1];
		}
	}
	
	public FichierAudio() {

	}
	
	public FichierAudio(String filePath, String title, String artist, String album) {
		this.filepath = filePath;
		this.artist = artist;
		this.name = title;
		this.album = album;
	}
	
	public String getFilepath() {
		return this.filepath;
	}
	public void setFilepath(String filepath) {
		this.filepath = filepath;
	}
	public String getName() {
		return this.name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getArtist() {
		return this.artist;
	}
	public void setArtist(String artist) {
		this.artist = artist;
	}
	public String getGenre() {
		return this.genre;
	}
	public void setGenre(String genre) {
		this.genre = genre;
	}
	public String getAlbum() {
		return this.album;
	}
	public void setAlbum(String album) {
		this.album = album;
	}
    public String toString() {
    	return this.name;
    }
}
