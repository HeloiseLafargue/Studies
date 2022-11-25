package Lecteur;

import java.io.*;

public class FileHandler {

	final static String PLAYLISTPATH = "playlist//";
	final static String AUDIOPATH = "audioInformations//";
	final static String PARAMETERSPATH = "parameters//";
	final static String FICHIERSWAVPATH = "TestAudio//";

	/**
	 * Vérifie l'intégrité des dossiers/fichiers necessaires au fonctionnement de
	 * l'application Reconstruit le repertoire dans le cas échéant
	 */

	private static void VerifyIntegrity() {
		File file = new File(PLAYLISTPATH);
		if (!file.isDirectory()) {
			System.out.println("Reconstruction de la base de donnée \"playlist\"");
			file.mkdir();
		}
		file = new File(PARAMETERSPATH);
		if (!file.isDirectory()) {
			System.out.println("Reconstruction de la base de donnée \"parameters\"");
			file.mkdir();
		}
		file = new File(AUDIOPATH);
		if (!file.isDirectory()) {
			System.out.println("Reconstruction de la base de donnée \"Informations Fichiers audios\"");
			file.mkdir();
		}
	}

	/**
	 * Enregistrer un élèment sauvegardable
	 * 
	 * @return
	 */
	static void Save(Saveable save) {

		VerifyIntegrity();
		String name = save.toString();
		//System.out.println("sauvegarde de " + name);
		File file;

		if (save.getClass() == PlaylistManuelle.class) {
			file = new File(PLAYLISTPATH + name + ".playlist");
		} else if (save.getClass() == FichierAudio.class) {
			file = new File(AUDIOPATH + name + ".audio");
		} else {
			file = new File("test" + name + ".fichiertest");
		}

		try {
			FileOutputStream fileOStream = new FileOutputStream(file);
			ObjectOutputStream objectOStream = new ObjectOutputStream(fileOStream);

			// Write objects to file

			if (save.getClass() == PlaylistManuelle.class) {
				objectOStream.writeObject((PlaylistManuelle) save);
			} else if (save.getClass() == FichierAudio.class) {
				objectOStream.writeObject((FichierAudio) save);
			} else {
				System.out.println("erreur lors de la sauvegarde");
			}
			
			objectOStream.close();
			fileOStream.close();

		} catch (FileNotFoundException e) {
			System.out.println("File not found");
		} catch (IOException e) {
			System.out.println("Error initializing stream");
		}

	}

	private static void Read(File file, Saveable object) {
		try {
			String type = file.toString().split("\\\\")[0];
			//System.out.println("extraction de " + file.toString());
			FileInputStream fileIStream = new FileInputStream(file);
			ObjectInputStream objectIStream = new ObjectInputStream(fileIStream);

			// Read objects
			if(type.contentEquals("playlist")) {
				object = (PlaylistManuelle) objectIStream.readObject();
			} else if(type.contentEquals("audioInformations")) {
				object = (FichierAudio) objectIStream.readObject();
				//System.out.println("fichier audio � : " + ((FichierAudio) object).getFilepath());
				//((FichierAudio) object).setFilepath(((FichierAudio) object).getFilepath());
			}
			
			objectIStream.close();
			fileIStream.close();

		} catch (FileNotFoundException e) {
			System.out.println("File not found");
		} catch (IOException e) {
			System.out.println("Error initializing stream");
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Charger une playlist.
	 * 
	 * @param fileName nom de la playlist
	 */
	public static PlaylistManuelle ReadPlaylist(String fileName) {

		File file = new File(PLAYLISTPATH + fileName);// + ".playlist");
		PlaylistManuelle playlist = new PlaylistManuelle();
		
		try {
			String type = file.toString().split("\\\\")[0];
			//System.out.println("extraction de " + file.toString());
			FileInputStream fileIStream = new FileInputStream(file);
			ObjectInputStream objectIStream = new ObjectInputStream(fileIStream);

			// Read playlist
		    playlist = (PlaylistManuelle) objectIStream.readObject();
			
			objectIStream.close();
			fileIStream.close();

		} catch (FileNotFoundException e) {
			System.out.println("File not found");
		} catch (IOException e) {
			System.out.println("Error initializing stream");
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}

		return playlist;
	}

	/**
	 * Charger les informations d'un fichier audio.
	 * 
	 * @param fileName nom du fichier audio
	 */
	static FichierAudio ReadAudio(String fileName) {

		File file = new File(fileName);//AUDIOPATH + fileName + ".audio");
		FichierAudio audio = new FichierAudio();//AUDIOPATH + fileName + ".audio");

		try {
			String type = file.toString().split("\\\\")[0];
			//System.out.println("extraction de " + file.toString());
			FileInputStream fileIStream = new FileInputStream(file);
			ObjectInputStream objectIStream = new ObjectInputStream(fileIStream);

			// Read Audio
			audio = (FichierAudio) objectIStream.readObject();
			//System.out.println("fichier audio � : " + ((FichierAudio) audio).getFilepath());
			//((FichierAudio) object).setFilepath(((FichierAudio) object).getFilepath());
			
			objectIStream.close();
			fileIStream.close();

		} catch (FileNotFoundException e) {
			System.out.println("File not found");
		} catch (IOException e) {
			System.out.println("Error initializing stream");
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}

		return audio;
	}

	/**
	 * Savoir si le fichier est existant
	 * 
	 * @param path nom du fichier de sauvegarde
	 */
	static boolean FileExist(String path) {
		
		//System.out.println("verification de l'existance de " + path);
		File file = new File(path);
		
		if (file.exists()) {
			return true;
		}
		return false;
	}

	public static String[] ListPlaylists() {
    	File f = new File(PLAYLISTPATH);
        File[] listeToutePlaylists = f.listFiles();
        String[] listePlaylists = new String[listeToutePlaylists.length];
        for(int i=0; i < listeToutePlaylists.length; i++) {
        	listePlaylists[i] = listeToutePlaylists[i].toString();
        }
        return listePlaylists;
	}

	public static FichierAudio[] ListFichiersAudios() {
		VerifyIntegrity();
    	File f = new File(AUDIOPATH);
        File[] listeTousFichiersAudios = f.listFiles();
        FichierAudio[] listeAudios = new FichierAudio[listeTousFichiersAudios.length];
        for(int i=0; i < listeAudios.length; i++) {
        	listeAudios[i] = FileHandler.ReadAudio(listeTousFichiersAudios[i].toString());
        }
        return listeAudios;
	}

	public static FichierAudio getAudio(String nom) {
		String path = FICHIERSWAVPATH + nom;
		//System.out.println(path);
		FichierAudio audio = new FichierAudio(path);
        
		if(FileHandler.FileExist(AUDIOPATH + audio.toString() + ".audio")) {
			audio = FileHandler.ReadAudio(AUDIOPATH + audio.toString() + ".audio");
        }
		
		return audio;
	}
}