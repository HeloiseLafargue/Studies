package API;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import Lecteur.*;

import javax.imageio.ImageIO;

public class DataSearchTest {
	
	public static void main(String[] args) {
		
		String path = "/Users/nathan/Documents/N7/L3/S6/Tob/projet-long.nosync/spotiplay/ProjetLong/Audio/parov.mp3";
		DataSearch searcher = new DataSearch();
		FichierAudio file;
		
		try {
		file = searcher.getInformations(path);
		System.out.println(file.getArtist());
		System.out.println(file.getAlbum());
		System.out.println(file.getName());
		BufferedImage image = searcher.getAlbumCover(file);
		
		File outputfile = new File("image.jpg");
		ImageIO.write(image, "jpg", outputfile);
		
		} catch (FileErrorException e) {
			System.out.println(e);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		
		
		
	}

}
