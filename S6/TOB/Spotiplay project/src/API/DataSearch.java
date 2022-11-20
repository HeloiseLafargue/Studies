package API;

import java.util.Map;
import java.util.HashMap;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.imageio.ImageIO;
import Lecteur.*;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.*;

import com.acrcloud.utils.ACRCloudRecognizer;
//import com.acrcloud.utils.ACRCloudExtrTool;

public class DataSearch {
	// clees (à changer tous les 15 jours)

	private final static String hostACR = "identify-eu-west-1.acrcloud.com";
	private final static String access_keyACR = "76fba97d5379288353cba88c76c0898b";
	private final static String access_secretACR = "9WYOITtcvbtHMhKgA1hHJXh87f4mzMDhTADRn1x2";
	private ACRCloudRecognizer re;

	private final static String hostLASTFM = "http://ws.audioscrobbler.com/2.0/";
	private final static String access_keyLASTFM = "9bd2de6f7a17cdfb1d0f125f0ab9a84a";

	public DataSearch() {
		Map<String, Object> config = new HashMap<String, Object>();
		config.put("host", hostACR);
		config.put("access_key", access_keyACR);
		config.put("access_secret", access_secretACR);
		config.put("rec_type", ACRCloudRecognizer.RecognizerType.AUDIO);
		config.put("debug", false);
		config.put("timeout", 10); // seconds sent to the server
		re = new ACRCloudRecognizer(config);
	}

	/**
	 * Search file informations on the internet and return a completed FichierAudio
	 * 
	 * @param filePath
	 * @return FichierAudio Audio Object completed
	 **/

	public FichierAudio getInformations(String filePath) throws FileErrorException, ConnexionErrorException {
		String result = re.recognizeByFile(filePath, 0);
		String title, artist, album;
		// System.out.println(result);

		try {
			JSONObject ob = new JSONObject(result);

			// if no informations have been found
			if (!ob.getJSONObject("status").getString("msg").equals("Success")) {
				return new FichierAudio(filePath, "Unknown", "Unknown", "Unknown");
			}

			JSONArray music = ob.getJSONObject("metadata").getJSONArray("music");

			try {
				title = music.getJSONObject(0).getString("title");
			} catch (org.json.JSONException e) {
				title = "Unknown";
			}
			try {
				album = music.getJSONObject(0).getJSONObject("album").getString("name");
			} catch (org.json.JSONException e) {
				album = "Unknown";
			}
			JSONArray artists = music.getJSONObject(0).getJSONArray("artists");
			try {
				artist = artists.getJSONObject(0).getString("name");
			} catch (org.json.JSONException e) {
				artist = "Unknown";
			}

			return new FichierAudio(filePath, title, artist, album);
		} catch (org.json.JSONException e) {
			throw new ConnexionErrorException(
					"An issue occured when sending the file to the server, maybe the token has been revoked or the file doesn't exit anymore");
		}
	}

	/**
	 * Search File Informations on the internet and return a Completed FichierAudio
	 * 
	 * @param AudioObject that we want to update it's informations
	 * @return FichierAudio Audio Object completed
	 **/
	public FichierAudio getInformations(FichierAudio AudioObject) {
		return getInformations(AudioObject.getFilepath());
	}

	/**
	 * Search song album cover on the Internet
	 * 
	 * @param AudioObject song that we want to update it's informations
	 * @return Image image of the album cover
	 **/
	public BufferedImage getAlbumCover(FichierAudio AudioObject) throws FileErrorException{
		StringBuilder result = new StringBuilder();
		String resultstr;
		String artist = AudioObject.getArtist().replace(" ", "+");
		String album = AudioObject.getAlbum().replace(" ", "+");

		try {
			URL url = new URL(hostLASTFM + "?method=album.getinfo&api_key=" 
					+ access_keyLASTFM + "&artist=" + artist
					+ "&album=" + album + "&format=json");
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			try (BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
				for (String line; (line = reader.readLine()) != null;) {
					result.append(line.replace("\\", ""));
				}
				resultstr = result.toString();
				resultstr = resultstr.split("\"image\":")[1];
				resultstr = resultstr.split("],")[0];
				resultstr = resultstr + ']';
			}
			conn.disconnect();
		} catch (IOException e) {
			throw new ConnexionErrorException("An error occured when connecting to the server");
		}
		System.out.println(resultstr);
		try {
			JSONArray ob = new JSONArray(resultstr);
			String coverLink = ob.getJSONObject(4).getString("#text");
			System.out.println(coverLink);
			URL url = new URL(coverLink);

			Image image = ImageIO.read(url);
			return (BufferedImage) image;

		} catch (org.json.JSONException e) {
			throw new FileErrorException("No album cover found");
		} catch (IOException  e) {
			throw new FileErrorException("No album cover found");
		} 

	}
	
}
