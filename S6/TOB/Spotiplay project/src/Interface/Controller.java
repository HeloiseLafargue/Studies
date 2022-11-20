package Interface;
import java.awt.image.BufferedImage;
import API.*;
import java.io.File;
import java.io.IOException;
import API.*;
import Lecteur.*;
import java.net.URL;
import java.util.ResourceBundle;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.embed.swing.SwingFXUtils;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.scene.text.Text;

import javax.imageio.ImageIO;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

import Lecteur.FichierAudio;
import Lecteur.FileHandler;
import Lecteur.Lecteur;

public class Controller implements Initializable {

    @FXML public Text title_name;
    @FXML public Text artist_name;
    @FXML public Text album_name;
    @FXML public ProgressBar progress_bar;
    @FXML public Button like_music;
    @FXML public Button internet;
    @FXML public Button previous_music;
    @FXML public Button next_music;
    @FXML public Button play_music;
    @FXML public ImageView album_image;
    @FXML public Tab label_titre;
    @FXML public Tab label_playlist;
    @FXML public Tab label_artistes;
    @FXML public Tab label_album;
    @FXML public ScrollBar scroll_titres;
    @FXML public ScrollBar scroll_playlist;
    @FXML public ScrollBar scroll_artistes;
    @FXML public ScrollBar scroll_albums;
    @FXML public ListView<Text> liste_titres;
    @FXML public ListView<Text> liste_playlist;
    @FXML public ListView<Text> liste_artistes;
    @FXML public ListView<Text> liste_album;


    static Lecteur clip;
    final int CONSTANTEINDEX = 0; 
	//petit topo de la variable ci dessus, si vous êtes sur macOS et autres sytèmes unix (genre linux) 
	//mettez cette variable à 0. Pour les utilisateurs Windows mettez la à 1.
	//Attention à changer aussi dans Playlist Automatique 
	//	A CHANGER!!!!!!
    final char CONSTANTFILE = '/';
    //sur windows on a '\/' pour les autres c'est '/'
    //A CHANGER !!!!!!!!
    
    private Image def; //image par défaut

    @Override
    public void initialize(URL location, ResourceBundle resourceBundle) {
    	
        try{
            clip = new Lecteur();
            initialize_listTitres();
            initialize_listPlaylist();
            softReset();
        } catch (Exception ex) {
            System.out.println("Error with initialisation.");
            ex.printStackTrace();
        }
        
        try {
            File img = new File("default.png");
            BufferedImage defbuff = ImageIO.read(img); 
            this.def = SwingFXUtils.toFXImage(defbuff, null);	
            album_image.setImage(this.def);
        } catch (IOException e) { 
        	System.out.println("Error with initialisation.");
            e.printStackTrace(); 
        }
        
    }

    public void play_pause(ActionEvent e) throws IOException {
        try{
            if (clip.status.equals("play")) {
                clip.pause();
            } else {
                clip.play();
            }
        } catch (Exception ex) {
            System.out.println("Error with playing sound.");
            ex.printStackTrace();
        }
    }
    
    public void internet(ActionEvent e) throws IOException {
        System.out.println(System.getProperty("user.dir") + CONSTANTFILE + clip.getPlaylistActuelle().getMusiqueCourante().getFilepath());
        
        String path = System.getProperty("user.dir") + CONSTANTFILE + clip.getPlaylistActuelle().getMusiqueCourante().getFilepath();
        
        DataSearch searcher = new DataSearch();
		FichierAudio file = new FichierAudio();
		BufferedImage image_found;
		
		try {
		file = searcher.getInformations(path);
		image_found = searcher.getAlbumCover(file);
		
		File outputfile = new File("image.jpg");
		
		Image image = SwingFXUtils.toFXImage(image_found, null);		
		ImageIO.write(image_found, "jpg", outputfile);
		
		album_image.setImage(image);
		
		
		} catch (FileErrorException ed) {
			System.out.println(ed);
		} ;
		title_name.setText(file.getName());
		artist_name.setText(file.getArtist());
		album_name.setText(file.getAlbum());
		
		initialize_listTitres();
		
        
    }

    public void previous(ActionEvent e) throws IOException {
        try {
            clip.precedent();
            softReset();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public void next(ActionEvent e) throws IOException {
        try {
            clip.suivant();
            softReset();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        // gÃ©rer le fait qu'il n'y a peut-Ãªtre pas de musique aprÃ¨s
    }
    public void like(ActionEvent e) throws IOException {
        // like/dislike
        if (like_music.getStyle()== "-fx-background-color: #cda7cd; ") {
            like_music.setStyle("-fx-background-color: #d9d9d9; ");
            //  -> add musique
        }else {
            like_music.setStyle("-fx-background-color: #cda7cd; ");
        }
        // ajout/suppression Ã  la playlist favoris
    }

    public void progress(ActionEvent e) throws IOException {
        // afficher la progression sur la barre

    }

    public void initialize_listTitres(){
        ObservableList<Text> names = FXCollections.observableArrayList();
        FichierAudio[] playlist = clip.getPlaylistActuelle().getFichierAudio();
        this.liste_titres.getItems().removeAll();
        this.liste_titres.getItems().clear();
        if(playlist != null){
            for ( FichierAudio e :  playlist) {
                Text name = new Text();
                name.setText(e.getName());
                name.setOnMouseClicked((EventHandler<MouseEvent>) mouseEvent -> {changeMusic(mouseEvent);});
                this.liste_titres.getItems().add(name);

            }
        }

        //this.liste_titres = new ListView<Text> (names) ;
    }

    public void initialize_listPlaylist(){
        ObservableList<Text> names = FXCollections.observableArrayList();
        String[] playlist = FileHandler.ListPlaylists();
        Text name= new Text();
        name.setText(clip.getPlaylistAutomatique().getName());
        name.setOnMouseClicked((EventHandler<MouseEvent>) mouseEvent -> {changeListMusic(mouseEvent);});
        this.liste_playlist.getItems().add(name);
        for (String e : playlist) {
            name= new Text();
            name.setText(e.split("\\\\")[CONSTANTEINDEX]);
            name.setOnMouseClicked((EventHandler<MouseEvent>) mouseEvent -> {changeListMusic(mouseEvent);});
            this.liste_playlist.getItems().add(name);
        }
    }

    public void changeMusic(MouseEvent mouseEvent){
        Text audio = (Text) mouseEvent.getSource();
        try {
            clip.changer(audio.getText());
            softReset();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void changeListMusic(MouseEvent mouseEvent){
        Text playlist = (Text) mouseEvent.getSource();
        if(playlist.getText().equals("Playlist Automatique")){
            clip.setPlaylistActuelle(clip.getPlaylistAutomatique());
        }else{
            clip.setPlaylistActuelle(FileHandler.ReadPlaylist(playlist.getText()));
        }
        initialize_listTitres();
        try {
            clip.stop();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void softReset() { //reinitialise seuelemnts les boutons, étiquettes et pochette d'album
    	title_name.setText(clip.getPlaylistActuelle().getMusiqueCourante().getName());
        artist_name.setText(clip.getPlaylistActuelle().getMusiqueCourante().getArtist());
        album_name.setText(clip.getPlaylistActuelle().getMusiqueCourante().getAlbum());
        album_image.setImage(this.def);
    }

}