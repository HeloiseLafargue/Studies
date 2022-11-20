package Lecteur;

import java.io.File;
import java.io.IOException;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.UnsupportedAudioFileException;

public class Lecteur{
    private Playlist playlistAutomatique = new PlaylistAutomatique();
    private Playlist playlistActuelle = playlistAutomatique;

    // to store current position
    Long currentFrame;
    Clip clip;

    // current status of clip
    public String status;

    AudioInputStream audioInputStream;
    // Constructeur
    public Lecteur() throws UnsupportedAudioFileException, IOException, LineUnavailableException {

        // create AudioInputStream object
        System.out.println(getPlaylistActuelle().getMusiqueCourante().getFilepath());
        audioInputStream = AudioSystem.getAudioInputStream(new File(getPlaylistActuelle().getMusiqueCourante().getFilepath()).getAbsoluteFile());
        this.status = "play";
        // create clip reference
        clip = AudioSystem.getClip();

        // open audioInputStream to the clip
        clip.open(audioInputStream);

        clip.loop(Clip.LOOP_CONTINUOUSLY);
    }
    // Change Playlist
    public void changerPlaylist(String playlist) throws IOException, LineUnavailableException, UnsupportedAudioFileException {
        setPlaylistActuelle(FileHandler.ReadPlaylist(playlist));
        restart();
    }

    // Create a new Playlist
    public void creePlaylist(String nom, FichierAudio[] fichiersAudios) {
        setPlaylistActuelle(new PlaylistManuelle(nom, fichiersAudios));
    }


    // Method to play the audio
    public void play() throws UnsupportedAudioFileException, IOException, LineUnavailableException {
        //start the clip
        clip.start();

        status = "play";
    }

    // Method to pause the audio
    public void pause() {
        if (status.equals("paused")){
            System.out.println("audio is already paused");
            return;
        }
        this.currentFrame =
                this.clip.getMicrosecondPosition();
        clip.stop();
        status = "paused";
    }

    // Method to resume the audio
    public void resumeAudio() throws UnsupportedAudioFileException,
            IOException, LineUnavailableException
    {
        if (status.equals("play")) {
            System.out.println("Audio is already "+
                    "being played");
            return;
        }
        clip.close();
        resetAudioStream();
        clip.setMicrosecondPosition(currentFrame);
        this.play();
    }

    // Method to restart the audio
    public void restart() throws IOException, LineUnavailableException,
            UnsupportedAudioFileException
    {
        clip.stop();
        clip.close();
        resetAudioStream();
        currentFrame = 0L;
        clip.setMicrosecondPosition(0);
        this.play();
    }

    // Method to stop the audio
    public void stop() throws UnsupportedAudioFileException,
            IOException, LineUnavailableException
    {
        currentFrame = 0L;
        clip.stop();
        clip.close();
    }

    // Method to jump over a specific part
    public void jump(long c) throws UnsupportedAudioFileException, IOException,
            LineUnavailableException
    {
        if (c > 0 && c < clip.getMicrosecondLength()) {
            clip.stop();
            clip.close();
            resetAudioStream();
            currentFrame = c;
            clip.setMicrosecondPosition(c);
            this.play();
        }
    }

    // Method to reset audio stream
    public void resetAudioStream() throws UnsupportedAudioFileException, IOException,
            LineUnavailableException
    {
        //System.out.println("lecture de : " + this.getPlaylistActuelle().getMusiqueCourante().getFilepath());
        audioInputStream = AudioSystem.getAudioInputStream(new File(this.getPlaylistActuelle().getMusiqueCourante().getFilepath()).getAbsoluteFile());
        clip.open(audioInputStream);
        clip.loop(Clip.LOOP_CONTINUOUSLY);
    }

    // Audio Suivant
    public void suivant() throws UnsupportedAudioFileException, IOException, LineUnavailableException {
        getPlaylistActuelle().suivant();
        restart();
    }

    public void changer(String name) throws UnsupportedAudioFileException, IOException, LineUnavailableException {
        getPlaylistActuelle().changeMusic(name);
        restart();
    }

    // Audio Precedent
    public void precedent() throws UnsupportedAudioFileException, IOException, LineUnavailableException {
        getPlaylistActuelle().precedent();
        restart();
    }
    public Playlist getPlaylistActuelle() {
        return playlistActuelle;
    }
    public void setPlaylistActuelle(Playlist playlistActuelle) {
        this.playlistActuelle = playlistActuelle;
    }

    public Playlist getPlaylistAutomatique() {
        return this.playlistAutomatique;
    }
}
