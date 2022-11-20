package Lecteur;

public interface Playlist extends Saveable {
    public void suivant();
    public void precedent();
    public FichierAudio getMusiqueCourante();
    public void changeMusic(String name);
    public FichierAudio[] getFichierAudio();
}
