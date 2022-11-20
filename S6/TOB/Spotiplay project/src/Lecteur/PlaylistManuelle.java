package Lecteur;


import java.io.*;

public class PlaylistManuelle implements Playlist {
	
	private String name;
	private static final long serialVersionUID = 1L;
    private FichierAudio musiqueCourante;
    private FichierAudio[] listeMusique;
    private int curseur;

    public PlaylistManuelle(String nom, FichierAudio[] fichiersAudios){
    	this.listeMusique = fichiersAudios;
        System.out.println(fichiersAudios);
        this.curseur = 0;
        this.musiqueCourante = fichiersAudios[this.curseur];
        this.name = nom;
    }

    public PlaylistManuelle(){
        this.curseur = 0;
    }

    @Override
    public void suivant() {
    	this.curseur ++;
    	this.curseur = this.curseur % this.listeMusique.length;
    	this.musiqueCourante = listeMusique[this.curseur];
    }

    @Override
    public void precedent() {
    	this.curseur--;
    	this.curseur = this.curseur < 0 ? (this.listeMusique.length-1) : this.curseur % this.listeMusique.length;
    	this.musiqueCourante = listeMusique[this.curseur];
    }

    @Override
    public void changeMusic(String name){
        for(int i = 0 ; i < listeMusique.length ; i++) {
            if(this.listeMusique[i].getName().equals(name) ) {
                this.curseur = i;
                this.musiqueCourante = FileHandler.getAudio(this.listeMusique[this.curseur].getFilepath());
            }
        }
    }

    @Override
    public FichierAudio[] getFichierAudio() {
        return this.listeMusique;
    }

    public FichierAudio getMusiqueCourante() {
    	if(this.musiqueCourante == null) {
    		this.musiqueCourante = listeMusique[this.curseur];
    	}
        return this.musiqueCourante;
    }
	
	public String getName() {
        return this.name;
    }
	
	public void setName(String name) {
        this.name = name;
    }
	
	public String toString() {
    	return this.name;
    }
	
	public void add(FichierAudio audio) {
		FichierAudio[] intermediaire = this.listeMusique;
		this.listeMusique = new FichierAudio[intermediaire.length + 1];
		for(int i = 0 ; i < intermediaire.length ; i ++) {
			this.listeMusique[i] = intermediaire[i];
		}
		this.listeMusique[-1] = audio;
	}
}
