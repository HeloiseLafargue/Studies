package Lecteur;


import java.io.File;


public class PlaylistAutomatique implements Playlist {
	final int CONSTANTEINDEX = 0; 
	//petit topo de la variable ci dessus, si vous êtes sur macOS et autres sytèmes unix (genre linux) 
	//mettez cette variable à 0. Pour les utilisateurs Windows mettez la à 1.
	//Attention à changer aussi dans le Controller 
	//	A CHANGER!!!!!!

	private String name;
	private static final long serialVersionUID = 1L;
    private FichierAudio musiqueCourante;
    private File[] listeMusique;
    private int curseur;

    public PlaylistAutomatique (){
        File f = new File(FileHandler.FICHIERSWAVPATH);
        this.listeMusique = f.listFiles();
        System.out.println(f.getAbsolutePath());
        this.curseur = 0;
        this.name = "Playlist Automatique";
        this.musiqueCourante = FileHandler.getAudio(this.listeMusique[this.curseur].getPath().split("\\\\")[CONSTANTEINDEX]);
    	
        for(int i = 0 ; i < listeMusique.length ; i++) {
        	if(!FileHandler.FileExist(this.listeMusique[i].getPath().split("\\\\")[CONSTANTEINDEX])) {
        		//System.out.println("sauvegarde de : " + listeMusique[i].getPath());
        		FileHandler.Save(new FichierAudio(listeMusique[i].getPath()));
        	}
        }
        //System.out.println("PLAYLIST AUTO CREE");
    }

    @Override
    public void suivant() {
    	this.curseur ++;
    	this.curseur = this.curseur % this.listeMusique.length;
    	
    	this.musiqueCourante = FileHandler.getAudio(this.listeMusique[this.curseur].getPath());
    }

    @Override
    public void precedent() {
    	this.curseur--;
    	this.curseur = this.curseur < 0 ? (this.listeMusique.length-1) : this.curseur % this.listeMusique.length;

    	this.musiqueCourante = FileHandler.getAudio(this.listeMusique[this.curseur].getPath());

    }
    @Override
    public void changeMusic(String name){
        for(int i = 0 ; i < listeMusique.length ; i++) {
            if(this.listeMusique[i].getPath().split("\\\\")[CONSTANTEINDEX].equals(name) ) {
                this.curseur = i;
                this.musiqueCourante = FileHandler.getAudio(this.listeMusique[this.curseur].getPath());
            }
        }
    }

    @Override
    public FichierAudio[] getFichierAudio() {
        FichierAudio[] list= new FichierAudio[this.listeMusique.length];
        
        FichierAudio[] listInformations = FileHandler.ListFichiersAudios();
        
        for(int i = 0 ; i < listeMusique.length ; i++) {
            list[i] = FileHandler.getAudio(this.listeMusique[i].getPath());
            
            for (int j = 0; j < listInformations.length; j ++) {				//on regarde si on a déjà les infos
            	if (list[i].getFilepath().equals(listInformations[j].getFilepath())) {
            		list[i] = listInformations[j];
            	}
            	else {
            		FileHandler.Save(list[i]); //si pas d'infos on enregustre 
            	}
            }
            if (listInformations.length == 0) { //si aucune info on enregistre tout 
            	FileHandler.Save(list[i]);
            }
            
        }
        return list;
    }


    public File[] getListeMusique() {
        return this.listeMusique;
    }

    public FichierAudio getMusiqueCourante() {
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
}
