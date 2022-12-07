import afficheur.Ecran;
import java.awt.Color;

/**
  * Exemple d'utilisation de la classe Ecran.
  */
class AfficheurTexte {

	public static void main(String[] args) {
		
		Point p = new Point(1.0, 2.0);
		Point p2 = new Point(6.0, 2.0);
		Point p3 = new Point(11.0, 9.0);
		Point centre = new Point(4.0, 4.0);
		double rayon = 2.5;
		Segment segment = new Segment(p2, p3);
		Cercle cercle = new Cercle(centre, rayon, Color.yellow);
		
		// Afficher le schéma
		System.out.println("Le schéma est composé de : ");
		p.afficher();		System.out.println();
		segment.afficher();		System.out.println();
		cercle.afficher();	System.out.println();
			
	}

}
