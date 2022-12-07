import java.awt.Color;

/** Cercle à partir du centre et du rayon.
 * @author  Héloïse Lafargue
 * @version 2.0
 */
 
 
 public class Cercle implements Mesurable2D {

    private Point centre;
    private double rayon;  
    private Color couleur;
 
    public final static double PI = Math.PI;
    
	/** Construire le cercle défini par son centre et son rayon.
	 * @param c le point centre
	 * @param r le rayon (réel positif)
	 */
    
    public Cercle (Point c, double r) {
    	assert c != null && r > 0;
    	this.centre = new Point(c.getX(), c.getY());	// pour E18
    	this.rayon = r;
    	this.couleur = Color.blue;  
    }
    
        
	/** Construire le cercle à partir de deux points diamétralement opposés.
	 * @param p1 le premier point
	 * @param p2 le second point
	 */
    
    public Cercle (Point p1, Point p2) {
    	assert p1 != null && p2 != null && p1.distance(p2) != 0;
    	double x = (p1.getX() + p2.getX()) / 2;		// l'abcisse du centre
    	double y = (p1.getY() + p2.getY()) / 2;		// l'ordonnée du centre
    	this.centre = new Point(x,y);
    	this.rayon = p1.distance(p2) / 2;
    	this.couleur = Color.blue; 
    }
    
    
	/** Construire le cercle à partir de deux points diamétralement opposés et de sa couleur.
	 * @param p1 le premier point
	 * @param p2 le second point
	 * @param col la couleur
	 */
    
    public Cercle(Point p1, Point p2, Color col) {
    	assert p1 != null && p2 != null && p1.distance(p2) != 0 && col != null;
    	double x = (p1.getX() + p2.getX()) / 2;		// l'abcisse du centre
    	double y = (p1.getY() + p2.getY()) / 2;		// l'ordonnée du centre
    	this.centre = new Point(x,y);
    	this.rayon = p1.distance(p2) / 2;
    	this.couleur = col;
    }
    
    
    /** Translater le cercle.
 	* @param dx le déplacement suivant l'axe des X
 	* @param dy le déplacement suivant l'axe des Y
 	*/
 	public void translater(double dx, double dy) {
 		this.centre.translater(dx, dy);		
 	}
 	
 	
	/** Obtenir le centre du cercle
	 * @return coordonnées du centre
	 */
 	public Point getCentre() {
 		return new Point(centre.getX(), centre.getY());		// pour E18
 	}
    
	/** Obtenir le rayon du cercle
	 * @return rayon du cerle
	 */
 	public double getRayon() {
 		return this.rayon;	
 	}
    
	/** Obtenir le diamètre du cercle
	 * @return diamètre du cercle
	 */
 	public double getDiametre() {
 		return 2*this.rayon;	
 	}
 	
	/** Savoir si un point est à l’intérieur (au sens large) d’un cercle
	 * @param p le point à tester
	 * @return booléan, True si le poin est dans le cercle
	 */
 	public boolean contient(Point p) {
 		assert p != null;
 		return (this.centre.distance(p) <= this.rayon);	
 	}
 	
 	
	/** Obtenir le périmètre du cercle
	 * @return périmètre du cercle
	 */
 	public double perimetre() {
 		
 		return 2*PI*this.rayon;
 	}
 	
 	
	/** Obtenir l'aire du cercle
	 * @return aire du cercle
	 */
 	public double aire() {
 	
 		return PI*(this.rayon)*(this.rayon);
 	}
 	
	/** Obtenir la couleur du cercle
	 * @return couleur du cercle
	 */
 	public Color getCouleur() {
 		return this.couleur;
 	}
 
	/** Changer la couleur du cercle
	 * @param couleur2 la nouvelle couleur 
	 */
 	public void setCouleur(Color couleur) {
 		assert couleur != null;
 		this.couleur = couleur;
 	}
 	
	/** Changer le rayon du cercle
	 * @param rayon2 le nouveaux rayon
	 */
 	public void setRayon(double rayon) {
 		assert rayon > 0;
 		this.rayon = rayon;
 	}
 	
	/** Changer le diamètre du cercle
	 * @param diametre2 le nouveaux rayon
	 */
 	public void setDiametre(double diametre) {
 		assert diametre > 0;
 		this.rayon = diametre / 2;
 	}
 	
	/** Afficher le cercle sous forme Cr@(a, b) où r : rayon et (a, b) : centre
	 */
 	public void afficher() {
 		System.out.print("C");
		System.out.print(this.rayon);
		System.out.print("@");
		System.out.print(centre.toString());
	}  
 	
 	/** Afficher le cercle.
 	 * @return String
	 */
	public String toString() {
		return "C" + this.rayon + "@" + this.centre;
	}
	 	
 	/** Une méthode de classe qui permet de créer un cercle à partir de
deux points, le premier correspond au c et le deuxième est un point du cercle
(de sa circonférence). Ces deux points forment donc un rayon du cercle.
 	 * @param c le centre du cercle
 	 * @param p
 	 * @return cercle
 	 */
	
 	public static Cercle creerCercle(Point c, Point p) {
 		assert c != null && p != null && c.distance(p)!= 0;
 		double rayon = c.distance(p); 
 		return new Cercle(c, rayon);
 	}   
    
 }
