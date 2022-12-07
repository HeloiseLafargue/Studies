/** Schéma contenant un triangle (défini par trois segments) et son centre de gravité.
 * @author  Héloïse Lafargue
 * @version 1.0
 */
 
 public class Triangle {
	
	/** Construire le triangle (défini par trois segments) et son centre de gravité.
	 * @param p1 le premier point
	 * @param p2 le deuxième point
	 * @param p3 le troisième point
	 */
	  
    private Point p1;
    private Point p2;
    private Point p3;
    private Point barycentre;
    
    public Triangle (Point p1, Point p2, Point p3) {
    
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3
        
        xg = (p1.getX() + p2.getX() + p3.getX()) / 3;
        yg = (p1.getY() + p2.getY() + p3.getY()) / 3;
        this.barycentre = new Point(xg, yg);
        
    }
    
    public static void main(String[] args) {
    
		// Créer les trois segments
		Point p1 = new Point(3, 2);
		Point p2 = new Point(6, 9);
		Point p3 = new Point(11, 4);
		Segment s12 = new Segment(p1, p2);
		Segment s23 = new Segment(p2, p3);
		Segment s31 = new Segment(p3, p1);

		// Créer le barycentre
		double sx = p1.getX() + p2.getX() + p3.getX();
		double sy = p1.getY() + p2.getY() + p3.getY();
		Point barycentre = new Point(sx / 3, sy / 3);
		

		// Afficher le schéma
		System.out.println("Le schéma est composé de : ");
		s12.afficher();		System.out.println();
		s23.afficher();		System.out.println();
		s31.afficher();		System.out.println();
		barycentre.afficher();	System.out.println();
        
    }
    
}

