import java.awt.Color;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.junit.*;
import static org.junit.Assert.*;

/**
  * Classe de test de la classe Cercle complétant SujetCercleTest.
  * @author	Héloïse Lafargue
  * @version 1.0
  */
public class CercleTest {

	// précision pour les comparaisons réelle
	public final static double EPSILON = 0.001;

	// Les points du sujet
	private Point A, B, C, D, E;

	// Les cercles du sujet
	private Cercle C3, C4, C5;


	@Before public void setUp() {
		// Construire les points
		A = new Point(1, 2);
		B = new Point(2, 1);
		C = new Point(4, 1);
		D = new Point(8, 1);
		E = new Point(8, 4);

		// Construire les cercles
		C3 = new Cercle(A, B);
		C4 = new Cercle(B, C);
		C4.setCouleur(Color.red);
		C5 = Cercle.creerCercle(D, E);
		
	}

	/** Vérifier si deux points ont mêmes coordonnées.
	  * @param p1 le premier point
	  * @param p2 le deuxième point
	  */
	static void memesCoordonnees(String message, Point p1, Point p2) {
		assertEquals(message + " (x)", p1.getX(), p2.getX(), EPSILON);
		assertEquals(message + " (y)", p1.getY(), p2.getY(), EPSILON);
	}

	@Test public void testerE12() {
		assertNotNull(A);
		assertNotNull(B);
		memesCoordonnees("E12 : Centre de C3 incorrect", 
				new Point((A.getX()+B.getX())/2, (A.getY()+B.getY())/2), C3.getCentre());
		assertEquals("E12 : Rayon de C3 incorrect", A.distance(B)/2, C3.getRayon(), EPSILON);
		assertEquals(Color.blue, C3.getCouleur());
	}
	
	@Test public void testerE13() {
		assertNotNull(B);
		assertNotNull(C);
		memesCoordonnees("E13 : Centre de C4 incorrect", 
				new Point((B.getX()+C.getX())/2, (B.getY()+C.getY())/2), C4.getCentre());
		assertEquals("E13 : Rayon de C4 incorrect", B.distance(C)/2, C4.getRayon(), EPSILON);
		assertEquals(Color.red, C4.getCouleur());
	}
	
	@Test public void testerE14() {
		assertNotNull(D);
		assertNotNull(E);
		memesCoordonnees("E14 : Centre de C5 incorrect", D, C5.getCentre());
		assertEquals("E14 : Rayon de C5 incorrect", D.distance(E), C5.getRayon(), EPSILON);
		assertEquals(Color.blue, C5.getCouleur());
		
	}

	@Test public void testerE14bis() throws Exception {
		Method creerCercle = null;
	}
}
