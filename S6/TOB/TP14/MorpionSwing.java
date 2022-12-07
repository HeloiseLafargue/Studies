import javax.swing.*;
import java.awt.*;
import javax.swing.event.*;
import java.awt.event.*;
import java.util.*;

/** Programmation d'un jeu de Morpion avec une interface graphique Swing.
  *
  * REMARQUE : Dans cette solution, le patron MVC n'a pas été appliqué !
  * On a un modèle (?), une vue et un contrôleur qui sont fortement liés.
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.4 $
  */

public class MorpionSwing {
	
	// les images à utiliser en fonction de l'état du jeu.
	private static final Map<ModeleMorpion.Etat, ImageIcon> images
		= new HashMap<ModeleMorpion.Etat, ImageIcon>();
	static {
		images.put(ModeleMorpion.Etat.VIDE, new ImageIcon("blanc.jpg"));
		images.put(ModeleMorpion.Etat.CROIX, new ImageIcon("croix.jpg"));
		images.put(ModeleMorpion.Etat.ROND, new ImageIcon("rond.jpg"));
	}

// Choix de réalisation :
// ----------------------
//
//  Les attributs correspondant à la structure fixe de l'IHM sont définis
//	« final static » pour montrer que leur valeur ne pourra pas changer au
//	cours de l'exécution.  Ils sont donc initialisés sans attendre
//  l'exécution du constructeur !

	private ModeleMorpion modele;	// le modèle du jeu de Morpion

//  Les éléments de la vue (IHM)
//  ----------------------------

	/** Fenêtre principale */
	private JFrame fenetre;

	/** Bouton pour quitter */
	private final JButton boutonQuitter = new JButton("Q");
	
	/** Action de quitter */
	private final ActionListener actionQuitter = new ActionQuitter();

	/** Bouton pour commencer une nouvelle partie */
	private final JButton boutonNouvellePartie = new JButton("N");

	/** Cases du jeu */
	private final JLabel[][] cases = new JLabel[3][3];

	/** Zone qui indique le joueur qui doit jouer */
	private final JLabel joueur = new JLabel();


// Le constructeur
// ---------------

	/** Construire le jeu de morpion */
	public MorpionSwing() {
		this(new ModeleMorpionSimple());
	}

	/** Construire le jeu de morpion */
	public MorpionSwing(ModeleMorpion modele) {
		// Initialiser le modèle
		this.modele = modele;

		// Créer les cases du Morpion
		for (int i = 0; i < this.cases.length; i++) {
			for (int j = 0; j < this.cases[i].length; j++) {
				this.cases[i][j] = new JLabel();
			}
		}

		// Initialiser le jeu
		this.recommencer();

		// Construire la vue (présentation)
		//	Définir la fenêtre principale
		this.fenetre = new JFrame("Morpion");
		this.fenetre.setLocation(100, 200);

		// Définir le gestionnaire de placement (Container)
		Container contenu = this.fenetre.getContentPane();
		contenu.setLayout(new GridLayout(4,3));
		
		// Positionner les éléments Swing		
		// -les cases
		for (int i=0; i<this.cases.length; i++) {
			for (int j=0; j<this.cases[i].length; j++) {
				contenu.add(this.cases[i][j]);
			}
		}
		// -les boutons
		contenu.add(boutonNouvellePartie);
		contenu.add(joueur);
		contenu.add(boutonQuitter);
		// -la barre de menu
		JMenuBar jmb = new JMenuBar();
		JMenu menuJeu = new JMenu("Jeu");
		jmb.add(menuJeu);
		
		JMenuItem itemNouvellePartie = new JMenuItem("Nouvelle partie");
		JMenuItem itemQuitter = new JMenuItem("Quitter");
		menuJeu.add(itemNouvellePartie);
		menuJeu.addSeparator();
		menuJeu.add(itemQuitter);
		
		this.fenetre.setJMenuBar(jmb);
		
		// Construire le contrôleur (gestion des événements)
		this.fenetre.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		boutonQuitter.addActionListener(this.actionQuitter);
		boutonNouvellePartie.addActionListener(this.actionRecommencer);
		
		for (int i=0; i<this.cases.length; i++) {
			for (int j=0; j<this.cases[i].length; j++){
				this.cases[i][j].addMouseListener(new ActionCliquer(i,j));
				
			}
		}
		
		
		// afficher la fenêtre
		this.fenetre.pack();			// redimmensionner la fenêtre
		this.fenetre.setVisible(true);	// l'afficher
	}

// Quelques réactions aux interactions de l'utilisateur
// ----------------------------------------------------

	/** Recommencer une nouvelle partie. */
	public void recommencer() {
		this.modele.recommencer();

		// Vider les cases
		for (int i = 0; i < this.cases.length; i++) {
			for (int j = 0; j < this.cases[i].length; j++) {
				this.cases[i][j].setIcon(images.get(this.modele.getValeur(i, j)));
			}
		}

		// Mettre à jour le joueur
		joueur.setIcon(images.get(modele.getJoueur()));
	}



// La méthode principale
// ---------------------

	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				new MorpionSwing();
			}
		});
	}
	private class ActionQuitter implements ActionListener {
		@Override
		public void actionPerformed(ActionEvent e) {
			System.out.println("");
			System.exit(0);
		}
	}
	private class ActionCliquer extends MouseAdapter {
		private int x;
		private int y;
		
		public ActionCliquer (int x, int y) {
			this.x = x;
			this.y = y;
		}
		@Override
		public void mouseClicked(MouseEvent e) {
			try {
				modele.cocher(x, y);
				cases[x][y].setIcon(images.get(modele.getValeur(x, y)));
				joueur.setIcon(images.get(modele.getJoueur()));
			}
			catch {
				
			}
			
		}
	}
}
