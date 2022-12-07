import java.awt.Color;

public class EnsembleChaine implements Ensemble{
	
	private Cellule cellule;
	
	
	public boolean estVide() {
		return this.cellule == null;	
	}
	
	public int cardinal() {
		if (this.estVide()) {
			return 0;
		}
		else {
			return this.cellule.suivante.cardinal();
		}
	}
	
	public boolean contient(int x) {
		if (this.estVide()) {
			return false;
		}
		else if (this.cellule.element == x) {
			return true;
		}
		else {
			return this.cellule.suivante.contient(x);
		}
	}
	
	public void ajouter(int x) {
		if (this.estVide()) {
			this.cellule = new Cellule();
			this.cellule.element = x;
			}
	}
	
	public void supprimer(int x) {
		if (this.contient(x)) {
			this.cellule = this.cellule.suivante;
		}
		else {
			this.cellule.suivante.supprimer(x);
		}
	}
}