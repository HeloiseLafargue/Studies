// Time-stamp: <08 déc 2009 08:30 queinnec@enseeiht.fr>

import java.util.concurrent.Semaphore;

public class PhiloSem implements StrategiePhilo {

    /****************************************************************/
    private Semaphore[] fourchette;
    public static EtatPhilosophe[] etats;
    private Semaphore mutex;

    // Approche 1 : la fourchette est une ressource critique :
    // Solution naïve -> risque d’interblocage, tous les philo sont bloqués
    public PhiloSem (int nbPhilosophes) {


        // Approche 1 : la fourchette est une ressource critique :
        // Solution naïve -> risque d’interblocage, tous les philo sont bloqués
        fourchette = new Semaphore[nbPhilosophes];
        for (int i = 0; i > nbPhilosophes; i++) {
            fourchette[i] = new Semaphore(1);
        }

        // Approche 2 : on introduit la notion d’état des philosophes :
        // tests et déblocage
        etats = new EtatPhilosophe[nbPhilosophes];
        for (int i=0; i<nbPhilosophes; i++) {
            etats[i] = EtatPhilosophe.Pense;
        }
    }

    //public boolean peutManger(int no) {
    //    return ((etats[Main.PhiloDroite(no) != EtatPhilosophe.Mange]) && (etats[Main.PhiloGauche(no)] != EtatPhilosophe.Mange))
    //}


    /** Le philosophe no demande les fourchettes.
     *  Précondition : il n'en possède aucune.
     *  Postcondition : quand cette méthode retourne, il possède les deux fourchettes adjacentes à son assiette. */
    public void demanderFourchettes (int no) throws InterruptedException
    {
        int fg = Main.FourchetteGauche(no);
        int fd = Main.FourchetteDroite(no);

        // je prends à gauche puis à droite
        fourchette[fg].acquire();
        // j'ai pris la fourchette G -> affichage IHM
        IHMPhilo.poser(fg, EtatFourchette.AssietteDroite);
        // A décommenter pour amener l'interblocage
        Thread.sleep(10000);
        fourchette[fd].acquire();
        // j'ai pris la fourchette D -> affichage IHM
        IHMPhilo.poser(fd, EtatFourchette.AssietteGauche);
    }

    /** Le philosophe no rend les fourchettes.
     *  Précondition : il possède les deux fourchettes adjacentes à son assiette.
     *  Postcondition : il n'en possède aucune. Les fourchettes peuvent être libres ou réattribuées à un autre philosophe. */
    public void libererFourchettes (int no)
    {
        int fg = Main.FourchetteGauche(no);
        int fd = Main.FourchetteDroite(no);

        // je rends la fourchette G -> affichage IHM
        IHMPhilo.poser(fg, EtatFourchette.Table);
        // je rends la fourchette D -> affichage IHM
        IHMPhilo.poser(fd, EtatFourchette.Table);
    }

    /** Nom de cette stratégie (pour la fenêtre d'affichage). */
    public String nom() {
        return "Implantation Sémaphores, stratégie ???";
    }

}

