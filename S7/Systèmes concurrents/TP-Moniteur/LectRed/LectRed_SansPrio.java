// Time-stamp: <08 Apr 2008 11:35 queinnec@enseeiht.fr>

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import Synchro.Assert;

/** Lecteurs/rédacteurs
 * stratégie d'ordonnancement: priorité aux rédacteurs,
 * implantation: avec un moniteur. */
public class LectRed_SansPrio implements LectRed
{
    private Lock moniteur;
    private Condition attente;
    private int nbLecteur; // nombre de lecteurs en train de lire
    private boolean redacteur;

    public LectRed_SansPrio()
    {
        moniteur = new java.util.concurrent.locks.ReentrantLock();
        attente = moniteur.newCondition();
        nbLecteur = 0;
        redacteur = false;
    }

    public void demanderLecture() throws InterruptedException
    {
        moniteur.lock();
        while (redacteur) { // tant que le rédacteur est en train d'écrire
            attente.await(); // on doit attendre
        }
        nbLecteur ++;
        moniteur.unlock();
        
    }

    public void terminerLecture() throws InterruptedException
    {
        moniteur.lock();
        nbLecteur --; // on veut juste arrêter le lecteur qui a demandé à arrêter de lire
        if (nbLecteur ==  0) {
            attente.signal(); // on va forcément réveiller un rédacteur comme il n'y plus de lecteur
        }
        moniteur.unlock();
    }

    public void demanderEcriture() throws InterruptedException
    {
        moniteur.lock(); 
        while (redacteur || nbLecteur > 0) { // tant qu'un rédacteur écrit et qu'il y a des lecteurs
            attente.await(); // on doit attendre
        } // il n'y plus de rédacteur ni de lecteur maintenant
        redacteur = true; // notre rédacteur peut écrire
        moniteur.unlock();

    }

    public void terminerEcriture() throws InterruptedException
    {
        moniteur.lock();
        redacteur = false; // le rédacteur arrête d'écrire
        attente.signalAll(); //réveiller tout le monde
        moniteur.unlock();
    }

    public String nomStrategie()
    {
        return "Stratégie: Sans Priorité.";
    }
}
