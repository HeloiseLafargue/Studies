// Time-stamp: <08 Apr 2008 11:35 queinnec@enseeiht.fr>

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import Synchro.Assert;

/** Lecteurs/rédacteurs
 * stratégie d'ordonnancement: priorité aux rédacteurs,
 * implantation: avec un moniteur. */
public class LectRed_PrioRedacteur implements LectRed
{
    private Lock moniteur;

    // pour prioriser
    private Condition attenteRed; // file d'attente des rédacteurs
    private Condition attenteLec; // file d'attente des lecteurs
    private int nbRedAttente; // nombre de rédacteurs en attente

    private int nbLecteur; // nombre de lecteurs en train de lire
    private boolean redacteur;

    public LectRed_PrioRedacteur()
    {
        moniteur = new java.util.concurrent.locks.ReentrantLock();
        attenteRed = moniteur.newCondition();
        attenteLec =  moniteur.newCondition();
        nbRedAttente = 0;
        nbLecteur = 0;
        redacteur = false;
    }

    public void demanderLecture() throws InterruptedException
    {
        moniteur.lock();
        while (redacteur || nbRedAttente > 0) { // tant que qu'un rédacteur est en train d'écrire ou qu'il y en a qui veulent écrire
            attenteLec.await(); // le lecteur doit attendre
        }
        nbLecteur ++; 
        if (!redacteur && nbRedAttente == 0) { // si aucun rédacteur écrit ou veut écrire
            attenteLec.signal();
        }
        moniteur.unlock();
        
    }

    public void terminerLecture() throws InterruptedException
    {
        moniteur.lock();
        nbLecteur --; // on veut juste arrêter le lecteur qui a demandé à arrêter de lire            
        attenteRed.signal(); // on va forcément réveiller un rédacteur comme il n'y plus de lecteur
        moniteur.unlock();
    }

    public void demanderEcriture() throws InterruptedException
    {
        moniteur.lock(); 
        nbRedAttente ++;
        while (redacteur || nbLecteur > 0) { // tant qu'un rédacteur écrit et qu'il y a des lecteurs
            attenteRed.await(); // on doit mettre que les rédacteurs dans la file d'attente
        } // il n'y plus de rédacteur ni de lecteur maintenant
        redacteur = true; // notre rédacteur écrit
        nbRedAttente --; // il ne demande plus à écrire
        attenteRed.signal(); // tous les autres rédacteurs peuvent demander d'écrire
        moniteur.unlock();
    }

    public void terminerEcriture() throws InterruptedException
    {
        moniteur.lock();
        redacteur = false; // le rédacteur arrête d'écrire
        if (nbRedAttente > 0 ) { // pour prioriser
            attenteRed.signal();
        } else { 
            attenteLec.signal();
        }
        moniteur.unlock();
    }

    public String nomStrategie()
    {
        return "Stratégie: Priorité Rédacteurs.";
    }
}
