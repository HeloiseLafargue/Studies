import java.rmi.RemoteException;
import java.util.Vector;

public class ServerObject implements ServerObject_itf {

    /**
	 * Variables de classe
	 */
    public int id;
    public Object obj;
    public Client_itf writer;
    public Vector<Client_itf> readers; //vector : synchronisé
    public int lock; // 3 états du point de vue du serveur

    
    private final static int W = 0; //écriture
    private final static int R = 1; //lecture
    private final static int N = 2; //aucun verrou

    /**
     * Constructeur ServeurObject
     * @param id : identifiant de l'objet partagé
     * @param obj : objet partagé
     */
    public ServerObject(int id, Object obj) { //Objet pour les méthode de server
        this.id = id;
        this.obj = obj;
        this.writer = null; // pas d'écrivains au début
        this.readers = new Vector<Client_itf>(); // pas de lecteurs au début, Vector est synchronisé

        this.lock = N; // aucun verrou
    }

 
    // Protocole de cohérence (ie gestion des verrous)

    /**
	 * Demande de verrou en lecture sur l'objet partagé
	 * @param client le client à l'origine de la requête
	 * @return Object, l'objet partagé dont l'état a été mis à jour 
	 */
    @Override
    public Object lock_read(Client_itf client) {
		synchronized (this) {
        try {
            switch (this.lock) {
            case W :
                this.obj = writer.reduce_lock(this.id); // réclamation du verrou lecture, gestion de l'attente par le SharedObject
                
                // Passage en mode lecture
                this.readers.add(this.writer);
                this.writer = null; // suppression de l'écrivain
                this.readers.add(client); // ajout du lecteur
                this.lock = R; // verrou en Reader
                break;

            case N :
                // Passage en mode lecture
                this.readers.add(client); // ajout du lecteur
                this.lock = R; // verrou en Reader
                break;

            case R :
                // Passage en mode lecture
                this.readers.add(client); // ajout du lecteur
                this.lock = R; // verrou en Reader
                break;

            default: 
                break;
            }
        
        } catch (RemoteException e) {
            e.printStackTrace();
            System.out.println("Erreur lors de la demande de verrou en lecture au serveur objet");
        }
        return this.obj;
        }
    }

    /**
	 * Demande de verrou en écriture sur l'objet partagé
	 * @param client le client à l'origine de la requête
	 * @return Object, l'objet partagé dont l'état a été mis à jour 
	 */
    @Override
    public Object lock_write(Client_itf client) {
		synchronized (this) {
        try {
            switch (this.lock) {
            case R :
                // Suppression du client à l'écriture
                this.readers.remove(client); 
                // Suppression de tous les lecteurs sur l'objet partagé        
                for (Client_itf c : this.readers) {
                    try {               
                        c.invalidate_reader(this.id); // réclamation de l'invalidation du verrou lecture à tous les lecteurs
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
                this.readers.clear(); // on vide la collection de lecteurs
                // Passage en mode écriture
                this.writer = client; // ajout de l'écrivain
                this.lock = W; // verrou en Writer
                break;

            case N :
                // Passage en mode écriture
                this.writer = client; // ajout de l'écrivain
                this.lock = W; // verrou en Writer
                
            case W: 
                try {
                    this.obj = this.writer.invalidate_writer(this.id); // réclamation de l'invalidation du verrou écriture
                } catch (Exception e) {
                    e.printStackTrace();
                }
                // Passage en mode écriture
                this.writer = client; // ajout de l'écrivain
                this.lock = W; // verrou en Writer
                break;
            
            default:
                break;
            }
        
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("Erreur lors de la demande de verrou en ecriture au serveur objet");
        }
        return this.obj;
    }
    }
   
}
