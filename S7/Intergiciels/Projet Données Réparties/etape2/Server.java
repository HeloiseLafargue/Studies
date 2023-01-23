import java.net.InetAddress;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;

public class Server extends UnicastRemoteObject implements Server_itf {

    /**
	 * Variables statiques
	 */
    private static final long serialVersionUID = -888183490414628873L; //suggéré par Eclipse
	private static HashMap<String, Integer> carnet; // Serveur de nom : nom et id de l'objet
    private static HashMap<Integer, ServerObject> serverObject; // Serveur object : id de l'objet et serveur object
    private static int compteur = 1; // compteur pour l'id unique des objets
    
    /** 
     * Constructeur du Serveur
     */
    public Server() throws RemoteException {
        super();
        carnet = new HashMap<String, Integer>();
        serverObject = new HashMap<Integer, ServerObject>();
    }
    
    //Service de nommage
    
    @Override
    // consulte le serveur de nom et retourne l'identifiant de l’objet pour récupérer l'objet dans le Client
    public int lookup(String name) throws RemoteException {        
        Integer id = carnet.get(name);
        if (id != null) { // vérifier que l'objet est dans le serveur de noms
            return (int) id;
        }
        else {
            return -1;
        }
    }

    @Override
    // enregistre un objet partagé dans le serveur de noms
    public void register(String name, int id) throws RemoteException {
        carnet.put(name, id);
    }

    @Override
    // Création d'un serveur objet pour créer un objet partagé
    public int create(Object o) throws RemoteException {
        int id_obj = compteur;
        ServerObject serv_obj = new ServerObject(compteur,o); // id unique grâce au compteur
        serverObject.put(compteur, serv_obj); // ajout le serveur objets
        compteur ++;
        return id_obj;
    }
    
    // lock

    @Override
    // Demande de verrou lecture
    public Object lock_read(int id, Client_itf client) throws RemoteException {
        ServerObject serv_obj = serverObject.get(id); // localiser le serveur
        return serv_obj.lock_read(client); // propagation de la demande de verrou au serveur objet
    }

    @Override
    // Demande de verrou écriture
    public Object lock_write(int id, Client_itf client) throws RemoteException {
        ServerObject serv_obj = serverObject.get(id); // localiser le serveur
        return serv_obj.lock_write(client); // propagation de la demande de verrou au serveur objet
    }

   

    public static void main(String args[]) {
        int port;
        String URL;
               
        try {
        	port = Registry.REGISTRY_PORT;
            // lancement du service naming
            Registry registry = LocateRegistry.createRegistry(port);
            // création d'une instance du server
            Server server = new Server();
            // détermination de l'url du server
            URL = "//"+InetAddress.getLocalHost().getHostName()+":"+port+"/server";
            Naming.rebind(URL, server);    
        } catch (Exception exc) {
        	exc.printStackTrace();
            System.out.println("Erreur lors de l'initialisation du server");
        }
        System.out.println("Serveur initialisé");
        }
    
    
    
}
