import java.rmi.*;
import java.rmi.server.UnicastRemoteObject;
import java.rmi.registry.*;
import java.net.*;
import java.util.HashMap;

public class Client extends UnicastRemoteObject implements Client_itf {

	/**
	 * Variables statiques
	 */
	private static final long serialVersionUID = 5838449924074267286L; //suggéré par eclipse
	private static Server_itf server; // la mémoire n'est allouée qu'une seule fois et non à chaque fois qu'une classe est instanciée
	private static HashMap<Integer, SharedObject> sharedObj; //objets utilisés localement sans informer le serveur
	private static Client_itf client = null;

	/** 
     * Constructeur du Client
     */
	public Client() throws RemoteException {
		super();
		sharedObj = new HashMap<Integer, SharedObject>();
	}


///////////////////////////////////////////////////
//         Interface to be used by applications
///////////////////////////////////////////////////

	// initialization of the client layer
	public static void init() {
		int port;
    	String URL;
		try {
				// détermination de l'url du server, la même que dans la classe serveur
				port = Registry.REGISTRY_PORT; //port utilisable par le serveur
				URL = "//"+InetAddress.getLocalHost().getHostName()+":"+port+"/server";
				// création d'une instance du client 
				Client.client = new Client();
				Client.server = (Server_itf) Naming.lookup(URL); //récupération du serveur		

		} catch (Exception exc) {
				exc.printStackTrace();
				System.out.println("Erreur lors de l'initialisation du client");
		}			
	}

	// create a stub
	public static SharedObject create_stub(int id, Object obj) {
		SharedObject so = null;
		try {
			Class<?> classe_stub = Class.forName(obj.getClass().getName() + "_stub"); // nouvelle classe avec stub à partir de la classe de l'objet
			java.lang.reflect.Constructor<?> constructeur = classe_stub.getConstructor(new Class[] {int.class, Object.class}); // le constructeur
			so = (SharedObject) constructeur.newInstance(new Object[] {id, obj}); // création du stub avec l'objet et son id
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Erreur lors de la création du stub");
		}
		return so;
	}
	
	// Service de nommage
	
	//consulte le serveur de nom et retourne l’objet partagé enregistré.	
	public static SharedObject lookup(String name) {
		SharedObject so = null;
		try {
			int id = server.lookup(name); //récupération de l’objet partagé enregistré
			//création de l'objet partagé trouvé dans le serveur de nom
			if (id >= 0) {
				Object obj = lock_read(id); //récupération du verrou de l'objet en lecture
				// Etape 1
				//so = new SharedObject(id, obj); 
				// Etape 2
				so = create_stub(id,obj);
				so.unlock(); //l'objet n'est plus utilisé on rend le verrou
				sharedObj.put(id, so); //on ajoute l'objet partagé à la liste des objets partagés utilisés localement
			} //sinon retourne null
		}catch (Exception e) {
			e.printStackTrace();
			System.out.println("Erreur lors de la consultation du serveur par le client");
		}
		return so;
	}		
	
	// enregistre un objet partagé dans le serveur de noms
	public static void register(String name, SharedObject_itf so) {
		try {
			SharedObject sobj =(SharedObject) so; //utile pour récupérer l'identifiant de l'objet
			server.register(name, sobj.getId()); // enregistre un objet partagé dans le serveur de noms
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Erreur lors de l'enregistrement dans le serveur par le client");
		}
	}

	// creation of a shared object
	public static SharedObject create(Object o) {
		SharedObject so = null;
		try {
			int id = server.create(o); //propagation au serveur qui renvoie un identifiant unique
			// Etape 1
			//so = new SharedObject (id, o); 
			// Etape 2
			so = create_stub(id,o);
			sharedObj.put(id, so); //on ajoute l'objet partagé à la liste des objets partagés utilisés localement
		} catch (RemoteException e) {
			e.printStackTrace();
			System.out.println("Erreur lors de la création de l'objet par le client");
		} return so;	
	}
	
/////////////////////////////////////////////////////////////
//    Interface to be used by the consistency protocol
////////////////////////////////////////////////////////////

	// request a read lock from the server
	public static Object lock_read(int id) {
		Object obj = null;
		try {
			obj = server.lock_read(id,client); //propagation de la demande de verrou au serveur 
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Erreur lors de la demande de verrou du client au server");
		} return obj;
	}

	// request a write lock from the server
	public static Object lock_write(int id) {
		Object obj = null;
		try {
			obj = server.lock_write(id,client); //propagation de la demande de verrou au serveur 
		} catch (Exception e) {
			e.printStackTrace();
			System.out.println("Erreur lors de la demande de verrou du client au serveur");
		} return obj;
	}

	// receive a lock reduction request from the server
	public Object reduce_lock(int id) throws java.rmi.RemoteException {
		//réclamation du passage du verrou de l’écriture à la lecture, propagation au sharedObject
		return sharedObj.get(id).reduce_lock();
	}


	// receive a reader invalidation request from the server
	public void invalidate_reader(int id) throws java.rmi.RemoteException {
		//réclamation l’invalidation d’un lecteur, propagation au sharedObject
		sharedObj.get(id).invalidate_reader();
	}


	// receive a writer invalidation request from the server
	public Object invalidate_writer(int id) throws java.rmi.RemoteException {
		//réclamation l’invalidation d’un écrivain, propagation au sharedObject
		return sharedObj.get(id).invalidate_writer();
	}
}
