import java.io.*;

public class SharedObject implements Serializable, SharedObject_itf {

	/**
	 * Variables 
	 */
	private static final long serialVersionUID = -7232473551212143113L; //suggéré par Eclipse
	public int id; // id unique par rapport au serveur
	public Object obj; // pointe sur l'objet effectif
	public int lock; // état du verrouillage de l’objet sur le site
	private boolean waiting; // booléen pour signifier qu'un processus est en attente			

	private final static int NL = 0; // NL : no local lock
    private final static int RLC = 1; // RLC : read lock cached (not taken)
    private final static int WLC = 2; // WLC : write lock cached
    private final static int RLT = 3; // RLT : read lock taken
    private final static int WLT = 4; // WLT : write lock taken
    private final static int RLT_WLC = 5; // RLT_WLC : read lock taken and write lock cached

	/**
	 * Constructeur SharedObject
	 * @param id  l'identifiant du SharedObject
	 * @param object l'object
	 */
	public SharedObject(int id, Object obj) {
		super();
		this.id = id;
		this.obj = obj;
		this.waiting = false;
		this.lock = NL; //aucun verrou
	}

	// retourne l'identifiant de l'objet, utilisé par le client
	public int getId() {
		return this.id;
	}

	// invoked by the user program on the client node
	public void lock_read() {

		synchronized (this) {

			// attente tant que le verrou n'est pas libéré
			while (this.waiting) {
				try {
					this.wait();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			switch (this.lock) {
				case RLC :
					this.lock = RLT; // verrou en lecture sans informer le serveur 
					break;
				
				case WLC :
					this.lock = RLT_WLC; // verrou pris en lecture sans informer le serveur    
					break; 
					
				case NL :
					this.obj = Client.lock_read(this.id); // on demande le verrou
					this.lock = RLT; // verrou pris en lecture
					break;
					
				default: 
					break;
			}
		}
	}

	// invoked by the user program on the client node
	public void lock_write() {

		synchronized (this) {

			// attente tant que le verrou n'est pas libéré
			while (this.waiting) {
				try {
					this.wait();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			switch (this.lock) {
				case RLC :        
					this.obj = Client.lock_write(this.id); // on demande le verrou
					this.lock = WLT; // verrou en écriture sans informer le serveur 
					break;
				
				case WLC :
					this.lock = WLT; // verrou pris en écriture sans informer le serveur  
					break; 
					
				case NL :
					this.obj = Client.lock_write(this.id); // on demande le verrou
					this.lock = WLT; // verrou pris en lecture
					break;
					
				default:
					break;
			}   
		}
	}

	// invoked by the user program on the client node
	public void unlock() {

		synchronized (this) {

		switch (this.lock) {
			case WLT:
				this.lock = WLC; // WLT => WLC
				break;

			case RLT:
				this.lock = RLC; // RLT => RLC
				break;
	
			case RLT_WLC: // RLT_WLC => WLC
				this.lock = WLC;
				break;
	
			default:
				break;
			}

			// On prévient que le verrou est rendu
			try {
				this.notify();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}


	// callback invoked remotely by the server
	public Object reduce_lock() {

		synchronized (this) {

		// Blocage : interdire d'autres lock le temps que le lock soit réduit
		this.waiting = true;
		switch (this.lock) {
			case WLT: 
				// le verrou est tenu donc il faut attendre
				while (this.lock == WLT) {
					try {
						this.wait(); // réclamation si le verrou est tenu
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				this.lock = RLC; // WLT => RLC
				break;
			
			case WLC: 
				// le verrou est caché donc pas d'attente
				this.lock = RLC; // WLC => RLC
				break;
	
			case RLT_WLC: 
				// le verrou RLT est tenu et le reste, et le verrou WLC est caché donc pas d'attente
				this.lock = RLT; // RLT_WLC => RLT
				break;

			default:
				break;
			}

		// On prévient que le verrou est rendu
		this.waiting = false;
		try {
			this.notify();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// On retourne l'objet dont l'état a été mis à jour
		return obj;
	}
	}

	// callback invoked remotely by the server
	public void invalidate_reader() {

		synchronized (this) {

		// Blocage : interdire d'autres lock le temps que le lock soit réduit
		this.waiting = true;
		switch (this.lock) {
			case RLT: 
				// le verrou est tenu donc il faut attendre
				while (this.lock == RLT) {
					try {
						this.wait(); // réclamation si le verrou est tenu
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				this.lock = NL; // RLT => NL
				break;

			case RLC:
				// le verrou est caché donc pas d'attente
				this.lock = NL; // RLC => NL
				break;

			default:
				break;
		}
		// On prévient que le verrou est rendu
		this.waiting = false;
		try {
			this.notify();
		} catch (Exception e) {
			e.printStackTrace();
		}
		}
	}

	public Object invalidate_writer() {

		synchronized (this) {
			
		// Blocage : interdire d'autres lock le temps que le lock soit réduit
		this.waiting = true;
		switch (this.lock) {
			case WLC: 
				// le verrou est caché donc pas d'attente
				this.lock = NL; // WLC => NL
				break;

			case WLT: 
				// le verrou est tenu donc il faut attendre
				while (this.lock == WLT) {
					try {
						this.wait(); // réclamation si le verrou est tenu
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				this.lock = NL; // WLT => NL
				break;

			case RLT_WLC: 
				// le verrou RLT est tenu et doit être relaché donc il faut attendre
				while (lock == RLT_WLC) {
					try {
						this.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				this.lock = NL; // RLT_WLC => NL
				break;

			default:
				break;
		}
		// On prévient que le verrou est rendu
		this.waiting = false;
		try {
			this.notify();
		} catch (Exception e) {
			e.printStackTrace();
		}
		// On retourne l'objet dont l'état a été mis à jour
		return obj;
	}
	}
}
