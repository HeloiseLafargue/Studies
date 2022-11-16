import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.rmi.Naming;


public class CarnetImpl extends UnicastRemoteObject implements Carnet {
    
    HashMap<String, RFiche> carnet = new HashMap<>();
    CarnetImpl carnet2;

    public CarnetImpl(CarnetImpl carnet2) throws RemoteException {
        this.carnet2 = carnet2;
    }

    @Override
    public void Ajouter(SFiche sf) throws RemoteException {
        carnet.put(sf.getNom(), new RFicheImpl(sf.getNom(), sf.getEmail()));
    }

    @Override
	public RFiche Consulter(String n, boolean forward) throws RemoteException {
        RFiche rf = carnet.get(n);
        if (rf == null & forward) {
            if (carnet2 == null) {
                carnet2 = (CarnetImpl) Naming.lookup("//localhost:4000/carnet",2);
            }
            rf = carnet2.Consulter(n, false);
        }
        return rf;
    }

    public static void main(String[] args) {
        try {
            LocateRegistry.createRegistry(8080);
            Carnet c1 = new CarnetImpl(c2);
            Naming.bind("//localhost:8080/carnet", c);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
