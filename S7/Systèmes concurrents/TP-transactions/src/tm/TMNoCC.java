package tm;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;

// Mémoire transactionnelle 
//		- sans contrôle de concurrence ;
//		- avec propagation directe des écritures et journal avant.
public class TMNoCC extends AbstractTM {

    // Map qui associe chaque transaction à son journal des valeurs avant.
    private Map<String,Map<String,Integer>> oldVals;

    public TMNoCC(Map<String,Integer> t_objects,
                  Set<String> transactions) {

        super(t_objects, transactions);

        this.oldVals = new HashMap<String,Map<String,Integer>>();
        for (String name : this.transactions) {
            this.oldVals.put(name, new HashMap<String,Integer>());
        }
    }

    public TMNoCC(Map<String,Integer> t_objects) {
        this(t_objects, new HashSet<String>());
    }

    // Nettoie de la mémoire transactionnelle les valeurs liées à la transaction en argument.
    private void remove(String transaction) {

        this.transactions.remove(transaction);
        this.oldVals.remove(transaction);
    }

    public boolean newTransaction(AbstractTransaction transaction) {

        if(this.transactions.contains(transaction.getName())) {
            return false;
        }

        this.transactions.add(transaction.getName());
        this.oldVals.put(transaction.getName(), new HashMap<String,Integer>());
        (new Thread(transaction)).start();
        return true;
    }

    public int read(String transaction, String t_object) throws AbortException {

        if (!oldVals.get(transaction).containsKey(t_object)) {
            this.oldVals.get(transaction)
                        .put(t_object,this.t_objects.get(t_object));
        }

        return this.t_objects.get(t_object);
    }


    public void write(String transaction,
                      String t_object,
                      int value) throws AbortException {

        if (!oldVals.get(transaction).containsKey(t_object)) {
            this.oldVals.get(transaction)
                        .put(t_object,this.t_objects.get(t_object));
        }

        this.t_objects.put(t_object, value);
    }


    public void abort(String transaction) throws AbortException {

        System.out.println("Annulation des modifications de "+transaction+".");

        for (String t_object : this.oldVals.get(transaction).keySet()) {
            this.t_objects.put(t_object, oldVals.get(transaction)
                                                .get(t_object));
        }

        System.out.println("Annulation terminée pour "+transaction+".");

        this.remove(transaction);

        throw new AbortException();
    }

    public void commit(String transaction) throws AbortException {

        this.remove(transaction);
    }

}