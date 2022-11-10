import java.net.*;
import java.util.Random;
import java.io.*;

public class LoadBalancer extends Thread {

    static String hosts[] = {"localhost", "localhost"};
    static int ports[] = {8081,8082};
    static int nbHosts = 2;
    static Random rand = new Random();

    private Socket client;
    public LoadBalancer(Socket accept) {
        this.client = accept;
    }

    public static void main (String[] args) throws IOException {
        ServerSocket s = new ServerSocket(Integer.parseInt(args[0])); // port du Serveur du Client
        while (true) { 
            new Thread(new LoadBalancer(s.accept())).start(); // création du thread léger
        }
    }

    public void printOneLine(String string, byte[] buffer) {
        //todo
    }
    public void run() {
        try {
            int i = rand.nextInt(nbHosts);

            // Récupérer le flux
            OutputStream clientOut = client.getOutputStream(); // sortie client, ce qui va vers le client depuis le LoadBalancer
            InputStream clientIn = client.getInputStream(); // entrée Client, ce qui provient du client

            // Lire la requête du client
            byte[] buffer = new byte[1024];
            int bytesRead;;
            bytesRead = clientIn.read(buffer);
            printOneLine("forward request", buffer);

            // Etablir la connexion avec le serveur
            Socket serveur = new Socket(hosts[i], ports[i]);
            OutputStream serveurOut = serveur.getOutputStream(); // sortie serveur, ce qui va vers le serveur depuis le LB
            InputStream serveurIn = serveur.getInputStream(); // entrée serveur, ce qui provient du serveur
            serveurOut.write(buffer, 0, bytesRead); // transfère le msg du LB au serveur

            // Transmettre
            bytesRead = serveurIn.read(buffer); // le serveur lis le msg
            String s = new String(buffer, 0, bytesRead);
            System.out.println("reçu:" + s);
            printOneLine("forward response", buffer);
            clientOut.write(buffer, 0, bytesRead); // la réponse du serveur est transmise au client
            
            // Fin de la communication
            clientOut.close(); 
            clientIn.close();
            serveurOut.close();
            serveurIn.close();
        
        } catch (IOException ex) {
			ex.printStackTrace();
		}
    }


}
