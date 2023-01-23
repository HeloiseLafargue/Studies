import java.io.File;
import java.io.FileOutputStream;
import java.lang.reflect.Method;

public class GeneratorStub {
    
    
    public static void newStub(String c) {

		try {
			// Introspection dans la classe "c"
			System.out.println("Création du stub de " + c );
			Class classe = Class.forName(c);
			String nomInterface = c + "_itf";
			Class itf = Class.forName(nomInterface);
			Method[] methodes = itf.getDeclaredMethods(); // récupération des méthodes de "c"

			////// Création du stub
			String classeStub = c + "_stub";

				// définition de la classe
				String fichier = "public class " + classeStub + " extends SharedObject implements " + nomInterface + ", java.io.Serializable {\n\n";
				// le constructeur
				fichier += "	public " + classeStub + "(int id, Object obj) {\n		super(id, obj);		\n}\n\n"; 
				
				// les méthodes
				for (Method m : methodes){
					String nomM = m.getName(); // nom de la méthode m
					System.out.println("Méthode créée : " + nomM);
					String typeM = m.getReturnType().getSimpleName(); // type de m
					
					fichier += "	public " + typeM + " " + nomM + "(";

					// les attributs des méthodes
					int nbArg = m.getParameterTypes().length;
					if (nbArg != 0) {
						for(int i=0; i<nbArg-1; i++) {
							Class type = m.getParameterTypes()[i]; // on récupère le type de l'argument i
							String typeArg = type.getSimpleName(); // on récupère le nom associé à ce type
							fichier += typeArg +" arg" + i + ", "; // on sépare tous les arguments par des virgules
						}
						Class type = m.getParameterTypes()[nbArg-1]; // on récupère le type du dernier argument
						String typeArg = type.getSimpleName(); // on récupère le nom associé à ce type
						fichier += typeArg +" arg"+ (nbArg-1) + ") {\n";
					}else {
						fichier += ") {\n";
					}

					// l'objet partagé
					fichier += "		" + c + " s = (" + c + ") obj ;\n";

					// si la méthode est void
					if (typeM.equals("void")){
						fichier += "		s." + nomM + "(";
					// si la méthode donne un retour
					} else {
						fichier += "		return s."+ nomM +"(";
					}
					
					if (nbArg != 0) {
						for(int i=0; i<nbArg-1; i++) {
							fichier += "arg" + i + ", ";
						}
						fichier += "arg" + (nbArg-1) + ");\n";
						fichier += "	}\n\n"; // fin de la méthode m
					}else {
						fichier += ");\n";
						fichier += "	}\n\n"; // fin de la méthode m
					}
				}
				fichier += "}"; // fin du fichier java

			// Création du fichier classeStub.java
			File fichierStub = new File(classeStub + ".java");
			fichierStub.createNewFile();
			FileOutputStream fileOS = new FileOutputStream(fichierStub);
			
			// Ecriture du fichier
			fileOS.write(fichier.getBytes());			
			fileOS.close();
			
		} catch (Exception e) { // ClassNotFoundException e 
			System.out.println(c + " n'est pas une classe existante ou l'interface " + c + "_itf n'existe pas ");
			e.printStackTrace();
		}
    }


	public static void main(String[] args) {
		try {
			newStub(args[0]);
		} catch (Exception e) {
			System.out.println("Erreur lors de la génération du stub");
			e.printStackTrace();
		}
	}

}