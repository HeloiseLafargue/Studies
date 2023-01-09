# Projet d'optimisation numérique

Pour récupérer les sources, il faut cloner ce dépot git : 

```bash
git clone https://gitlab.irit.fr/toc/mathn7/optimisation-numerique/projet-optinum.git
```

Les sujets liés au projet se trouvent dans les notebooks du répertoire `src`. Voici l'ordre des sujets :

* Newton
* Régions de confiance
* Lagrangien augmenté

Pour réaliser le projet vous aurez besoin de cette [documentation](doc-projet.pdf) qui décrit les différents algorithmes à implémenter. 

**Attention**, seuls les fichiers dans le répertoire `src` sont à modifier !

## Utilisation de `julia`dans les salles d'enseignement
* Pour lancer `julia` sur les machines enseignement il faut dans votre fichier `.bashrc` initialiser la variable d'environnement de `julia` `JULIA_DEPOT_PATH` et ajouter à la variable `PATH` l'endroit où se trouve `julia`. Il faut pour cela ajouter les lignes suivantes : 
`JULIA_DEPOT_PATH="/mnt/n7fs/cimi/.julia"`  
`export JULIA_DEPOT_PATH`  
`export PATH="/mnt/n7fs/cimi/julia-1.8.2/bin":$PATH`

* Pour pouvoir utiliser les notebooks, il faudra alors taper la commande ` jupyter-notebook`

