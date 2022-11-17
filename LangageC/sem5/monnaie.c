#include <stdlib.h> 
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

// Definition du type monnaie

struct monnaie {
    int valeur;
    char devise;
};

/**
 * \brief Initialiser une monnaie 
 * \param[]
 * \pre 
 */ 
 
void initialiser(int valeur, char devise){
    assert (valeur > 0);
    struct monnaie monnaie_i = {valeur, devise};  
}


/**
 * \brief Ajouter une monnaie m2 à une monnaie m1 
 * \param[]
 * // TODO
 */ 
 
bool ajouter(struct monnaie m1, struct monnaie m2){

    if (m1.devise == m2.devise) {
        m2.valeur += m1.valeur;
        return true;
    } else {
        return false;
    }        
}


/**
 * \brief Tester Initialiser 
 * \param[]
 * // TODO
 */ 
void tester_initialiser(){

    struct monnaie monnaie_i = {1, 'e'};
    
    //Cas monnaie à valeur négative
    struct monnaie monnaie_1 = {-2, '$'};
    initialiser(-2, '$');
    if ((monnaie_i.valeur == 1) && (monnaie_i.devise == 'e')) {
        printf ("L'initialisation n'est pas effectuée");
    }
    
    // Cas monnaie à devise non char
    struct monnaie monnaie_2 = {2, 3};
    initialiser(2, 3);
    if ((monnaie_i.valeur == 1) && (monnaie_i.devise == 'e')) {
        printf ("L'initialisation n'est pas effectuée");
    }     
    // Cas monnaie bien définie
    struct monnaie monnaie_3 = {2, '$'};
    initialiser(2, '$');
    if ((monnaie_i.valeur == monnaie_3.valeur) && (monnaie_i.devise == monnaie_3.devise)) {
        printf ("L'initialisation est effectuée");
    }
        
}

/**
 * \brief Tester Ajouter 
 * \param[]
 * // TODO
 */ 
void tester_ajouter(){
    
    // Cas de deux monnaies de la même devise, avec valeurs positives
    struct monnaie monnaie_1 = {2, '$'};
    struct monnaie monnaie_2 = {4, '$'};
    bool b = ajouter(monnaie_1, monnaie_2);
    if (b == true && monnaie_2.valeur == 6) {
        printf ("L'opération a eu lieu : %d\n", b);    
        printf ("Valeur de monnaie_2 : %d", monnaie_2.valeur);
        printf("La valeur a bien été ajoutée");
    }

    // Cas de deux monnaies de devises différentes
    monnaie_1.valeur, monnaie_1.devise = 2, 'e';
    monnaie_2.valeur, monnaie_2.devise = 4, '$';
    b = ajouter(monnaie_1, monnaie_2);
    if (b == false) {
        printf ("L'opération a eu lieu : %d\n", b); 
        printf("La valeur n'a pas été ajoutée");
    }
}



// B2.5

struct monnaie choix_utilisateur()
{
    int valeur = 0;
    printf("Valeur de la monnaie ? \n");
    scanf("%d \n", &valeur);
    char devise = 'i';
    printf("Devisee de la monnaie ? \n");
    scanf("%c \n", &devise);
    struct monnaie monnaie1 = {valeur, devise};
    return monnaie1;
}

int n = 5;

int main(void){
    // Un tableau de 5 monnaies
    typedef struct monnaie t_tab[n];
    t_tab porte_monnaie;

    //Initialiser les monnaies
    struct monnaie monnaie_k;
    for(int k = 0 ; k<n ; ++k) {
        monnaie_k = choix_utilisateur();
        porte_monnaie[k].valeur = monnaie_k.valeur;
        porte_monnaie[k].devise = monnaie_k.devise;        
    }
 
    // Afficher la somme de toutes les monnaies qui sont dans une devise entrée par l'utilisateur.
    char devise_utilisateur = 'i';
    printf("Choix devise pour la somme ? \n");
    scanf("%c", &devise_utilisateur);
    
    int s = 0;
    for(int k = 0 ; k<n ; ++k) {
        if (porte_monnaie[k].devise == devise_utilisateur) {
            s += porte_monnaie[k].valeur;
        }  
    }
    
    printf ("La somme est : %d %c.", s, devise_utilisateur);

    return EXIT_SUCCESS;
}
