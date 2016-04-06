#include <R.h>
#include <Rmath.h>

/* Note : le code contient des Rprintf en commentaires. Ceux-ci servent à débugger le code au besoin

Exemples de commandes pour afficher dans la console R
   for (j=0; j < *L-1; j++) Rprintf("%d  ",pbh[j]); Rprintf("%d  ",isup ); Rprintf("\n");
   Rprintf("%f  ",TY ); Rprintf("\n");
   
%d = entier (integer), %f = réel (double)  */


/* Définition des paramètres communs à plusieurs fonctions 

Un nom de paramètre représente toujours la même chose.
Deux suffixes reviennent à quelques reprises :
...noc : no certainty stratum (n'inclut pas la strate certain)
...nonint : non integer (non entier)

## arguments en lien avec les données et créés en R :

@param xnoc observations de la variable de stratification données en entrée, exlcuant les  
            observations pour la certainty stratum
@param Nnoc nombre total d'observations après avoir retiré les observations pour la certainty stratum
            (longueur de xnoc et de stratumIDnoc) 
@param x1noc vecteur des valeurs uniques, ordonnées de la plus petite à la plus grande, 
          dans les observations de la variable de stratification données en entrée, exlcuant les  
            observations pour la certainty stratum (xnoc)
@param N1noc nombre de valeurs uniques dans les observations xnoc (longueur de x1noc)
@param Nc le nombre d'observations dans la strate certain
@param EYc l'espérance anticipée de Y dans la strate certain
@param EX moyenne de toutes les observations, peu importe leur strate, incluant les obs de la strate certain
          (utile au modèle random)
@param EX2 moyenne de toutes les observations au carré, peu importe leur strate, incluant les obs de la strate certain
           (utile au modèle random)

## arguments qui sont en fait des arguments données en entrée à une
   fonction R publique (et possiblement formaté) :
   
@param bhfull les bornes des strates, incluant b0 et bL (vecteur de longueur L + 1)
@param L nombre total de strates (ne compte pas la strate certain) 
         -> L doit être >=2 pour assurer le bon fonctionnement des fonctions
@param takenone le nombre de strate take-none : 0 ou 1
@param takeall le nombre de strate take-all : 0, 1, ou plus rarement un nombre entier > 1
@param q1 le premier exposant définissant l'allocation
@param q2 le deuxième exposant définissant l'allocation
@param q3 le troisième exposant définissant l'allocation
@param findn indicatrice : 1 si on a un CV cible, 0 si on  un n cible
@param n le n cible donné en entrée
@param CV RRMSE cible donné en entrée à une fonction externe
@param rhL taux de réponse : vecteur de longueur L dont la première valeur est 1 s'il y a une strate takenone,
           donc qui n'est pas nécessairement identique au rh donné en entrée à une fonction externe.
           (La valeur 1 est sans importance car elle n'est pas utilisée. C'est la position des autres valeurs 
            qui est importante.) 
@param biaspenalty argument donné en entrée à une fonction externe
@param nmodelID un identifiant du modèle 0 = none, 1 = loglinear, 2 = linear, 3 =  random
@param beta
@param sig2
@param ps 
@param ph vecteur de longueur L pour les taux de mortalité dans les L strates 
@param gamma
@param epsilon
@param minNh paramètre de l'algo donné en entrée à la fonction externe : 
             valeur minimale de Nh acceptée pour les strates échantillonnées (take-some et take-all)

## arguments pour des fonctions C internes

@param xs le vecteur double des observations dans une state spécifique (notée strate s)
@param Ns le nombre d'observations dans la strate s (longueur de xs)
@param nhcalcul les nh choisis pour faire le calcul, parfois des réels, d'autres fois des entiers

## arguments qui sont obtenus d'une fonction C

@param stratumIDnoc tel que créé par get_stratumIDnoc_C
@param Nh vecteur de longueur L des tailles de populations obtenu de get_Nh_C
@param VYh la variance anticipée de Y dans chaque strate obtenue de get_momentY_C
@param ah vecteur de longueur L : ah pour les strates take-some, 0 pour les autres strates, obtenu de get_ah_C
@param TCN la somme des tailles de populations Nh pour les strates take-all (C), obtenue de get_nnonint_C
@param nhnonint un vecteur de L nombres réels : les tailles d'échantillon dans les L strates, 
                issues de l'application de la règle d'allocation, obtenu de get_nhnonint_C                
@param TAY la somme anticipée de Y dans les strates take-none, obtenue de get_momentY_C

*/

/* Définition des valeurs retournées (ou plutôt modifiées) communes à plusieurs fonctions

@return stratumIDnoc un vecteur de longueur Nnoc contenant les chiffres 1 à L, identifiant la strate
                     à laquelle chaque observation appartient
@return Nh vecteur de longueur L des tailles de populations
@return EYh Un vecteur de longueur L : l'espérance anticipée de Y dans chaque strate
@return VYh Un vecteur de longueur L : la variance anticipée de Y dans chaque strate
@return TY Un nombre de type double : la somme gloable anticipée de Y (incluant la certainty stratum)
@return nhnonint un vecteur de L nombre réel : les tailles d'échantillon dans les L strates, 
                        issues de l'application de la règle d'allocation
@return nh les tailles d'echantillon entieres pour les L strates = nhnonint arrondies.
*/

/* ******************************************************************************************** */

/* Détermination des strates en ne considérant pas la strate certain

Pour chaque observation, cette fonction identifie sa strate.

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

Aucune valeur retournée (ou modifiée) n'est unique à cette fonction, 
voir la description générale des sorties au début de ce fichier.
  
Créée et vérifiée le 26 septembre 2012
voir wrapper R
*/
void get_stratumIDnoc_C (double *xnoc, int *Nnoc, double *bhfull, int *L,
                      /* Sortie */ int *stratumIDnoc)
{
  int i, j;
  for (i=0; i < *Nnoc; i++) {
		for (j=0; j < *L; j++) {
			if ( (xnoc[i] >= bhfull[j]) && (xnoc[i] < bhfull[j+1]) ) {
				stratumIDnoc[i] = j + 1;
			}
		}
	}
  /* La fonction retourne un résultat même si bhfull est illogique (voir testDevel.R) */
}

/* ******************************************************************************************** */

/* Détermination des tailles de population dans les strates (sans considérer la strate certain)

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

Aucune valeur retournée (ou modifiée) n'est unique à cette fonction, 
voir la description générale des sorties au début de ce fichier.

Créée et vérifiée le 26 septembre 2012
voir wrapper R
*/
void get_Nh_C (int *stratumIDnoc, int *Nnoc, int *L,
                /* Sortie */ int *Nh)
{
  int i, j;
  
  for (j=0; j < *L; j++) Nh[j] = 0;
  
  for (i=0; i < *Nnoc; i++) {
  	for (j=0; j < *L; j++) {
			if ( stratumIDnoc[i] == j + 1 ) {
				Nh[j] = Nh[j] + 1;
			}
		}
	}
}

/* ******************************************************************************************** */

/* Fonction qui extrait les observations d'une strate

@param nstatum le numéro de la strate que l'on souhaite extraire
voir la description générale au début de ce fichier pour les autres paramètres.

@return xs observations pour la strate nstratum

Créée et vérifiée avec valeurs par défaut le 27 septembre 2012
pas de wrapper R, fonction C seulement
*/
void extract_stratum_C (int *nstratum, double *xnoc, int *stratumIDnoc, int *Nnoc,
                         /* Sortie */ double *xs)
{
  int i, j=0;
  
  for (i=0; i < *Nnoc; i++) {
    if (stratumIDnoc[i] == *nstratum) {
      xs[j] = xnoc[i];
      j = j + 1;
    }
  }
}

/* ******************************************************************************************** */

/* Obtention de E[Y] pour une strate spécifique s

Calcul l'espérance anticipée de Y pour les observations d'une seule strate, la state s.

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

@return \item{EYs}{Un nombre de type double : l'espérance anticipée de Y pour les observations de la state s}
@return \item{EXs}{Un nombre de type double : l'espérance de X pour les observations de la state s 
                   (uniquement utile à get_VYs_C)}
@return \item{phis}{Un nombre de type double : la valeur de phi pour la state s 
                    (utile à get_VYs_C et get_momentY_C)}
                    
Précondition : Ns est positif

Créée et vérifiée avec valeurs par défaut le 26 septembre 2012
voir wrapper R
*/
void get_EYs_C (double *xs, int *Ns, int *nmodel, 
                  double *beta, double *sig2, double *ps, double *gamma, double *epsilon, double *EX,
                  /* Sortie */ double *EYs, double *EXs, double *phis)
{
  int i;  
  
  /* Première étape : sommes */
  *EXs = *phis = 0;
  for (i = 0; i < *Ns; i++){ 
    if (*nmodel == 1) {
      *phis = *phis + R_pow(xs[i], *beta);
    } else {
      *EXs = *EXs + xs[i] / *Ns;
    }
  }
  
  /* Deuxième étape : multiplications ou divisions */
  if (*nmodel == 1) {
    *EYs = *ps * *phis / *Ns;
  } else {
    if (*nmodel == 0) *EYs = *EXs;
    if (*nmodel == 2) *EYs = *beta * *EXs;
    if (*nmodel == 3) *EYs = (1 - *epsilon) * *EXs + *epsilon * *EX;
  }
}

/* ******************************************************************************************** */

/* Obtention de Var[Y] pour une strate spécifique s

Calcul la variance anticipée de Y pour les observations d'une seule strate, la state s.

@param EYs l'espérance anticipée de Y pour les observations de la state s obtenue de get_EYs_C 
@param EXs l'espérance de X pour les observations de la state s obtenue de get_EYs_C
@param phis la valeur de phi pour la state s obtenue de get_EYs_C
voir la description générale au début de ce fichier pour les autres paramètres.

@return \item{VYs}{Un nombre de type double : la variance anticipée de Y pour les observations de la state s}
@return \item{psis}{Un nombre de type double : la valeur de psi pour la state s (uniquement utile à get_momentY_C)}

Précondition : Ns est positif

Créée et vérifiée avec valeurs par défaut le 27 septembre 2012
pas de wrapper R, fonction C seulement
*/
void get_VYs_C (double *xs, int *Ns, double *EYs, double *EXs, double *phis, int *nmodel, 
                  double *beta, double *sig2, double *ps, double *gamma, double *epsilon, double *EX, double *EX2,
                  /* Sortie */ double *VYs, double *psis)
{
  int i;  
  double EX2s = 0, EXgammas = 0, VXs;
  
  /* Première étape : sommes */
  *psis = 0;
  for (i = 0; i < *Ns; i++){ 
    if (*nmodel == 1) {
      *psis = *psis + R_pow(xs[i], 2 * *beta);
    } else {
      EX2s = EX2s + R_pow(xs[i], 2) / *Ns;
      if (*nmodel == 2) EXgammas = EXgammas + R_pow(xs[i], *gamma) / *Ns;
    }
  }
  
  /* Deuxième étape : multiplications ou divisions */
  if (*nmodel == 1) {
    *VYs = *ps * ( (exp(*sig2) * *psis / *Ns) - (*ps * R_pow(*phis / *Ns, 2)) );
  } else {
    VXs = EX2s - R_pow(*EXs, 2);
    if (*nmodel == 0) *VYs = VXs;
    if (*nmodel == 2) *VYs = R_pow(*beta,2) * VXs + *sig2 * EXgammas;
    if (*nmodel == 3) *VYs = (1 - *epsilon) * EX2s + *epsilon * *EX2 - R_pow(*EYs, 2);
  }
  
  /* Dernière étape : ajustement cas extrême */
  if (*VYs < 0) *VYs = 0;
  /* Cet ajustement est utile dans un cas où tous les individus d'une strate ont la meme valeur,
     donc la variance est nulle. Mais à cause de l'écriture de notre formule pour VYs, on obtient
     plutôt VYh[j]=-0. Cette valeur négative peut ensuite faire planter R dans get_gammah_C lors
     de l'éxécution de la fonction R_pow(VYh[j],*q3) si q3 n'est pas entier. */
}

/* ******************************************************************************************** */

/* Obtention de E[Y], Var[Y] pour toutes les strates (excluant la strate certain) + TY et TAY

Calcul des espérances et variances anticipées de Y pour les observations de chacune des L strates.
Aussi, calcul de TY et TAY.

voir la description générale au début de ce fichier pour les autres paramètres.

@return \item{phih}{Un vecteur de longueur L : la valeur de phi dans chaque strate 
                   (uniquement utile à l'algo de Sethi dans la fonction R strata.LH)}
@return \item{psih}{Un vecteur de longueur L : la valeur de psi dans chaque strate
                   (uniquement utile à l'algo de Sethi dans la fonction R strata.LH)}
@return \item{TAY}{Un nombre de type double : la somme anticipée de Y dans les strates take-none}
voir la description générale au début de ce fichier pour les autres valeurs en sortie.

Créée et vérifiée avec valeurs par défaut le 27 septembre 2012
voir wrapper R
*/
void get_momentY_C (double *xnoc, int *stratumIDnoc, int *Nnoc, int *Nh, int *L, int *Nc, double *EYc, int *takenone,
                      int *nmodel, double *beta, double *sig2, double *ph, double *gamma, double *epsilon, 
                      double *EX, double *EX2,
                      /* Sortie */ double *EYh, double *VYh, double *phih, double *psih, double *TY, double *TAY)
{
  int j;
  
  *TY = 0;
  for (j=0; j < *L; j++){
    int Ns = Nh[j];
    if (Ns > 0) {
      int nstratum = j + 1;
      double xs[Ns], EXs;
      extract_stratum_C(&nstratum, xnoc, stratumIDnoc, Nnoc, xs);
      get_EYs_C(xs, &Ns, nmodel, beta, sig2, &ph[j], gamma, epsilon, EX, &EYh[j], &EXs, &phih[j]);
      get_VYs_C(xs, &Ns, &EYh[j], &EXs, &phih[j], nmodel, beta, sig2, &ph[j], gamma, epsilon, EX, EX2, 
                    &VYh[j], &psih[j]);
      *TY = *TY + Ns * EYh[j];
    } else {
      /* valeurs non utilisées je crois, mais je ne prends pas de chance */
      *EYh = *VYh = *phih = *psih = 0;
    }
  }
  if (*Nc > 0) *TY = *TY + *Nc * *EYc; /* Ajout a TY pour la strate certain */
  
  *TAY = 0;
  if (*takenone>0){
		for (j=0; j < *takenone; j++) {
      if (Nh[j] > 0) *TAY = *TAY + Nh[j] * EYh[j];	      
		}
  }
} 

/* ******************************************************************************************** */

/* Obtention de gammah pour les L strates, dans le but de faire l'allocation

@param EYh l'espérance anticipée de Y dans chaque strate obtenue de get_momentY_C
voir la description générale au début de ce fichier pour les autres paramètres.

@return \item{gammah}{Un vecteur de longueur L : gammah pour toutes les strates}

Créée et vérifiée le 27 septembre 2012
*/
void get_gammah_C (int *Nh, double *EYh, double *VYh, int *L, double *q1, double *q2, double *q3,
                      /* Sortie */ double *gammah)
{
  int j;  
  for (j=0; j < *L; j++) {
		gammah[j] = R_pow(R_pow(Nh[j],2),*q1) * R_pow(R_pow(EYh[j],2),*q2) * R_pow(VYh[j],*q3);
	}    
}

/* ******************************************************************************************** */

/* Obtention de ah pour les strates take-some uniquement, dans le but de faire l'allocation

Cette fonction calcule aussi TCN car cette valeur est utile à get_nnoc_C, qu'on utilise seulement
avec un CV cible, mais aussi à la fonction get_nhnonint_C qui est utilisé peu importe le type
de cible (CV ou n).

@param gammah vecteur gammah pour les L strates, obtenu de get_gammah_C
voir la description générale au début de ce fichier pour les autres paramètres.

@return \item{ah}{Un vecteur de longueur L : ah pour les strates take-some, 0 pour les autres strates}
@return \item{TCN}{un nombre entier : la somme des tailles de populations Nh pour les strates take-all}
@return \item{U2}{Un nombre réel : U2 = sommes des gamma des strates take-some
                  (uniquement utile à l'algo de Sethi dans la fonction R strata.LH)}
}

Créée et vérifiée le 27 septembre 2012
*/
void get_ah_C (double *gammah, int *Nh, int *L, int *takenone, int *takeall,
                   /* Sortie */ double *ah, int *TCN, double *U2)
{
  int j;
  double sgammah = 0;
  
  for (j=0; j < *L; j++) ah[j] = 0;
  for (j=*takenone; j < (*L-*takeall); j++) sgammah = sgammah + gammah[j];
  *U2 = sgammah;
	for (j=*takenone; j < (*L-*takeall); j++) ah[j] = gammah[j] / sgammah;

  *TCN = 0;
  if (*takeall>0)
		for (j=(*L-*takeall); j < *L; j++)
			*TCN = *TCN + Nh[j];		
}

/* ******************************************************************************************** */

/* Obtention de nnoc réel, uniquement utile dans le cas d'un CV cible

@param TY la somme gloable anticipée de Y (incluant la certainty stratum), obtenue de get_momentY_C
voir la description générale au début de ce fichier pour les autres paramètres.

@return \item{nnoc}{un nombre réel : la taille d'échantillon permettant d'atteindre le CV cible 
                       (excluant la strate certain)}
@return \item{U}{Un nombre réel : U (uniquement utile à l'algo de Sethi dans la fonction R strata.LH)}
@return \item{V}{Un nombre réel : V (uniquement utile à l'algo de Sethi dans la fonction R strata.LH)}

Créée et vérifiée le 27 septembre 2012
*/
void get_nnoc_C (double *CV, int *Nh, double *ah, double *VYh, double *rhL, int *L, 
                      int *takenone, int *takeall, double *biaspenalty, double *TAY, double *TY, int *TCN, 
                      /* Sortie */ double *nnoc, double *U, double *V)
{
  int j;
  double V1, V2, V3 = 0, V4 = 0;
  
  V1 = R_pow(*CV * *TY, 2);
  V2 = R_pow(*biaspenalty * *TAY, 2);
  
  *U = 0;
  for (j=*takenone; j < (*L-*takeall); j++) {
    if (Nh[j] != 0) {
		  if (Nh[j] != 0 && VYh[j] != 0) *U = *U + R_pow(Nh[j],2) * VYh[j] / (ah[j] * rhL[j]);
		  V3 = V3 + Nh[j] * VYh[j];
    }
	}
  
	if (*takeall>0){
		for (j=(*L-*takeall); j < *L; j++){
			if (Nh[j] != 0) V4 = V4 + Nh[j] * VYh[j] * ( 1 - 1 / rhL[j] );
		}
	}
      
  *V = V1 - V2 + V3 + V4;
	if (*U == 0) *nnoc = *TCN; else *nnoc = *TCN + *U / *V;
}

/* ******************************************************************************************** */

/* Allocation = obtention des taille d'échantillon nh réel pour les L strates

@param ntargetnoc le n cible de l'allocation, en excluant la strate certain
                  Cet argument est de type double car dans le cas d'un CV cible on fournira un nombre
                  réel obtenu de la fonction get_nnoc_C. Cependant, dans le cas d'un n cible,
                  le ntargetnoc est le n cible auquel on soustrait Nc. Il s'agira donc à l'origine
                  d'un entier, que l'on devra convertir en double afin de pouvoir le donner en entrée
                  à get_nhnonint_C.
voir la description générale au début de ce fichier pour les autres paramètres.

Aucune valeur retournée (ou modifiée) n'est unique à cette fonction, 
voir la description générale des sorties au début de ce fichier.

Créée et vérifiée le 27 septembre 2012
*/
void get_nhnonint_C (double *ntargetnoc, int *TCN, int *Nh, double *ah, int *L, int *takenone, int *takeall, 
                         /* Sortie */ double *nhnonint)
{
  int j;
  if (*takenone > 0){
    for (j=0; j < *takenone; j++){
		  nhnonint[j] = 0;
    }
  }
 	for (j=*takenone; j < (*L-*takeall); j++){
		if (Nh[j] == 0) nhnonint[j] = 0; else nhnonint[j] = (*ntargetnoc - *TCN) * ah[j];
 	}
  if (*takeall>0){
  	for (j=(*L-*takeall); j < *L; j++){
			nhnonint[j] = Nh[j];
  	}
  }
}

/* ******************************************************************************************** */

/* simple vérification : doit-on faire un ajustement pour strate take-all?

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

@return takeall ici la valeur de takeall peut être modifiée, en étant augmentée de 1
@return valid vaut 0 si on a besoin de faire un ajustement (la valeur de takeall a été modifiée), 
                   1 si aucun ajustement n'est nécessaire

Créée et vérifiée le 28 septembre 2012
*/
void verif_takeall_C (double *nhnonint, int *Nh, int *L, int *takenone, 
                         /* Sortie et entrée */ int *takeall,
                         /* Sortie */ int *valid)
{
  int j, Tta = 0;
  
  for (j=*takenone; j < (*L-*takeall); j++)
		if (nhnonint[j] > Nh[j]) Tta = Tta + 1;
    /* Attention, si j'ai des problemes, ça pourrait provenir de cette comparaison entre un int et un double */
    
  if ((Tta > 0) && (*takeall < *L - 1 - *takenone)){
    *takeall = *takeall + 1; 
    *valid = 0;     
  } else {
    *valid = 1; 
  }
/* J'ai changé la condition nh[B]>=Nh[B] pour nh[B]>Nh[B] car on ne veux pas de dépassement. Si nh[B]=Nh[B]
   on atteint le CV cible ou le n cible, c'est correct. Ce changement règle certains cas problématiques que j'avais
   rencontré avec Kozak qui restait pris dans un minimum global à cause d'un ajustement pour strate takeall
   qui n'avait pas lieu d'être fait (dans premières strates par exemple). Par contre, ça peut créer des résultats
   surprenant pour lesquels une strate qui paraît recensement est présente en dessous d'une strate échantillonnée. */
}

/* ******************************************************************************************** */

/* Arrondissement des nhnonint afin d'obtenir les nh

Algorithme :
 Pas d'arrondissement à faire pour les strates takenone et takeall.
 Pour un CV cible : les nhnonint des strates takesome sont tous arrondis vers le haut.
 Pour un n cible (Il faut que la somme des nh arrondis = n, mais attention si on a une
 strate certain, le n cible pour l'arrondissement, donc la valeur donnée à l'agument n
 en entrée, doit être le n cible total - Nc, la taille de la strate certain.) :
 1- Ramener a 1 les nhnonint >0 et <1.
 2- Identifier les nhnonint qui reste a arrondir.
 3- Calculer le nombre de nhnonint a arrondir de chacune des facons possibles
    (nm1 : partie entiere - 1, mp0 : partie entiere, np1 : partie entiere + 1)
  Un nm1 non nul peut survenir quand des nhnonint entre 0 et 1 sont ramenés à 1.
 4- Ordonner les parties decimales de nhnonint qui reste a arrondir en ordre croissant.
 5- Arrondissement : 
    Les nhnonint avec les nm1 plus petites parties decimales seront arrondis par : partie entiere - 1,
    les nhnonint avec les np1 plus grandes parties decimales seront arrondis par : partie entiere + 1,
	les autres seront arrondis par : partie entiere.

@param ntargetround Utilisé seulement si on a un n cible : il s'agit du n cible pour l'arrondissement.
                    C'est en fait ntargetnoc, le n cible demandé - le nombre d'observations dans la strate certain,
                    donc un entier.
voir la description générale au début de ce fichier pour les autres paramètres.

Aucune valeur retournée (ou modifiée) n'est unique à cette fonction, 
voir la description générale des sorties au début de ce fichier.

Créée le 25 avril 2012, ajustée et vérifiée le 28 septembre 2012
*/ 
void get_nh_C(double *nhnonint, int *findn, int *ntargetround, int *Nh, int *TCN, 
                  int *L, int *takenone, int *takeall, 
  		            /* sortie */ double *nh)
{ 
  /* Note : Logiquement, nh devrait être un entier. Cependant, les entiers ne peuvent pas prendre la
     valeur nan en C. Alors la programmation est plus simple si je stocke nh en double, em gardant en tête
     qu'en théorie il s'agit bien d'un entier */
  
	int j; /* pour les boucles */

 	/* Pas d'arrondissement a faire pour les strates takenone et takeall. */
  if (*takenone>0)
	  for (j=0; j < *takenone; j++) 
      nh[j] = 0;
	if (*takeall>0)
		for (j=(*L-*takeall); j < *L; j++)
			nh[j] = Nh[j];

	int Lts = *L - *takenone - *takeall; /* le nombre de strates takesome */
	if (*findn == 1) {
    /* Pour un CV cible : les nhnonint des strates takesome sont tous arrondis vers le haut.*/
		int nht[Lts]; /* parties entieres des nhnonint a arrondir */
		double reste[Lts]; /* parties decimales des nhnonint a arrondir  */	
		for (j=0; j < Lts; j++) {
		/* Ce code fait la meme chose que la fonction ceiling en R. */
    		nht[j] = floor(nhnonint[*takenone + j]);
    		reste[j] = nhnonint[*takenone + j] - nht[j]; 
    		if(reste[j]!=0) nh[*takenone + j] = nht[j] + 1; else nh[*takenone + j] = nht[j];
		}
	} else { 
    /* Pour un n cible : */
	
 		/* 1- Ramener a 1 les nhnonint >0 et <1 */
		int Ltsr = Lts; /* nombre de nhnonint a arrondir parmi les strates takesome */
		for (j=0; j < Lts; j++) {
	 		if( nhnonint[*takenone + j] > 0 && nhnonint[*takenone + j] < 1 ) {
				nh[*takenone + j] = 1; 
				Ltsr = Ltsr - 1;
			} 
		}
    
    /* Note : on laisse aller les nhnonint nuls ou négatifs. Il vont simplement ête arrondis comme les autres.
       Ces valeurs ne sont pas valides logiquement, mais elles sont possibles mathématiquement dans les cas suivants :
       Situation menant à un nhnonint nul : variance nulle dans une strate take-some
       Situation menant à des nhnonint négatifs : strate take-all plus grande que le n cible */
		
		if ( Ltsr > 0 ) { /* Si on a quelque chose a arrondir seulement */

			/* 2- Identifier les nhnonint qui doivent être arrondis */
			int Id[Ltsr];
			int i=0; /* Indice pour le vecteur des valeurs a arrondir uniquement */
			for (j=0; j < Lts; j++) {
				if( !(nhnonint[*takenone + j] > 0 && nhnonint[*takenone + j] < 1) ) { 
					Id[i] = *takenone + j;
					i = i + 1;
				}
			}
   
			/* 3- Calculer le nombre de nhnonint a arrondir de chacune des facons possibles */
			int snht = 0; /* somme des parties entieres pour les nhnonint a arrondir */
			int nht[Ltsr]; /* parties entieres des nhnonint a arrondir */
			double reste[Ltsr]; /* parties decimales de nhnonint a arrondir  */	
			int index[Ltsr]; /* vecteur d'indices utile pour ordonner les parties decimales */
			for (j=0; j < Ltsr; j++) {
				nht[j] = floor(nhnonint[Id[j]]);
				reste[j] = nhnonint[Id[j]] - nht[j]; 
				snht = snht + nht[j];
				index[j] = j;			
			}
			int nhaut; /* nombre de nhnonint à arrondir vers le haut (peut être négatif) */
			nhaut = (*ntargetround - (Lts - Ltsr) - snht - *TCN );
   
			int nm1, np0; /* (nm1 strates : partie entière - 1, mp0 strates : partie entière, 
							  np1 strates : partie entière + 1 (pas besoin de le calculer)) */
			if ( nhaut < 0) {
				nm1 = -nhaut;
				np0 = Ltsr - nm1;
			} else {
				nm1 = 0;
				np0 = Ltsr - nhaut;
			}
			
			/* 4- Ordonner les parties décimales de nhnonint qui reste à arrondir en ordre croissant. */
			rsort_with_index(reste,index,Ltsr); /* si égalités, fait comme rank avec ties.method="first" */
			
			/* 5- Arrondissement final */   	
			for (j=0; j < Ltsr; j++) {
				if ( j < nm1) {
					nh[Id[index[j]]] = nht[index[j]] - 1;
				} else if ( j >= nm1 && j < nm1 + np0) {
					nh[Id[index[j]]] = nht[index[j]];
				} else {
					nh[Id[index[j]]] = nht[index[j]] + 1;
				}
				/* Si un nh est inférieur à 0, je vais le ramener à 0 */
				nh[Id[index[j]]] = fmax2(0, nh[Id[index[j]]]);
        /* Seul cas où c'est possible : nnoc négatif, ce qui est vraiment tiré par les chaveux.
           il est certain ue les ah sont positifs étant donné que les Nh et les VhY sont toujours des 
           nombres positifs, et que EYh est élevé au carré dans la formule des gammah. */
			}
		} 
	}
  
  /* Si les nhnonint prennaient la valeur nan, les nh caculés par le code ci-dessus ne prendront
     pas la valeur nan, mais plutot une valeur extrême. Alors je vais faire une petite correction ici */
  for (j=0; j < Lts; j++) {
    if (!R_FINITE(nhnonint[*takenone + j])) nh[*takenone + j] = R_NaN;
  }
  /* Valeurs manquantes en C, source = Wrinting R extensions, sections 5.10.3 et 6.4 */
  
  
/* J'ai étudié les valeurs possibles de nhaut.
   *ntargetround est le ncible. Il est en fait égal à  :
  0 [strates takenone] + sum(nhnonint) [strates takesome] + TCN [strates takeall].
	On peut briser sum(nhnonint) en deux parties : une pour les nhnonint entre 0 et 1, 
	et une autre partie pour les autres nhnonint.
	On soustrait à ça 
	0 [strates takenone] + ((Lts - Ltsr)*1 + snht) [strates takesome] + snhta [strates takeall].
	On voit que les termes pour les strates takenone et takeall s'annulent.
	écrivons le résultat de la soustraction comme la somme des deux termes suivants :
	t1 = sum(nhnonint) [strates takesome avec nhnonint entre 0 et 1] - (Lts - Ltsr)
	t2 = sum(nhnonint) [strates takesome avec nhnonint pas entre 0 et 1] - snht
	où, rappelons-le, snht est la somme des parties entères des nhnonint pas entre 0 et 1
	pour une strate takesome.
	Les valeurs max et min de ces deux termes sont :
	min(t1) tend vers - (Lts - Ltsr), max(t1) tend vers 0;
	min(t2) tend vers 0, max(t2) tend vers Ltsr.
	Ainsi, min(nhaut) = min(t1) + min(t2) : tend vers - (Lts - Ltsr)
		   max(nhaut) = max(t1) + max(t2) : tend vers Ltsr.
	C'est donc dire qu'il est impossible d'avoir à arrondir vers le haut plus de nombres
	qu'on en a, ce qui est une excellente chose.
	Cependant, on pourrait avoir des cas limite comme
	nhnonhint = 0.3333 , 0.3333, 0.3333 : serait arrondi par 1, 1, 1, mais ici ncible = 1.
	Donc les nh fournis par notre algorithme ne somment pas au ncible.
	Je pense qu'on n'a pas ici à essayer de modifier l'algo d'arrondissement. 
	C'est simplement un signe que le ncible demandé n'est pas réaliste. */
}			

/* ******************************************************************************************** */

/* Calcul du RRMSE

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

@return RRMSE Le Relative Root Mean Squared Error pour le plan donné en entrée. 

Créée le 27 avril 2012, ajustée et vérifiée le 28 septembre 2012
*/
void get_RRMSE_C (double *biaspenalty, double *TY, double *TAY, int *Nh, double *VYh, double *nhcalcul, 
                  double *rhL, int *L,int *takenone,
                  /* sortie */ double *RRMSE)
{
  int j; /* pour les boucles */

  /* Si les nh sont nuls, RMSE prendra une valeur manquante */
  /* Cette situation est impossible dans strata_bh_opti_C avec dotests = 1 */
	for (j=*takenone; j < *L; j++) {
		if (nhcalcul[j] < 0)
		  *RRMSE = NA_REAL;	
	}
	if (!ISNA(*RRMSE)) { 
		double sum = 0; /* Somme à calculer sur les strates takesome et takeall */
		for (j=*takenone; j < *L; j++) {
      if (Nh[j] != 0 && VYh[j] != 0) sum = sum + R_pow(Nh[j],2) * VYh[j] * (R_pow(nhcalcul[j]*rhL[j],-1) - R_pow(Nh[j], -1));     
      /* Ici, on utilise R_pow(Nh[j], -1) plutôt que 1/Nh[j] parce qu'avec le symbole /, étant
         donné que Nh[j] et 1 sont des entiers, le résultat est aussi stocké comme un entier. Ainsi la
         fraction Nh[j] est arrondie à 1 (si Nh[j]=1) ou 0 (sinon). Alors que R_pow transforme Nh[j] en 
         double avant de faire le calcul (déf. de R_pow : double R_pow (double x, double y)).      
         R_pow(nhcalcul[j]*rhL[j],-1) pourrait, pour sa part, être remplacé par (1/(nhcalcul[j]*rhL[j]))
         parce que nhcalcul et rhL sont déjà des double. */
		}
		*RRMSE = R_pow(fmax2(0, R_pow(*biaspenalty * *TAY, 2) + sum), 0.5) / *TY;
	} 
/* Valeurs manquantes en C, source = Wrinting R extensions, sections 5.10.3 et 6.4 */
}

/* ******************************************************************************************** */

/* Calcul de n, la taille d'échantillon totale, incluant la strate certain
   (il s'agit du critère à optimiser dans le cas d'un CV cible)

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

@return n la somme des éléments de nhcalcul + Nc

Créée et vérifiée le 28 septembre 2012.
*/
void get_n_C (double *nhcalcul, int *L, int *Nc,
                /* sortie */ double *n)
{
  int j;
  *n = *Nc; /* Il faut compter les unités de la strates certain dans cette somme */
  for (j=0; j < *L; j++) *n = *n + nhcalcul[j];
}

/* ******************************************************************************************** */

/* Teste les conditions sur les Nh 

Conditions : Nh >= minNh pour toutes les Ls strates take-some et take-all,
             Nh >= 0 pour les strate take-none

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

@return NhOK 1 si les conditions sur les Nh sont respectées pour toutes les strates, 0 sinon

Créée et vérifiée le 12 octobre 2012.
*/
void test_Nh_C (int *Nh, int *L, int *takenone, int *minNh,
                  /* sortie */ int *NhOK)
{
  int j, cond = 0;
	for (j=0; j<*takenone; j++)    if( Nh[j] >= 0 )      cond = cond + 1; 
	for (j=*takenone; j < *L; j++) if( Nh[j] >= *minNh ) cond = cond + 1;
  if ( cond == *L) *NhOK = 1; /* 1 = les conditions sont respectées pour toutes les strates*/
  else *NhOK = 0;             /* 0 = les conditions ne sont pas respectée */
}


/* ******************************************************************************************** */

/* Teste les conditions sur les nh 

Conditions : nh > 0 pour toutes les Ls strates take-some et take-all
             (ce test sur les nh entiers revient à nh !=0 car on a ramené à 0 tous les nhnonint négatifs, 
              ce qui devrait survenir très rarement, uniquement dans des cas limite),
             rien à tester sur les strates take-none car dans ce cas par définition nhnonint = nh = 0.

Aucun paramètre n'est unique à cette fonction, 
voir la description générale des paramètres au début de ce fichier.

@return nhOK 1 si les conditions sur les nh sont respectées pour toutes les strates, 0 sinon

Créée et vérifiée le 12 octobre 2012.
*/
void test_nh_C (double *nh, int *L, int *takenone,
                  /* sortie */ int *nhOK)
{
  int j, cond = 0;
	for (j=*takenone; j < *L; j++) if( nh[j] > 0 ) cond = cond + 1;
  if ( cond == *L - *takenone) *nhOK = 1; /* 1 = les conditions sont respectées pour toutes les strates*/
  else *nhOK = 0;                         /* 0 = les conditions ne sont pas respectée */
}


/* ******************************************************************************************** */

/* Fonction qui fait tous les calculs nécessaires à l'algorithme de Kozak lorsqu'il "essaie une borne"

Cette fonction fournit les détails d'un plan d'échantillonnage stratifié, pour des bornes données.
Elle travaille sur le vecteur des observations excluant la strate certain.

@param dotests indicatrice : 1 si on doit faire les tests des conditions sur les Nh et les nh 
                             (c'est le cas pour l'algo de Kozak, incluant l'énumération complète),
                             0 si on n'a pas besoin de faire ces tests (strata.bh, .geo et .cumrootf)
@param takealladjust indicatrice : 1 si on demande à faire un ajustement automatique pour les strates recensement, 
                                   0 sinon
voir la description générale au début de ce fichier pour les autres paramètres.

@result takeallout un entier : valeur de takeall à la fin des calculs.
@result optinhnonint le critère à optimiser (n si CV cible, RRMSE si n cible) calculé à partir
                     des tailles d'échantillon non entières nhnonint
@result optinh le critère à optimiser (n si CV cible, RRMSE si n cible) calculé à partir
               des tailles d'échantillon entières nh
voir la description générale au début de ce fichier pour les autres sorties.

Créée et vérifiée le 28 septembre 2012
voir fonction R strata.bh.internal qui appelle cette fonction
*/
void strata_bh_opti_C(double *xnoc, int *Nnoc, double *bhfull, int *L, int *takenone, int *takeall,
            int *Nc, double *EYc, double *q1, double *q2, double *q3,
            int *nmodel, double *beta, double *sig2, double *ph, double *gamma, double *epsilon, double *EX, double *EX2,
            int *findn, int *n, double *CV, double *rhL, double *biaspenalty, int *takealladjust, int *dotests, int *minNh,
            /* Sortie */ int *NhOK, int *nhOK, /* uniquement utile à l'algo de Kozak*/ 
                         double *phih, double *psih, double *gammah, double *ah, double *U2, double *U, double *V, 
                         /* uniquement utile à l'algo de Sethi*/
                         int *stratumIDnoc, int *Nh, double *EYh, double *VYh, double *TY, double *TAY,  
                         double *nhnonint, int *takeallout, double *nh, double *optinhnonint, double *optinh)
{
  int j, TCN, valid = 0, arret1, arret2;
  double ntargetnoc;
  *takeallout = *takeall;
  
  /* Délimitation des strates */
  get_stratumIDnoc_C(xnoc, Nnoc, bhfull, L, stratumIDnoc);
  
  /* Calcul des tailles de population dans les strates*/
  get_Nh_C(stratumIDnoc, Nnoc, L, Nh);

/*Rprintf("Nh : "); for (j=0; j < *L; j++) Rprintf("%d  ",Nh[j]);  Rprintf("\n");*/

  /* Si besoin, test sur les Nh ici */
  if (*dotests == 1) {
    test_Nh_C(Nh, L, takenone, minNh, NhOK);
    if (*NhOK == 0) arret1 = 1; else arret1 = 0; /* On arrête l'exécution de la fonction si NhOK=0 */
  } else {
    arret1 = 0;
  }

/*Rprintf("arret1 : %d  ",arret1);  Rprintf("\n");*/
 
  if (arret1 == 0) {
    /* Tout le reste de la fonction est dans ce if car si on a testé les Nh et qu'ils ne respectaient pas les
       conditions, il est inutile de faire les calculs subséquents (donc on sauve du temps en ne les faisant pas,
       mais attention, les valeurs en sortie autres que NhOK ne seront pas bonnes, il ne faut pas les considérer) */
    
    /* Calcul des moments anticipées (EYh et VYh) et de sommes dont on a besoin dans les calculs subséquents (TY et TAY)*/
    get_momentY_C(xnoc, stratumIDnoc, Nnoc, Nh, L, Nc, EYc, takenone, nmodel, beta, sig2, ph, gamma, epsilon, 
                      EX, EX2, EYh, VYh, phih, psih, TY, TAY);

/*Rprintf("EYh : "); for (j=0; j < *L; j++) Rprintf("%f  ",EYh[j]);  Rprintf("\n");
Rprintf("VYh : "); for (j=0; j < *L; j++) Rprintf("%f  ",VYh[j]);  Rprintf("\n");*/

    /* Allocation : répartition de n parmi les L strates. */
    /* Étape 1 alloc : calcul des gammah */
    get_gammah_C(Nh, EYh, VYh, L, q1, q2, q3, gammah);
    
/*Rprintf("gammah : "); for (j=0; j < *L; j++) Rprintf("%f  ",gammah[j]);  Rprintf("\n");*/

    while (valid == 0) {
      /* Étape 2 alloc : calcul des ah et de TCN*/
      get_ah_C(gammah, Nh, L, takenone, takeallout, ah, &TCN, U2);
      
/*Rprintf("ah : "); for (j=0; j < *L; j++) Rprintf("%f  ",ah[j]);  Rprintf("\n");*/

      /* Étape 3 alloc : calcul du ntargetnoc*/
      if (*findn == 1){
        /* Dans le cas d'un CV cible, on doit appliquer une formule */
        get_nnoc_C(CV, Nh, ah, VYh, rhL, L, takenone, takeallout, biaspenalty, TAY, TY, &TCN, &ntargetnoc, U, V);
      } else {
        ntargetnoc = *n - *Nc; 
      }
      
/*Rprintf("ntargetnoc : %f  ",ntargetnoc);  Rprintf("\n");*/

      /* Étape 4 alloc : application de la règle d'allocation 
                         (qui donnera en résultat les tailles d'échantillon non entières nhnonint) */
      get_nhnonint_C(&ntargetnoc, &TCN, Nh, ah, L, takenone, takeallout, nhnonint);

/*Rprintf("nhnonint : "); for (j=0; j < *L; j++) Rprintf("%f  ",nhnonint[j]);  Rprintf("\n");*/

      /* Étape 5 alloc : vérification strates recensement, si besoin */
      if (*takealladjust == 1){
        verif_takeall_C(nhnonint, Nh, L, takenone, takeallout, &valid);
      } else {
        valid = 1;
      }

/*Rprintf("valid : %d  ",valid);  Rprintf("\n");*/


    }
    /* Étape 6 alloc : Arrondissement des nhnonint pour obtenir les nh entiers */
    int ntargetround = ntargetnoc;
    get_nh_C(nhnonint, findn, &ntargetround, Nh, &TCN, L, takenone, takeallout, nh);

/*Rprintf("nh : "); for (j=0; j < *L; j++) Rprintf("%d  ",nh[j]);  Rprintf("\n");*/

       /* Ici, si on fait un ajustement automatique pour strates recensement, on ne ramène pas à Nh les nh > Nh
       (nh entiers ou réels), car le but de cette fonction est de calculer les critères pour 
       l'optimisation. La fonction sera utilisée par l'algorithme de Kozak. 
       Si on rameneait à Nh les nh > Nh dans l'optimisation, on aurait le problème suivant.
       Dans le cas d'un CV cible, le critère à minimiser et n. Si on tronque à Nh les valeurs de nh, l'apparition
       d'une strate recensement causerait une diminution du critère n, qu'on cherche justement à minimiser. Alors
       les plans faisant apparaître une strate recensement seraient favorisés. L'optimisation prendrait alors une 
       mauvaise direction. De toute façon, l'ajustement automatique pour strate recensement est obligatoire à 
       toutes les fonctions qui calculent des bornes. Cet ajustement nous assure que tous les nh (entiers ou réels)
       seront <= Nh au terme des calculs. */     
    if (*takealladjust == 0 || *takeallout == *L - 1 - *takenone)
      for (j=*takenone; j < (*L-*takeall); j++)
        if (nh[j] > Nh[j]) nh[j] = Nh[j];
    /* Si l'ajustement automatique n'est pas fait ou s'il y a une seule strate take-some, il se pourrait qu'un 
       nh soit > Nh à la sortie de la présente fonction. Dans ce cas, on va ramener à Nh les nh entiers > Nh, 
       mais pas les nh réels pour que l'utilisateur puisse comprendre à partir de la sortie pourquoi le CV cible 
       n'est pas nécessairement atteint sans correction pour strate recensement. */
        
    /* Si besoin, test sur les nh ici */
    if (*dotests == 1) {
      test_nh_C(nh, L, takenone, nhOK);
      if (*nhOK == 0) arret2 = 1; else arret2 = 0; /* On arrête l'exécution de la fonction si nhOK=0 */
    } else {
      arret2 = 0;
    }
    
/*Rprintf("arret2 : %d  ",arret2);  Rprintf("\n");*/
  
    if (arret2 == 0) {
      /* Tout le reste de la fonction est dans ce if car si on a testé les nh et qu'ils ne respectaient pas les
         conditions, il est inutile de faire les calculs subséquents (donc on sauve du temps en ne les faisant pas, mais
         attention, les valeurs en sortie optinhnonint et optinh ne seront pas bonnes, il ne faut pas les considérer) */
      
      /* Calcul du critère à optimiser, sur les nh et sur les nhnonint */
      if (*findn == 1) { /* Pour un CV cible, le critère à optimiser est n, la taille d'échantillon totale */
        get_n_C(nhnonint, L, Nc, optinhnonint);
        get_n_C(nh, L, Nc, optinh);
      } else { /* Pour un n cible, le critère à optimiser est le RRMSE */
        get_RRMSE_C(biaspenalty, TY, TAY, Nh, VYh, nhnonint, rhL, L, takenone, optinhnonint);
        get_RRMSE_C(biaspenalty, TY, TAY, Nh, VYh, nh, rhL, L, takenone, optinh);
      }
      
/*Rprintf("optinh : %f  ",optinh);  Rprintf("optinhnonint : %f  ",optinhnonint); Rprintf("\n");*/
    }
  }
}


/* ******************************************************************************************** */

/* Makes the conversion from stratum boundaries expressed in terms of data rank (pbh), 
   to stratum boundaries expressed on the scale of the data (bhfull)

@param pbh vecteur de longueur L-1 représentant des bornes de strates, mais sur l'échelle des rangs des données :
           chaque élément de pbh est un entier représentant la position dans le vecteur x1noc d'une borne.
voir la description générale au début de ce fichier pour les autres paramètres.

@return bhfull le vecteur L+1 des bornes pleines, sur l'échelle des données, équivalentes aux pbh

Créée le 12 octobre 2012
*/
void pbh2bhfull_C(int *pbh, int *L, double *x1noc, int *N1noc, double *bhfull)
{ 
  int j;
  bhfull[0] = x1noc[0];
  for (j=0; j < *L-1; j++) {
		if(pbh[j]<=1) {
			bhfull[j+1] = x1noc[0];
		} else if (pbh[j]>*N1noc) {
			bhfull[j+1] = x1noc[*N1noc - 1] + 1;
		} else 
			bhfull[j+1] = (x1noc[pbh[j]-1] + x1noc[pbh[j]-2])/2;
	}
  bhfull[*L] = x1noc[*N1noc - 1] + 1;  
} 


/* ******************************************************************************************** */

/* Énumération complète des ensembles de bornes possibles pour touver le meilleur 

On essaie tous les ensembles de bornes possibles et on recherche la meilleure, celle qui minimise le critère (RRMSE si n cible, n si CV cible).

@param pbhsol vecteur de longueur (L-1)*nsol qui contient tous les ensembles de bornes à essayer, 
              représentées sur l'échelle des rangs : les L-1 premiers éléments forment le premier ensemble de bornes,
              les L-1 éléments suivants le deuxième ensemble, etc.
@param nsol le nombre d'ensemble de bornes à essayer
voir la description générale au début de ce fichier pour les autres paramètres.

@return soldetail un énorme vecteur de longueur ((L-1)+2*L+5)*nsol, qui devriendra une matrice en R,
                  comprennant les éléments dont on veut garder la trace (bh, Nh, nh, takeall, Nhok, nhok, 
                  optinh, optinhnonint) pour chaque ensemble de bornes à essayer. Ordre dans le vecteur :
                  tous les éléments pour le premier ensemble de bornes, suivi de tous les éléments pour le 
                  deuxième, etc.

Créée le 12 octobre 2012
*/
void complete_enum_C(int *pbhsol, int *nsol, int *L, double *x1noc, int *N1noc, double *xnoc, int *Nnoc, 
                         int *takenone, int *takeall, int *Nc, double *EYc, double *q1, double *q2, double *q3,
                         int *nmodel, double *beta, double *sig2, double *ph, double *gamma, double *epsilon, 
                         double *EX, double *EX2, int *findn, int *n, double *CV, double *rhL, 
                         double *biaspenalty, int *minNh,
                         /* Sortie */ double *soldetail)
{
  int i, j, pbh[*L-1], NhOK, nhOK, stratumIDnoc[*Nnoc], Nh[*L], takeallout;
  double bhfull[*L+1], EYh[*L], VYh[*L], TY, TAY, nh[*L] , nhnonint[*L], optinhnonint, optinh;
  double phih[*L], psih[*L], gammah[*L], ah[*L], U2, U, V;
  int takealladjust = 1; /* Ici on fait l'ajustement pour strates takeall, */
  int dotests = 1;       /* ainsi que les tests sur Nh et nh. */

  for (i=0; i < *nsol; i++){
    /* Déterminer les bornes pleines sur l'échelle des données à partir des combinaisons fournies en entrée */
    for (j=0; j < *L - 1; j++) pbh[j] = pbhsol[j + i * (*L - 1)];
    pbh2bhfull_C(pbh, L, x1noc, N1noc, bhfull);

    /* Calculs pour la stratification */
    strata_bh_opti_C(xnoc, Nnoc, bhfull, L, takenone, takeall, Nc, EYc, q1, q2, q3,nmodel, beta, sig2, ph, 
           gamma, epsilon, EX, EX2, findn, n, CV, rhL, biaspenalty, &takealladjust, &dotests, minNh,
           &NhOK, &nhOK, phih, psih, gammah, ah, &U2, &U, &V, stratumIDnoc, Nh, EYh, VYh, &TY, &TAY, 
           nhnonint, &takeallout, nh, &optinhnonint, &optinh);
           
    /* Enregistrement des résultats (bh, Nh, nh, takeall, Nhok, nhok, optinh, optinhnonint)*/
    for (j=0; j < *L - 1; j++) soldetail[j + i * (3 * *L + 4)] = bhfull[j + 1];
    for (j=0; j < *L; j++) soldetail[*L - 1 + j + i * (3 * *L + 4)] = Nh[j];
    for (j=0; j < *L; j++) soldetail[2 * *L - 1 + j + i * (3 * *L + 4)] = nh[j];
    soldetail[3 * *L - 1 + i * (3 * *L + 4)] = optinh;
    soldetail[3 * *L + i * (3 * *L + 4)]     = optinhnonint;
    soldetail[3 * *L + 1 + i * (3 * *L + 4)] = takeallout;
    soldetail[3 * *L + 2 + i * (3 * *L + 4)] = NhOK;
    soldetail[3 * *L + 3 + i * (3 * *L + 4)] = nhOK;
  }
  
}


/* ******************************************************************************************** */

/* Algorithme de Kozak (original) 

@param combin2try vecteur de longueur (2*(L-1)+6)*ncombin qui contient toutes les combinaisons à essayer.
                  Le vecteur contient dans l'ordre
                  éléments : (ibhtype,    ibh,    ipbh, optinh, optinhnonint, takeall, maxstep, maxstill)
                  positions: (      0, 1->L-1, L->2L-2,   2L-1,           2L,    2L+1,    2L+2,     2L+3)
                  définissant la première combinaison, tous les mêmes éléments pour la deuxième combinaison, etc.
@param ncombin le nombre de combinaisons à essayer (le nombre de lignes dans la version matrice de combin2try)
voir la description générale au début de ce fichier pour les autres paramètres.

@return rundetail  un énorme vecteur de longueur (2*(L-1)+8)*(ncombin*rep), qui devriendra une matrice en R,
                   comprennant les éléments dont on veut garder la trace pour chaque combinaison à essayer :
                   éléments : (    bh, optinh, optinhnonint, takeall, niter, ibhtype,       ibh,  rep)
                   positions: (0->L-2,    L-1,            L,     L+1,   L+2,     L+3, L+4->2L+2, 2L+3)
                   Ordre dans le vecteur : tous les éléments pour la première combinaison, 
                   suivi de tous les éléments pour la deuxième, etc.
@return iterdetail un énorme vecteur de longueur ((L-1)+6)*nrowiterdetail, qui devriendra une matrice en R,
                   comprennant les éléments dont on veut garder la trace pour chaque itération de l'algo :
                   éléments : (    bh, optinh, optinhnonint, takeall, step, iter, run)
                   positions: (0->L-2,    L-1,            L,     L+1,  L+2,  L+3, L+4)
                   Ordre dans le vecteur : tous les éléments pour la première itération, 
                   suivi de tous les éléments pour la deuxième, etc.
@return nrowiterdetail le nombre de lignes dans la version matrice du vecteur iterdetail

Créée le 17 et 18 octobre 2012
*/
void algo_Kozak_C(double *combin2try, int *ncombin, int *L, double *x1noc, int *N1noc, double *xnoc, int *Nnoc, 
                      int *takenone, int *takeall, int *Nc, double *EYc, double *q1, double *q2, double *q3,
                      int *nmodel, double *beta, double *sig2, double *ph, double *gamma, double *epsilon, 
                      double *EX, double *EX2, int *findn, int *n, double *CV, double *rhL, 
                      double *biaspenalty, int *minNh, int *maxiter, int *idoptinh, int *rep,
                      /* Sortie */ double *rundetail, double *iterdetail, int *nrowiter)
{
  int r, i, j, iter, istill, accept, maxstep, maxstill, jbh, step, sign;
  int pbh[*L-1], npbh[*L-1], NhOK, nhOK, stratumIDnoc[*Nnoc], Nh[*L], takeallout;
  double bhfull[*L+1], EYh[*L], VYh[*L], TY, TAY, phih[*L], psih[*L], gammah[*L], ah[*L], U2, U, V;
  double nh[*L], nhnonint[*L], optinhnonint, optinh, noptinhnonint, noptinh, diffrelopti;
  int takealladjust = 1; /* Ici on fait l'ajustement pour strates takeall, */
  int dotests = 1;       /* ainsi que les tests sur Nh et nh. */
  int irow = 0;  /* Le compteur du nombre de lignes dans iterdetail est initialisé à zéro. */
  int rrow = 0;  /* Le compteur du nombre de lignes dans rundetail est initialisé à zéro. */
  int ncol_combin = 2 * *L + 4; /* Le nombre de colonnes dans la version matrice du vecteur combin2try */
  int ncol_run = 2 * *L + 4;    /* Le nombre de colonnes dans la version matrice du vecteur rundetail */
  int ncol_iter = *L + 5;       /* Le nombre de colonnes dans la version matrice du vecteur iterdetail */
  
  for (i=0; i < *ncombin; i++){  /* boucle sur les combinaisons à essayer */
  
    for (r=0; r < *rep; r++){  /* boucle sur les répétitions de l'algorithme */   
      
      /* Variables à initialiser pour l'algorithme */
      /* Rappel utile à la programmation ici -> combin2try : éléments et leur positions
         (ibhtype,    ibh,    ipbh, optinh, optinhnonint, takeall, maxstep, maxstill)
         (      0, 1->L-1, L->2L-2,   2L-1,           2L,    2L+1,    2L+2,     2L+3) */
      iter = 0;   /* Le compteur d'itérations par répétitions est initialisé à zéro. */
      istill = 0; /* Le compteur d'itérations sans acceptation de nouvelles bornes est initialisé à zéro. */
      for (j=0; j < *L - 1; j++)  /* bornes initiales sur l'échelle des rangs */
        pbh[j] = combin2try[(j + *L) + i * ncol_combin];
      optinh       = combin2try[(2 * *L - 1) + i * ncol_combin];
      optinhnonint = combin2try[(2 * *L + 0) + i * ncol_combin];
      maxstep      = combin2try[(2 * *L + 2) + i * ncol_combin];
      maxstill     = combin2try[(2 * *L + 3) + i * ncol_combin];
      

      /* Info à mettre dans iterdetail pour les bornes initiales */
      /* Rappel utile à la programmation ici -> iterdetail : éléments et leur positions
         (    bh, optinh, optinhnonint, takeall, step, iter, run)
         (0->L-2,    L-1,            L,     L+1,  L+2,  L+3, L+4) */
      for (j=0; j < *L - 1; j++)                     /* bornes sur l'échelle des données */
        iterdetail[j + irow * ncol_iter] = combin2try[(j + 1) + i * ncol_combin];
      for (j=*L - 1; j < *L + 2; j++)                /* optinh, optinhnonint et takeall */
        iterdetail[j + irow * ncol_iter] = combin2try[(j + *L) + i * ncol_combin];  
      iterdetail[(*L + 2) + irow * ncol_iter] = 0;    /* pas fait par l'algo de Kozak */
      iterdetail[(*L + 3) + irow * ncol_iter] = iter; /* numéro de l'itération */
      iterdetail[(*L + 4) + irow * ncol_iter] = rrow + 1; /* numéro du run de l'algo */
      /* On vient de remplir une ligne de iterdetail, il faut donc incrémenter de 1 irow. */
      irow = irow + 1;

      /* On peut déjà mettre les info pour les bornesinitiales dans dans rundetail  */
      /* Rappel utile à la programmation ici -> rundetail : éléments et leur positions
         (    bh, optinh, optinhnonint, takeall, niter, ibhtype,       ibh,  rep)
         (0->L-2,    L-1,            L,     L+1,   L+2,     L+3, L+4->2L+2, 2L+3) */
      rundetail[(*L + 3) + rrow * ncol_run] = combin2try[0 + i * ncol_combin];
      for (j=0; j < *L-1; j++) rundetail[(j + *L + 4) + rrow * ncol_run] = combin2try[(j + 1) + i * ncol_combin];      
      rundetail[(2 * *L + 3) + rrow * ncol_run] = r + 1;      

      /* Itérations de l'algorithme */      
    	while ( (iter < *maxiter) && (istill < maxstill) ) {	
        
  		  /* Identifier le nouvel ensemble de bornes à essayer */
        /* Choix aléatoire de la borne à remplacer*/
    		GetRNGstate(); 
    		jbh = floor(unif_rand()* (*L-1)); /* 0, 1, ..., L-2 */
    			if (jbh==(*L-1)) jbh=0; /* si jbh vaut L-1, ce qui a une probabilité presque nulle */
        /* Choix aléatoire du pas à faire*/
    		step =  floor(unif_rand() * maxstep) + 1; /* 1, 2, ..., maxstep */
    			if (step==(maxstep+1)) step=1; /* si step vaut maxstep+1, ce qui a une probabilité presque nulle */
    		sign = floor(unif_rand()*2); /* 0 ou 1 */
    		if (sign==0) step=-step;
    		PutRNGstate();    		
    		/* Obtention des nouvelles bornes en modifiant les anciennes */
    		for (j=0; j < *L-1; j++) npbh[j] = pbh[j];
    		npbh[jbh] = pbh[jbh] + step;
        
/*Rprintf("%d  : ",iter); 
for (j=0; j < *L-1; j++) Rprintf("%d  ",npbh[j]); */
        
        /* Détermination des bornes pleines sur l'échelle des données associées à ces bornes modifiées */
        pbh2bhfull_C(npbh, L, x1noc, N1noc, bhfull);
    
        /* Calculs pour la stratification */
        strata_bh_opti_C(xnoc, Nnoc, bhfull, L, takenone, takeall, Nc, EYc, q1, q2, q3,nmodel, beta, sig2, ph, 
               gamma, epsilon, EX, EX2, findn, n, CV, rhL, biaspenalty, &takealladjust, &dotests, minNh,
               &NhOK, &nhOK, phih, psih, gammah, ah, &U2, &U, &V, stratumIDnoc, Nh, EYh, VYh, &TY, &TAY, 
               nhnonint, &takeallout, nh, &noptinhnonint, &noptinh);
               
        /* Acceptation ou rejet des nouvelles bornes */
        if(NhOK == 1 && nhOK == 1){
    			/* test sur le critère à optimiser (n ou RRMSE) : a-t-il diminué? */
  				if (*idoptinh) { /* Si on veux calculer le critère sur les nh entiers */
            if (!R_FINITE(noptinh)){
              accept = 0;
            } else if ( noptinh < optinh) { /* si noptinh est positif et plus petit que *optinh, */
  						accept = 1;             /* on change les bornes */
  					} else if (noptinh == optinh) { /* si noptinh est égale à *optinh, */
  					    /* on va comparer les critère calculé sur les nhnonint */       
  						if (noptinhnonint < optinhnonint) accept = 1; else accept = 0;
  					} else {     /* si noptinh est plus grand que optinh, */					
  						accept = 0;  /* c'est certain qu'on ne change pas les bornes */
  					} 
  				} else { /* Si on veux calculer le critère sur les nhnonint (non entiers) */
            if (!R_FINITE(noptinhnonint)){
              accept = 0;
            } else {
              if ( noptinhnonint < optinhnonint) accept = 1; else accept = 0;
            }
  				}
  				/* fin du test sur le critère à optimiser */          
        } else {
          /* Si les conditions sur les Nh et les nh ne sont pas respectées, on n'accepte pas les nouvelles bornes */
          accept = 0;
        }

/*Rprintf(" : %d ",accept); 
Rprintf("\n");*/

      	/* Actions posées dépendamment de l'acceptation ou du rejet des nouvelles bornes */
    		if(accept==1) {
          /* On vérifie d'abord si on doit changer la valeur de maxstep et maxstill,
             car lorsque l'algorithme se raproche de la convergence, on veut un petit maxstep
             afin de minimiser le temps de calcul. */
            diffrelopti = (optinhnonint - noptinhnonint)/optinhnonint;
            if (istill >= 50 && (step <= 3 && step >= -3) && diffrelopti < 0.001){ 
            /* La règle arbitraire que je me suis donné est donc : 
               si on a dû faire plusieurs (plus de 50) itérations sans changement avant d'accepter 
               les nouvelles bornes et
               si le pas accepté est petit (inférieur à 3) en valeur absolue et
               si le changement relatif dans le critère d'optimisation sur les nhnonint est petit 
               (inférieur à 0.001),
               alors on change le maxstep pour 3 et le maxstill pour 50 (suggestions initiales de Kozak
               dans son article de 2006 dans Techniques d'enquête).*/
            maxstep = 3;
            maxstill = 50;
          }
          /* Le compteur d'itérations sans acceptation de nouvelles bornes est réinitialisé à zéro. */
    			istill = 0;
          /* Les bornes et les critères d'optimisation doivent être mis à jour */
    			for (j=0; j < *L-1; j++) pbh[j] = npbh[j];
    			optinh = noptinh;
    			optinhnonint = noptinhnonint;
          /* On doit enregistrer les résultats dans iterdetail */
          /* Rappel utile à la programmation ici -> iterdetail : éléments et leur positions
             (    bh, optinh, optinhnonint, takeall, step, iter, run)
             (0->L-2,    L-1,            L,     L+1,  L+2,  L+3, L+4) */
      		for (j=0; j < *L-1; j++) iterdetail[j + irow * ncol_iter] = bhfull[j+1];
    			iterdetail[(*L - 1) + irow * ncol_iter] = optinh;
    			iterdetail[(*L + 0) + irow * ncol_iter] = optinhnonint;
      		iterdetail[(*L + 1) + irow * ncol_iter] = takeallout;
    			iterdetail[(*L + 2) + irow * ncol_iter] = step;
    			iterdetail[(*L + 3) + irow * ncol_iter] = iter + 1;
      		iterdetail[(*L + 4) + irow * ncol_iter] = rrow + 1;
          /* On vient de remplir une ligne de iterdetail, il faut donc incrémenter de 1 irow. */
        	irow = irow + 1;
    		} else {
          /* Le compteur d'itérations sans acceptation de nouvelles bornes est incrémenté de 1. */
      	  istill = istill + 1 ;
          /* Il n'y a pas d'autres actions à poser. */
    		}
        
        /* Fin de l'itération */
        iter = iter + 1;
        
    	}
      
      /* On enregistre les résultats pour les bornes finales dans rundetail  */
      /* Rappel utile à la programmation ici -> rundetail : éléments et leur positions
         (    bh, optinh, optinhnonint, takeall, niter, ibhtype,       ibh,  rep)
         (0->L-2,    L-1,            L,     L+1,   L+2,     L+3, L+4->2L+2, 2L+3) */
      /* En premier on tire  bh, optinh, optinhnonint et takeall de iterdetail */
      for (j=0; j < *L+2; j++) rundetail[j + rrow * ncol_run] = iterdetail[j + (irow - 1) * ncol_iter];
      rundetail[(*L + 2) + rrow * ncol_run] = iter;      

/*for (j=0; j < ncol_run; j++) Rprintf("%f  ",rundetail[j + rrow * ncol_run]); Rprintf("\n");*/

      /* On vient de remplir une ligne de rundetail, il faut donc incrémenter de 1 rrow. */
      rrow = rrow + 1;

    }
    
  }
  
  /* Au terme des calculs on connait le nombre de lignes de la version matrice de iterdetail */
  *nrowiter = irow;
  
}


