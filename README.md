
___________________________________________________________________________________________________

# `ORCHAMP` - Définition de la campagne d'échantillonnage
___________________________________________________________________________________________________

### Paramètres
#### Donnés en entrée
- années de départ et de fin d'échantillonnage
- `n` : nombre de sites à échantillonner par année
- contraintes d'échantillonnage
- contraintes d'association
- facteurs de réduction/augmentation des probabilités d'échantillonnage
- 

#### Inférés à partir des paramètres d'entrée
- l'ensemble des combinaisons de `n` sites :
    - moins les combinaisons ne respectant pas les contraintes d'échantillonnage
    - moins les combinaisons ne respectant pas les contraintes d'association

___________________________________________________________________________________________________

### Initialisation
- du tableau recensant pour chaque site :
    - la dernière année à laquelle il a été échantillonné
    - le nombre d'année successives d'échantillonnage
- du tableau recensant pour chaque combinaison de sites :
    - sa probabilité d'etre échantillonnée (initialisée à 1)
    - si elle peut etre échantillonnée cette année là (initialisé à 'oui')

___________________________________________________________________________________________________

### Sélection annuelle
1. Tirer une combinaison parmi celle disponibles
2. Si première année d'échantillonnage, initialiser `LAST_YEAR` à `year.start - 1`

Pour chaque site :
- si sélectionné cette année là :
    1. enregistrer cette année comme étant la dernière échantillonnée pour ce site (`LAST_YEAR = year.current`)
    2. incrémenter de 1 le nombre d'années successives d'échantillonnage pour ce site (`NB_SUCC_SAMP ++`)
    3. réduire la probabilité de rééchantillonner ce site
- sinon :
    1. mettre à 0 le nombre d'années successives d'échantillonnage pour ce site (`NB_SUCC_SAMP = 0`)
    2. augmenter la probabilité d'échantillonner ce site si pas échantillonné depuis X années

Pour l'échantillonnage de l'année suivante, retire les combinaisons comportant au moins 4 sites échantillonnés cette année.
    
___________________________________________________________________________________________________
