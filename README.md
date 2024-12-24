## Résumé
Le package `aquatools` offre de multiples outils de traitement de données relatives à la gestion et à l'étude des milieux aquatiques.

## Installation
Les commandes à utiliser pour installer le package sont les suivantes :
``` r
install.packages("pak")
pak::pak("jbfagotfede39/aquatools")
```

## Organisation
Les fonctions sont organisées avec des préfixes, permettant d'établir de grands groupes :

- `chronique` : les fonctions permettant de traiter des données de chroniques thermiques, piézométriques, oxygène dissous, etc.
- `formatage` : fonctions de reformatage des données, notamment temporelles
- `hydrovu` : fonctions de traitement des données issues de l'API Hydrovu
- `PC` : fonctions de traitements des données physico-chimiques
- `poissons` : fonctions de traitements des données piscicoles
- `SIG` : fonctions de traitements des données géomatiques
- `topographie` : fonctions de traitements des données topographiques

## Recevoir de l'aide
Si vous rencontrez un bug clair, merci de créer un ticket avec un exemple minimal reproductible sur [GitHub](https://github.com/jbfagotfede39/aquatools/issues).

## License
[GNU GPL v3](https://www.r-project.org/Licenses/GPL-3)