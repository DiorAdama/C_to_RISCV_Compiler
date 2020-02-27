# ALPAGA (An Ll(1) PArser GenerAtor)

ALPAGA est un générateur d'analyseurs syntaxiques LL(1).

Le format d'entrée (simple) est le suivant :

Premièrement, on définit les terminaux du langage :

'''
tokens SYM_EOF SYM_IF SYM_IDENTIFIER<string> SYM_INTEGER<int> SYM_PLUS SYM_MINUS
tokens SYM_ASTERISK SYM_DIV
'''

Les "SYM_XXX" correspondent aux symboles définis dans src/symbols.ml

Lorsqu'un symbole n'est pas constant (i.e. est paramétré par une chaîne de
caractères ou un entier, n indique son type entre chevrons < et >.

Ensuite, on définit les non-terminaux du langage :

'''
non-terminals S EXPR TERM FACTOR
non-terminals EXPRS TERMS
'''

Puis le non-terminal distingué qui sert d'axiome à la grammaire :

'''
axiom S
'''

Ensuite le mot clé 'rules' indique le début des règles de la grammaire

'''rules'''

Une règle est simplement de la forme 'N -> SYM_TRUC AUTRE_NON_TERMINAL SYM_MACHIN'

Par exemple,

'''
S -> EXPR SYM_EOF
EXPR ->  TERM EXPRS
EXPRS -> SYM_PLUS TERM EXPRS
EXPRS -> SYM_MINUS TERM EXPRS
EXPRS ->
'''

La dernière ligne de cette grammaire indique une règle qui produit le mot vide
(Epsilon) pour le non-terminal EXPRS.

Vous pouvez lancer ALPAGA avec la commande suivante :

'''
./ml_parser_generator -g <votre_fichier_de_grammaire.g> -t <table.html>
'''

Cela va analyser votre grammaire et produire un fichier HTML, que vous pouvez
ouvrir avec un navigateur web, et qui contient les tables Null, First et Follow
relatives à votre grammaire.

Ce fichier contient aussi la table de prédiction LL(1) avec dans chaque case
(NT, t) la règle à appliquer lorsque l'analyseur doit reconnaître le
non-terminal NT, et a le lexème t en entrée.

Si la case est vide, il s'agit d'une erreur de syntaxe : on ne s'attendait pas à
voir ce lexème-ci dans cet état.

Si la case contient plusieurs règles, c'est un conflit : votre grammaire est
ambigüe. Sans doute avez-vous utilisé de la récursivité à gauche ?

Dans la case (NT,t), une règle affichée en bleu signifie que cette règle est
présente car t appartient à First(NT), tandis qu'une règle affichée en rouge
signifie que NT est Nullable et que t appartient à Follow(NT).


## Actions dans la grammaire

Maintenant que vous avez une grammaire sans conflits, il est temps de générer un
arbre de syntaxe abstraite. Pour ce faire, vous avez deux choses à faire :

- sur les lignes avant le mot-clé 'rules', insérez un bloc de code entre
  accolades, qui sera copié au début du code source généré pour l'analyseur
  syntaxique. (en fait dans le squelette qui vous est fourni, ce bloc de code
  est déjà présent, et vous n'avez qu'à le remplir.)
  
- après chaque règle 'X -> w1 w2 ... wn', ajoutez du code entre accolades qui
  construit le sous-arbre correspondant à la dérivation effectuée par cette
  règle. On appelle ce code une **action**.
  
  Le code que vous écrivez correspond donc à la construction d'un terme OCaml.
  
  Par exemple, le morceau suivant vous est fourni :
  '''
  S -> GLOBDEF SYM_EOF {  Node (Tlistglobdef, [$1]) }
  IDENTIFIER -> SYM_IDENTIFIER {  StringLeaf ($1) }
  INTEGER -> SYM_INTEGER { IntLeaf ($1) }
  GLOBDEF -> IDENTIFIER SYM_LPARENTHESIS LPARAMS SYM_RPARENTHESIS INSTR {
      let fargs = $3 in
      let instr = $5 in
      Node (Tfundef, [$1; Node (Tfunargs, fargs) ; instr ])
  }
  '''
  
  Pour une règle X -> w1 w2 ... wn, les variables $i correspondent aux actions
  générées par le symbole wi. Par exemple, dans l'action de la première règle,
  la variable $1 correspond à l'arbre rendu par le non-terminal GLOBDEF.
  
  Les définitions des arbres et nœuds sont trouvées dans le fichier src/ast.ml.
  
  Ces actions vont servir à générer le parser, ce qui se fera avec la commande :
  
  '''
./ml_parser_generator -g <votre_fichier_de_grammaire.g> -t <table.html> -pml <generated_parser.ml>
  '''
  
  ce qui créera le fichier <generated_parser.ml> à l'endroit où vous l'avez
  indiqué.
  
  En fait, les différents Makefile de ce projet font que vous n'aurez
  normalement pas à écrire cette commande à la main : un simple 'make test' à la
  racine de ce projet devrait faire tout ce dont vous avez besoin.
