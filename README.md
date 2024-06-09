
					COMPILATEUR PASCAL
					
					
	
	Ajouts personnels:
	- Ajout de la boucle FOR(TO|DOWNTO)
	- Ajout du CASE OF(fonctionnel)
	- AJOUT du REPEAT UNTIL(fonctionnel)
	- string special_keyword(const char *kw, const char *kw2);(dérivé de ReadKeyword pour 2 mots clés)
	
	Fonctions ajoutés(Fonctionnel):
	 	(CASE OF)
	 Fonctions:	
		- void caselabellist(void);
		- void casestatement();
		- void caselistelement();
	Grammaire:
	- <case label list> ::= <constant> {, <constant> } 
	- <case statement> ::= case <expression> of <case list element> {; <case list element> } end 
	- <case list element> ::= <case label list> : <statement> |  -> le empty a été supprimé

		(FOR)
	Fonctions:
		- void Forstatement(void)
	Grammaire:
		- ForStatement := "For" ID ":=" Expression ("TO"|"DOWNTO") Expression "DO" Statement

		(REPEAT UNTIL)
	Fonctions:
		- void	repeatStatement(void)
	Grammaire: <repeat statement> ::= repeat <statement> {; <statement>} until <expression>
		
	Ajouts(Non fonctionnel)
	 	(SET)
	Fonctions:
		- set()
		- elementlist()
		- element()
	Grammaire:
		- <set> ::= [ <element list> ]
		- <element list> ::= <element> {, <element> } | <empty>
		- <element> ::= <expression> | <expression> .. <expression> 
	
Remarque: J'ai voulu un peu de temps avant le rendu commencer l'implementation du set, mais je n'ai malheuresement pas eu le temps, de ce fait j'ai pu à peine commencer


		MODIFICATION DES FONCTIONS/FICHIERS/VARIABLES:
		
		Statement()
		- J'ai modifié la fonction Statement pour y ajouter les conditions pour:
			- FOR
			- CASE OF
			- REPEAT UNTIL
			
		Fichiers:
			Tokeniser.l s'est vu ajouté FOR|CASE|OF|REPEAT|UNTIL
		
		Variables Globales:
			Ajout - TagNumber : ATTRIBUTION DE NUMEROS GLOBALEMENT(Ex FOR)
			Ajout - CPT_LABEL : ATTRIBUTION DE NUMEROS AUX LABELS DE (CASE OF)
			AJOUT - CPT_CASE_LIST : ATTRIBUTION DE NUMEROS AUX CASES DE (CASE OF)
		
		

Note global:
	Je n'ai pu finir ma version personnelle du compilateur de ce fait la version utilisé pour réaliser mon projet est celle de Dr.Pierre Jourlin : https://framagit.org/jourlin/cericompiler
	
	- Le projet avez pour but d'utiliser nos connaissances acquérient lors du semestre 3 dans le cours "Théorie des langages" dispensé par monsieur Dr.Stephane Huet afin de construire pas à pas  un compilateur en C/C++ qui traduira un programme écrit dans un langage pascal simplifié en assembleur 80x86 64 bits en utilisant la syntaxte AT&T, puis en exécutable ELF. 
	
	Ce projet a été fait en essayant de respecter au possible la grammaire du langage pascal:	 https://condor.depaul.edu/ichu/csc447/notes/wk2/pascal.html
	
	Site ayant aidé à comprendre la logique du langage pascal: https://www.freepascal.org/docs-html/ref/refsu56.html#x166-19000013.2.2
	
