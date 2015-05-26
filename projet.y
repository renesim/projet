%{
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "fonctTab.h"
int yyerror(char*);
int yylex();
void endProgram();
 FILE* yyin;
 int jump_label=0;	
 void stockage_X(void);
 void restauration_X(void);
 void inst(const char *);
 void instarg(const char *,int);
 void comment(const char *);
 TabSymboles TB;
%}

%union{
	int entier;
	char caractere;
	char chaine[BUFSIZ];
}

%token <chaine> TYPE
%token <chaine> IDENT
%token <entier> NUM 
%token <chaine> COMP
%token <caractere> CARACTERE 
%token <caractere> ADDSUB
%token <caractere> DIVSTAR
%token <chaine> BOPE
%token <caractere >NEGATION
%token <chaine> IF ELSE 
%token <caractere> EGAL PV VRG LPAR RPAR LACC RACC LSQB RSQB 
%token CONST
%token PRINT READ READCH WHILE
%token VOID RETURN 
%token MAIN

%type <entier> JUMPIF
%type <entier> JUMPELSE
%type <entier> WLABEL
%type <entier> READ 
%type <chaine> READCH

%left BOPE
%left COMP
%left ADDSUB /* ADDSUB Binaire */ 
%left DIVSTAR
%right NEGATION
%right ADDSUBU /* ADDSUB Unaire */

%left NOELSE
%left ELSE


%%
Prog : DeclConst DeclVarPuisFonct DeclMain

DeclConst : /** NULL **/ 
| DeclConst CONST ListConst PV

ListConst : ListConst VRG IDENT EGAL Litteral
| IDENT EGAL Litteral { }

Litteral : NombreSigne
| CARACTERE

NombreSigne : NUM
| ADDSUBU NUM

DeclVarPuisFonct : /** NULL **/
| TYPE ListVar PV DeclVarPuisFonct
| DeclFonct

ListVar : ListVar VRG Ident
| Ident

Ident : IDENT Tab

Tab : /** NULL **/
| Tab LSQB NUM RSQB

DeclMain : EnTeteMain Corps

EnTeteMain : MAIN LPAR RPAR

DeclFonct : DeclFonct DeclUneFonct
| DeclUneFonct

DeclUneFonct : EnTeteFonct Corps

EnTeteFonct : TYPE IDENT LPAR Parametres RPAR
| VOID IDENT LPAR Parametres RPAR

Parametres : VOID
| ListTypVar

ListTypVar : ListTypVar VRG TYPE IDENT
| TYPE IDENT

Corps : LACC DeclConst DeclVar SuiteInstr RACC

DeclVar : /** NULL **/
| DeclVar TYPE ListVar PV

SuiteInstr : /** NULL **/
| SuiteInstr Instr

InstrComp : LACC SuiteInstr RACC

JUMPIF :  
  {
    instarg("JUMPF", jump_label);
    $$ = jump_label++;
   };

JUMPELSE :  
  {
    instarg("JUMP", jump_label);
    $$ = jump_label++;
   };

WLABEL :  
  {
    instarg("LABEL", jump_label);
    $$ = jump_label++;
   };

Instr : LValue EGAL Exp PV
| IF LPAR Exp RPAR JUMPIF Instr %prec NOELSE { instarg("LABEL",$5);}
| IF LPAR Exp RPAR JUMPIF Instr ELSE JUMPELSE { instarg("LABEL",$5);} Instr { instarg("LABEL",$8);}
| WLABEL WHILE LPAR Exp RPAR JUMPIF Instr {instarg("JUMP",$1); instarg("LABEL",$6);}
| RETURN Exp PV {inst("POP"); inst("RETURN"); inst("PUSH");}
| RETURN PV { inst("RETURN"); }
| IDENT LPAR Arguments RPAR PV 
| READ LPAR IDENT RPAR PV {getVal(&TB,$3,)}
| READCH LPAR IDENT RPAR PV
| PRINT LPAR Exp RPAR PV {inst("POP"); inst("WRITE");}
| PV
| InstrComp

Arguments : /** NULL **/
| ListExp

LValue : IDENT TabExp

TabExp : /** NULL **/
| TabExp LSQB Exp RSQB

ListExp : ListExp VRG Exp
| Exp

Exp : 
 Exp ADDSUB Exp {
 	inst("POP");
 	inst("SWAP");
 	inst("POP");
 	if($2 == '+'){
 		inst("ADD");
	}
 	else{ 
 		inst("SUB");
    }
  inst("PUSH");
 }

| Exp DIVSTAR Exp{
 	inst("POP");
 	inst("SWAP");
 	inst("POP");
 	if($2 == '*'){
 		inst("MUL");
	}
 	else{ 
 		inst("DIV");
    }
  inst("PUSH");
 }

| Exp COMP Exp{
  inst("POP");
  inst("SWAP");
  inst("POP");
  if(strcmp($2,"==") == 0)
    inst("EQUAL");
  else if(strcmp($2,"!=") == 0)
    inst("NOTEQ");
  else if(strcmp($2,"<") == 0)
    inst("LESS");
  else if(strcmp($2,"<=") == 0)
    inst("LEQ");
  else if(strcmp($2,">") == 0)
    inst("GREATER");
  else if(strcmp($2,">=") == 0)
    inst("GEQ");
  inst("PUSH");
}
| ADDSUB Exp %prec ADDSUBU{
  if($1 == '-'){
    inst("POP");
    inst("SWAP"); 
    instarg("SET",-1);
    inst("MUL");
    inst("PUSH");
  }
}
| Exp BOPE Exp{   /* 0 : FALSE  1 : TRUE */
  inst("POP");
  inst("SWAP");
  inst("POP");
  inst("ADD");
  inst("SWAP");
  if(strcmp($2,"||")){
    instarg("SET",0);
    inst("GREATER");
  }
  else if(strcmp($2,"&&")){
    instarg("SET",2);
    inst("EQUAL");
  }
}
| NEGATION Exp{
  inst("POP");
  inst("SWAP");
  instarg("SET",1);
  inst("ADD");
  instarg("SET",2);
  inst("SWAP");
  inst("MOD");
}
| LPAR Exp RPAR
| LValue
| NUM{
    instarg("SET",$1);
    inst("PUSH");
  }
| CARACTERE
  {
    instarg("SET",$1);
    inst("PUSH");
  }
| IDENT LPAR Arguments RPAR

%%
void stockage_X(){
  inst("SET 0");
  inst("SWAP");
  inst("POP");
  inst("SAVE");
}

void restauration_X(){
  inst("SET 0");
  inst("LOAD");
  inst("PUSH");
}

int yyerror(char* s) {
  fprintf(stderr,"%s\n",s);
  return 0;
}

void endProgram() {
  printf("HALT\n");
}

void inst(const char *s){
  printf("%s\n",s);
}

void instarg(const char *s,int n){
  printf("%s\t%d\n",s,n);
}


void comment(const char *s){
  printf("#%s\n",s);
}

int main(int argc, char** argv){
  TB.index = 0;
	yyparse();
  endProgram();
	return 0;
}
