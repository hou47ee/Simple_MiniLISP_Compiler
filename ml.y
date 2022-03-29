%{	
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
void yyerror(const char *message);

char *put(char *,char *,char *,char *,char *); //紀錄fn body
typedef struct def{	//定義variable用的struct
    char* var; //放variable名
    char* val; //放varible值
}def;
struct def *node[100];
int cn = 0; //count varible
typedef struct fn{	//定義function用的struct
	char* name;
	struct def *pa[10]; //參數
	int cpn;  //count parameter
	char* op; //儲存fn body
}fn;
struct fn *fnode[50];
int cfn = 0;  //count function
bool f = false; //判斷是否在跑FUN-EXP
int num[50]; //運算用
int n = 0;	 //count num
int ifvalue; //跑IF-EXP時儲存output值用的
bool iff = false; //判斷是否在跑IF-EXP
%}
%union{
char *cval; //因為要記錄fn, 所以都用char
}
%token <cval> ID pnum pbool define plus minus mul divide mod great small equ and or not If Num fun bval
%type  <cval> STMT DEF-STMT PRINT-STMT EXP NUM-OP PLUS MINUS MULTIPLY DIVIDE MODULUS GREATER SMALLER EQUAL EXPP EXPM LOGICAL-OP EXPA AND-OP EXPO OR-OP NOT-OP IF-EXP TEST-EXP THAN-EXP ELSE-EXP FUN-BODY PARAM FUN-EXP FUN-CALL VARIABLE PARAMETER FUN-IDs '(' ')'
%%
					//跑完每個式子都要把紀錄num的地方歸零
STMT	: EXP		{n = 0;if(iff){iff=false;}}
		| EXP {n = 0;if(iff){iff=false;}} STMT  
		| DEF-STMT
		| DEF-STMT STMT 
		| PRINT-STMT
		| PRINT-STMT STMT
		;
					//儲存要運算的數字
EXP	: Num	{if(!f){num[n++]=atoi($1);}else{$$ = put($1," ","","","");}}
	| bval	{num[n++]=atoi($1);}
	| VARIABLE	{if(!f)
				 {
					int i;
					for(i = 0; i < cn; i++)
					{
						if(!strcmp(node[i]->var,$1)) //看是否有儲存過的variable,一樣會回傳0
						{
							$$ = node[i]->val; 
							break;
						}
					}
				  }
				  else
				  {
					$$ = put($1," ","","","");
				  }
				 }
	| NUM-OP	
	| LOGICAL-OP
	| IF-EXP   {iff = true;}
	| FUN-EXP 
	| FUN-CALL {f = false;}
	;

PRINT-STMT	: '('pnum EXP')'	{if(!iff){printf("%d\n",num[n-1]);n--;}
								 else{printf("%d\n",ifvalue);n--;}
								 }
			| '('pbool EXP')'	{if(!iff){if(num[n-1]){printf("#t\n");}else{printf("#f\n");}n--;}
								 else{if(ifvalue){printf("#t\n");}else{printf("#f\n");}n--;}
								}
			;
DEF-STMT	: '('define VARIABLE EXP')'	{int i;
										 for(i = 0; i < cn; i++)	//不能重複宣告
										 {
											if(!strcmp(node[i]->var,$3))
											{
												printf("syntax error\n");
												return 0;
											}
										 }
										 node[cn] = (def*)malloc(sizeof(def));
										 node[cn]->var = $3;
										 node[cn]->val = $4;
										 cn++;
										 };
VARIABLE	: ID ;

NUM-OP	: PLUS 
		| MINUS 
		| MULTIPLY 
		| DIVIDE 
		| MODULUS 
		| GREATER
  		| SMALLER 
		| EQUAL
		;
EXPP	: EXPP EXP	{if(!f){num[n-2]=num[n-2]+num[n-1];n--;}
					 else{
					 	   fnode[cfn]->op = put($1,$2,"","","");
						 }
					};
		| EXP		
		;
EXPM	: EXPM EXP	{if(!f){num[n-2]=num[n-2]*num[n-1];n--;}
					 else{
						   fnode[cfn]->op = put($1,$2,"","","");
						 }
					};
		| EXP		
		;

PLUS		: '('plus EXP EXPP')'	{if(!f){num[n-2]=num[n-2]+num[n-1];n--;}
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };
MINUS		: '('minus EXP EXP')'   {if(!f){num[n-2]=num[n-2]-num[n-1];n--;}
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };
MULTIPLY	: '('mul EXP EXPM')' 	{if(!f){num[n-2]=num[n-2]*num[n-1];n--;}
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };
DIVIDE		: '('divide EXP EXP')'  {if(!f){num[n-2]=num[n-2]/num[n-1];n--;}
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };
MODULUS	: '('mod EXP EXP')'			{if(!f){num[n-2]=num[n-2]%num[n-1];n--;}
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };
GREATER	: '('great EXP EXP')'       {if(!f){
											if(num[n-2] > num[n-1]){num[n-2] = 1;}else{num[n-2] = 0;}
											n--;
										   }
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };
SMALLER	: '('small EXP EXP')'		{if(!f){
											if(num[n-2] < num[n-1]){num[n-2] = 1;}else{num[n-2] = 0;}
											n--;
										   }
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };

EQUAL	: '('equ EXP EXP')'         {if(!f){
											if(num[n-2] == num[n-1]){num[n-2] = 1;}else{num[n-2] = 0;}
											n--;
										   }
									 else{
											fnode[cfn]->op = put("(",$2,$3,$4,")");
										 }
								    };

LOGICAL-OP	: AND-OP	  
			| OR-OP 	  
			| NOT-OP	  
			;
EXPA	: EXPA EXP	{num[n-2]=num[n-2]&num[n-1];n--;}
		| EXP		
		;
AND-OP		: '('and EXP EXPA')' {num[n-2]=num[n-2]&num[n-1];n--;};

EXPO		: EXPO EXP	{num[n-2]=num[n-2]|num[n-1];n--;};
			| EXP		
			;
OR-OP		: '('or EXP EXPO')' {num[n-2]=num[n-2]|num[n-1];n--;};

NOT-OP		: '('not EXP')' 	{num[n-1]=!num[n-1];};

IF-EXP		: '('If TEST-EXP THAN-EXP ELSE-EXP')'	{
														if(num[n-3]){ifvalue = num[n-2];num[n-3]=num[n-2];}
														else{ifvalue= num[n-1];num[n-3]=num[n-1];}
														n-=2;
													};

TEST-EXP	: EXP ;
THAN-EXP	: EXP ;
ELSE-EXP	: EXP ;

FUN-EXP		: '('fun FUN-IDs FUN-BODY')'  

FUN-IDs		: '('PARAMETER')' {$$ = $2;f = true;}	
			| '(' ')'	{f = true;}	  
			;

FUN-BODY	: EXP;

PARAMETER	: ID PARAMETER{	 
							 int j;
							 for(j = 0; j < fnode[cfn]->cpn; j++)	//不能重複宣告
							 {
								if(!strcmp(fnode[cfn]->pa[j]->var,$1))
								{
									printf("syntax error\n");
									return 0;
								}
							 }
							 fnode[cfn]->pa[fnode[cfn]->cpn] = (def*)malloc(sizeof(def));
							 fnode[cfn]->pa[fnode[cfn]->cpn]->var = $1;
							 fnode[cfn]->cpn++;
						  }	
			| ID		{	 
							 int j;
							 for(j = 0; j < fnode[cfn]->cpn; j++)	//不能重複宣告
							 {
								if(!strcmp(fnode[cfn]->pa[j]->var,$1))
								{
									printf("syntax error\n");
									return 0;
								}
							 }
							 fnode[cfn]->pa[fnode[cfn]->cpn] = (def*)malloc(sizeof(def));
							 fnode[cfn]->pa[fnode[cfn]->cpn]->var = $1;
							 fnode[cfn]->cpn++;
						}
			;
FUN-CALL	: '('FUN-EXP PARAM')' ;

PARAM		: EXP PARAM	{fnode[cfn]->pa[fnode[cfn]->cpn-1]->val = $1;fnode[cfn]->cpn--;}
			| EXP		{fnode[cfn]->pa[fnode[cfn]->cpn-1]->val = $1;fnode[cfn]->cpn--;} //參數值會從後面往前放
			;



%%
void yyerror(const char *message)
{
	fprintf(stderr,"%s\n",message);
}
char *put(char *n1,char *n2,char *n3,char *n4,char *n5)
{
	int tlen = strlen(n1) + strlen(n2) + strlen(n3) +  strlen(n4) +  strlen(n5) + 1;
	char *out = malloc(sizeof(char)*tlen);
	int i = 0;
	int j = 0;
    for(j=0; n1[j]!='\0'; j++)
        out[i++] = n1[j];
    for(j=0; n2[j]!='\0'; j++)
        out[i++] = n2[j];
	for(j=0; n3[j]!='\0'; j++)
        out[i++] = n3[j];
	for(j=0; n4[j]!='\0'; j++)
        out[i++] = n4[j];
	for(j=0; n5[j]!='\0'; j++)
        out[i++] = n5[j];
    out[i] = '\0';
	return out;
}
int main()
{
	fnode[cfn] = (fn*)malloc(sizeof(fn));
	fnode[cfn]->cpn = 0;
	yyparse();
	return 0;
}