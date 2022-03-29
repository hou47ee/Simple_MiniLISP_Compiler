%{
#include "ml.tab.h"
%}
sep	[ \t\n\r]+
num	0|([1-9][0-9]*)|(-[1-9][0-9]*)
id	[a-z][a-z0-9-]*

%%
{sep}		{}
"print-num"		{yylval.cval=strdup(yytext);return(pnum);}
"print-bool"	{yylval.cval=strdup(yytext);return(pbool);}
"define"		{yylval.cval=strdup(yytext);return(define);}
"+"		{yylval.cval=strdup(yytext);return(plus);}
"-"		{yylval.cval=strdup(yytext);return(minus);}
"*"		{yylval.cval=strdup(yytext);return(mul);}
"/"		{yylval.cval=strdup(yytext);return(divide);}
"mod"	{yylval.cval=strdup(yytext);return(mod);}
">"		{yylval.cval=strdup(yytext);return(great);}
"<"		{yylval.cval=strdup(yytext);return(small);}
"="		{yylval.cval=strdup(yytext);return(equ);}
"#t"		{yylval.cval="1"; return(bval);}
"#f"		{yylval.cval="0"; return(bval);}
"and"		{yylval.cval=strdup(yytext);return(and);}
"or"		{yylval.cval=strdup(yytext);return(or);}
"not"		{yylval.cval=strdup(yytext);return(not);}
"if"		{yylval.cval=strdup(yytext);return(If);}
"fun"		{yylval.cval=strdup(yytext);return(fun);}
{num}		{yylval.cval=strdup(yytext);return(Num);}
{id}		{yylval.cval=strdup(yytext);return(ID);}
.		{return(yytext[0]);}
%% 