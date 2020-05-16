/*
** The author of this program disclaims copyright.
*/

%include {
   with Ada.Text_IO;
   --#include <stdio.h>
   --#include <stdlib.h> /* malloc, free */
   --#include "calc.h"
}

%token_type {int}

%left PLUS MINUS.
%left DIVIDE TIMES.

%syntax_error {
    Ada.Text_IO.Put_Line ("Syntax error!");
}

program ::= expr(A). {
    Ada.Text_IO.Put_Line ("Result=" & A.Image);
}

expr(A) ::= expr(B) MINUS expr(C). {
 A := B - C;
}

expr(A) ::= expr(B) PLUS expr(C). {
 A := B + C;
}

expr(A) ::= expr(B) TIMES expr(C). {
 A := B * C;
}

expr(A) ::= expr(B) DIVIDE expr(C). {
    if C /= 0 then
	   A := B / C;
    else
	   Ada.Text_IO.Put_Line ("A divide by zero detected");
	end if;
}

/* Misspelling og integer because of Ada clash */
expr(A) ::= INTEGR(B). {
 A := B;
}

/* Local Variables: */
/* c-basic-offset: 4 */
/* tab-width: 4 */
/* indent-tabs-mode: t */
/* mode: lemon */
/* End: */
