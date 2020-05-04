/*
** Emit Ada packages.
*/

#ifndef __EMIT_ADA_H__
#define __EMIT_ADA_H__


/* Generate parser body */
void ReportTable_Ada (struct lemon *lemp,
                      int mhflag,     /* Output in makeheaders format if true */
                      int sqlFlag);   /* Generate the *.sql file too */

/* Generate parser specification */
void ReportHeader_Ada (struct lemon *);

/* Print a LINE comment to the output file. */
void tplt_linedir_Ada (FILE *out, int lineno, char *filename);

/*
** Generate code which executes when the rule "rp" is reduced.  Write
** the code to "out".  Make sure lineno stays up-to-date.
*/
PRIVATE void emit_code_Ada (FILE *out,
                            struct rule *rp,
                            struct lemon *lemp,
                            int *lineno);

/*
** Print the definition of the union used for the parser's data stack.
** This union contains fields for every possible data type for tokens
** and nonterminals.  In the process of computing and printing this
** union, also set the ".dtnum" field of every terminal and nonterminal
** symbol.
*/
void print_stack_union_Ada(
  FILE *out,                  /* The output stream */
  struct lemon *lemp,         /* The main info structure for this parser */
  int *plineno,               /* Pointer to the line number */
  int mhflag                  /* True if generating makeheaders output */
                           );

/*
** Write and transform the rp->code string so that symbols are expanded.
** Populate the rp->codePrefix and rp->codeSuffix strings, as appropriate.
**
** Return 1 if the expanded code requires that "yylhsminor" local variable
** to be defined.
*/
PRIVATE int translate_code_Ada (struct lemon *lemp,
                                struct rule *rp);

#endif /* __EMIT_ADA_H__ */
