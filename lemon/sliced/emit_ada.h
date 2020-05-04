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


#endif /* __EMIT_ADA_H__ */
