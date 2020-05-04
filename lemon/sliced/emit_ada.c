/*
** Emit Ada packages.
*/


/* Generate parser body */
void ReportTable_Ada (
  struct lemon *lemp,
  int mhflag,     /* Output in makeheaders format if true */
  int sqlFlag     /* Generate the *.sql file too */
){
  FILE *out, *in, *sql;
  int  lineno;
  struct state *stp;
  struct action *ap;
  struct rule *rp;
  struct acttab *pActtab;
  int i, j, n, sz;
  int nLookAhead;
  int szActionType;     /* sizeof(YYACTIONTYPE) */
  int szCodeType;       /* sizeof(YYCODETYPE)   */
  const char *name;
  int mnTknOfst, mxTknOfst;
  int mnNtOfst, mxNtOfst;
  struct axset *ax;

  lemp->minShiftReduce = lemp->nstate;
  lemp->errAction = lemp->minShiftReduce + lemp->nrule;
  lemp->accAction = lemp->errAction + 1;
  lemp->noAction = lemp->accAction + 1;
  lemp->minReduce = lemp->noAction + 1;
  lemp->maxAction = lemp->minReduce + lemp->nrule;

  in = tplt_open(lemp);
  if( in==0 ) return;
  out = file_open (lemp, ".adb", "wb");
  if( out==0 ){
    fclose(in);
    return;
  }
  if( sqlFlag==0 ){
    sql = 0;
  }else{
    sql = file_open(lemp, ".sql", "wb");
    if( sql==0 ){
      fclose(in);
      fclose(out);
      return;
    }
    fprintf(sql,
       "BEGIN;\n"
       "CREATE TABLE symbol(\n"
       "  id INTEGER PRIMARY KEY,\n"
       "  name TEXT NOT NULL,\n"
       "  isTerminal BOOLEAN NOT NULL,\n"
       "  fallback INTEGER REFERENCES symbol"
               " DEFERRABLE INITIALLY DEFERRED\n"
       ");\n"
    );
    for(i=0; i<lemp->nsymbol; i++){
      fprintf(sql,
         "INSERT INTO symbol(id,name,isTerminal,fallback)"
         "VALUES(%d,'%s',%s",
         i, lemp->symbols[i]->name,
         i<lemp->nterminal ? "TRUE" : "FALSE"
      );
      if( lemp->symbols[i]->fallback ){
        fprintf(sql, ",%d);\n", lemp->symbols[i]->fallback->index);
      }else{
        fprintf(sql, ",NULL);\n");
      }
    }
    fprintf(sql,
      "CREATE TABLE rule(\n"
      "  ruleid INTEGER PRIMARY KEY,\n"
      "  lhs INTEGER REFERENCES symbol(id),\n"
      "  txt TEXT\n"
      ");\n"
      "CREATE TABLE rulerhs(\n"
      "  ruleid INTEGER REFERENCES rule(ruleid),\n"
      "  pos INTEGER,\n"
      "  sym INTEGER REFERENCES symbol(id)\n"
      ");\n"
    );
    for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
      assert( i==rp->iRule );
      fprintf(sql,
        "INSERT INTO rule(ruleid,lhs,txt)VALUES(%d,%d,'",
        rp->iRule, rp->lhs->index
      );
      writeRuleText(sql, rp);
      fprintf(sql,"');\n");
      for(j=0; j<rp->nrhs; j++){
        struct symbol *sp = rp->rhs[j];
        if( sp->type!=MULTITERMINAL ){
          fprintf(sql,
            "INSERT INTO rulerhs(ruleid,pos,sym)VALUES(%d,%d,%d);\n",
            i,j,sp->index
          );
        }else{
          int k;
          for(k=0; k<sp->nsubsym; k++){
            fprintf(sql,
              "INSERT INTO rulerhs(ruleid,pos,sym)VALUES(%d,%d,%d);\n",
              i,j,sp->subsym[k]->index
            );
          }
        }
      }
    }
    fprintf(sql, "COMMIT;\n");
  }
  lineno = 1;
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the include code, if any */
  tplt_print(out,lemp,lemp->include,&lineno);
  if( mhflag ){
    char *incName = file_makename(lemp, ".h");
    fprintf(out,"#include \"%s\"\n", incName); lineno++;
    free(incName);
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  fprintf(out,"package body Calc_Ada is\n"); lineno++;

  /* Generate #defines for all tokens */
  if( mhflag ){
    const char *prefix;
    fprintf(out,"#if INTERFACE\n"); lineno++;
    if( lemp->tokenprefix ) prefix = lemp->tokenprefix;
    else                    prefix = "";
    for(i=1; i<lemp->nterminal; i++){
      fprintf(out,"#define %s%-30s %2d\n",prefix,lemp->symbols[i]->name,i);
      lineno++;
    }
    fprintf(out,"#endif\n"); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the defines */
  fprintf(out,"YYCODETYPE   : constant := %s;\n",
    minimum_size_type(0, lemp->nsymbol, &szCodeType)); lineno++;
  fprintf(out,"YYNOCODE     : constant := %d;\n",lemp->nsymbol);  lineno++;
  fprintf(out,"YYACTIONTYPE : constant := %s;\n",
    minimum_size_type(0,lemp->maxAction,&szActionType)); lineno++;
  if( lemp->wildcard ){
    fprintf(out,"YYWILDCARD : constant := %d;\n",
       lemp->wildcard->index); lineno++;
  }
  print_stack_union_Ada(out,lemp,&lineno,mhflag);
  // fprintf(out, "#ifndef YYSTACKDEPTH\n"); lineno++;
  if( lemp->stacksize ){
    fprintf(out,"YYSTACKDEPTH : constant := %s;\n",lemp->stacksize);  lineno++;
  }else{
    fprintf(out,"YYSTACKDEPTH : constant := 100;\n");  lineno++;
  }
  // fprintf(out, "#endif\n"); lineno++;
  if( mhflag ){
    fprintf(out,"#if INTERFACE\n"); lineno++;
  }
  name = lemp->name ? lemp->name : "Parse";
  if( lemp->arg && lemp->arg[0] ){
    i = lemonStrlen(lemp->arg);
    while( i>=1 && ISSPACE(lemp->arg[i-1]) ) i--;
    while( i>=1 && (ISALNUM(lemp->arg[i-1]) || lemp->arg[i-1]=='_') ) i--;
    fprintf(out,"   %sARG_SDECL : constant := %s;\n",name,lemp->arg);  lineno++;
    fprintf(out,"#define %sARG_PDECL ,%s\n",name,lemp->arg);  lineno++;
    fprintf(out,"#define %sARG_PARAM ,%s\n",name,&lemp->arg[i]);  lineno++;
    fprintf(out,"#define %sARG_FETCH %s=yypParser->%s;\n",
                 name,lemp->arg,&lemp->arg[i]);  lineno++;
    fprintf(out,"#define %sARG_STORE yypParser->%s=%s;\n",
                 name,&lemp->arg[i],&lemp->arg[i]);  lineno++;
  }else{
    fprintf(out,"procedure %sARG_SDECL is null;\n",name); lineno++;
    fprintf(out,"procedure %sARG_PDECL is null;\n",name); lineno++;
    fprintf(out,"procedure %sARG_PARAM is null;\n",name); lineno++;
    fprintf(out,"procedure %sARG_FETCH is null;\n",name); lineno++;
    fprintf(out,"procedure %sARG_STORE is null;\n",name); lineno++;
  }
  if( lemp->ctx && lemp->ctx[0] ){
    i = lemonStrlen(lemp->ctx);
    while( i>=1 && ISSPACE(lemp->ctx[i-1]) ) i--;
    while( i>=1 && (ISALNUM(lemp->ctx[i-1]) || lemp->ctx[i-1]=='_') ) i--;
    fprintf(out,"#define %sCTX_SDECL %s;\n",name,lemp->ctx);  lineno++;
    fprintf(out,"#define %sCTX_PDECL ,%s\n",name,lemp->ctx);  lineno++;
    fprintf(out,"#define %sCTX_PARAM ,%s\n",name,&lemp->ctx[i]);  lineno++;
    fprintf(out,"#define %sCTX_FETCH %s=yypParser->%s;\n",
                 name,lemp->ctx,&lemp->ctx[i]);  lineno++;
    fprintf(out,"#define %sCTX_STORE yypParser->%s=%s;\n",
                 name,&lemp->ctx[i],&lemp->ctx[i]);  lineno++;
  }else{
    fprintf(out,"procedure %sCTX_SDECL is null;\n",name); lineno++;
    fprintf(out,"procedure %sCTX_PDECL is null;\n",name); lineno++;
    fprintf(out,"procedure %sCTX_PARAM is null;\n",name); lineno++;
    fprintf(out,"procedure %sCTX_FETCH is null;\n",name); lineno++;
    fprintf(out,"procedure %sCTX_STORE is null;\n",name); lineno++;
  }
  if( mhflag ){
    fprintf(out,"#endif\n"); lineno++;
  }
  if( lemp->errsym && lemp->errsym->useCnt ){
    fprintf(out,"YYERRORSYMBOL : constant := %d;\n",lemp->errsym->index); lineno++;
    fprintf(out,"YYERRSYMDT    : constant := yy%d;\n",lemp->errsym->dtnum); lineno++;
  }
  if( lemp->has_fallback ){
    fprintf(out,"YYFALLBACK : constant := 1;\n");  lineno++;
  }

  /* Compute the action table, but do not output it yet.  The action
  ** table must be computed before generating the YYNSTATE macro because
  ** we need to know how many states can be eliminated.
  */
  ax = (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
  if( ax==0 ){
    fprintf(stderr,"malloc failed\n");
    exit(1);
  }
  for(i=0; i<lemp->nxstate; i++){
    stp = lemp->sorted[i];
    ax[i*2].stp = stp;
    ax[i*2].isTkn = 1;
    ax[i*2].nAction = stp->nTknAct;
    ax[i*2+1].stp = stp;
    ax[i*2+1].isTkn = 0;
    ax[i*2+1].nAction = stp->nNtAct;
  }
  mxTknOfst = mnTknOfst = 0;
  mxNtOfst = mnNtOfst = 0;
  /* In an effort to minimize the action table size, use the heuristic
  ** of placing the largest action sets first */
  for(i=0; i<lemp->nxstate*2; i++) ax[i].iOrder = i;
  qsort(ax, lemp->nxstate*2, sizeof(ax[0]), axset_compare);
  pActtab = acttab_alloc(lemp->nsymbol, lemp->nterminal);
  for(i=0; i<lemp->nxstate*2 && ax[i].nAction>0; i++){
    stp = ax[i].stp;
    if( ax[i].isTkn ){
      for(ap=stp->ap; ap; ap=ap->next){
        int action;
        if( ap->sp->index>=lemp->nterminal ) continue;
        action = compute_action(lemp, ap);
        if( action<0 ) continue;
        acttab_action(pActtab, ap->sp->index, action);
      }
      stp->iTknOfst = acttab_insert(pActtab, 1);
      if( stp->iTknOfst<mnTknOfst ) mnTknOfst = stp->iTknOfst;
      if( stp->iTknOfst>mxTknOfst ) mxTknOfst = stp->iTknOfst;
    }else{
      for(ap=stp->ap; ap; ap=ap->next){
        int action;
        if( ap->sp->index<lemp->nterminal ) continue;
        if( ap->sp->index==lemp->nsymbol ) continue;
        action = compute_action(lemp, ap);
        if( action<0 ) continue;
        acttab_action(pActtab, ap->sp->index, action);
      }
      stp->iNtOfst = acttab_insert(pActtab, 0);
      if( stp->iNtOfst<mnNtOfst ) mnNtOfst = stp->iNtOfst;
      if( stp->iNtOfst>mxNtOfst ) mxNtOfst = stp->iNtOfst;
    }
#if 0  /* Uncomment for a trace of how the yy_action[] table fills out */
    { int jj, nn;
      for(jj=nn=0; jj<pActtab->nAction; jj++){
        if( pActtab->aAction[jj].action<0 ) nn++;
      }
      printf("%4d: State %3d %s n: %2d size: %5d freespace: %d\n",
             i, stp->statenum, ax[i].isTkn ? "Token" : "Var  ",
             ax[i].nAction, pActtab->nAction, nn);
    }
#endif
  }
  free(ax);

  /* Mark rules that are actually used for reduce actions after all
  ** optimizations have been applied
  */
  for(rp=lemp->rule; rp; rp=rp->next) rp->doesReduce = LEMON_FALSE;
  for(i=0; i<lemp->nxstate; i++){
    for(ap=lemp->sorted[i]->ap; ap; ap=ap->next){
      if( ap->type==REDUCE || ap->type==SHIFTREDUCE ){
        ap->x.rp->doesReduce = 1;
      }
    }
  }

  /* Finish rendering the constants now that the action table has
  ** been computed */
  fprintf(out,"YYNSTATE             : constant := %d;\n",lemp->nxstate);  lineno++;
  fprintf(out,"YYNRULE              : constant := %d;\n",lemp->nrule);  lineno++;
  fprintf(out,"YYNRULE_WITH_ACTION  : constant := %d;\n",lemp->nruleWithAction);
         lineno++;
  fprintf(out,"YYNTOKEN             : constant := %d;\n",lemp->nterminal); lineno++;
  fprintf(out,"YY_MAX_SHIFT         : constant := %d;\n",lemp->nxstate-1); lineno++;
  i = lemp->minShiftReduce;
  fprintf(out,"YY_MIN_SHIFTREDUCE   : constant := %d;\n",i); lineno++;
  i += lemp->nrule;
  fprintf(out,"YY_MAX_SHIFTREDUCE   : constant := %d;\n", i-1); lineno++;
  fprintf(out,"YY_ERROR_ACTION      : constant := %d;\n", lemp->errAction); lineno++;
  fprintf(out,"YY_ACCEPT_ACTION     : constant := %d;\n", lemp->accAction); lineno++;
  fprintf(out,"YY_NO_ACTION         : constant := %d;\n", lemp->noAction); lineno++;
  fprintf(out,"YY_MIN_REDUCE        : constant := %d;\n", lemp->minReduce); lineno++;
  i = lemp->minReduce + lemp->nrule;
  fprintf(out,"YY_MAX_REDUCE        : constant := %d;\n", i-1); lineno++;
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Now output the action table and its associates:
  **
  **  yy_action[]        A single table containing all actions.
  **  yy_lookahead[]     A table containing the lookahead for each entry in
  **                     yy_action.  Used to detect hash collisions.
  **  yy_shift_ofst[]    For each state, the offset into yy_action for
  **                     shifting terminals.
  **  yy_reduce_ofst[]   For each state, the offset into yy_action for
  **                     shifting non-terminals after a reduce.
  **  yy_default[]       Default action for each state.
  */

  /* Output the yy_action table */
  lemp->nactiontab = n = acttab_action_size(pActtab);
  lemp->tablesize += n*szActionType;
  fprintf(out,"YY_ACTTAB_COUNT : constant := (%d);\n", n); lineno++;
  fprintf(out,"yy_action : constant array (Natural range <>) of YYACTIONTYPE := (\n"); lineno++;
  for(i=j=0; i<n; i++){
    int action = acttab_yyaction(pActtab, i);
    if( action<0 ) action = lemp->noAction;
    if( j==0 ) fprintf(out,"   --  %5d\n   ", i);
    fprintf(out, " %4d,", action);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "   );\n"); lineno++;

  /* Output the yy_lookahead table */
  lemp->nlookaheadtab = n = acttab_lookahead_size(pActtab);
  lemp->tablesize += n*szCodeType;
  fprintf(out,"yy_lookahead : constant array (Natural range <>) of YYCODETYPE := (\n"); lineno++;
  for(i=j=0; i<n; i++){
    int la = acttab_yylookahead(pActtab, i);
    if( la<0 ) la = lemp->nsymbol;
    if( j==0 ) fprintf(out,"   --  %5d\n   ", i);
    fprintf(out, " %4d,", la);
    if( j==9 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  /* Add extra entries to the end of the yy_lookahead[] table so that
  ** yy_shift_ofst[]+iToken will always be a valid index into the array,
  ** even for the largest possible value of yy_shift_ofst[] and iToken. */
  nLookAhead = lemp->nterminal + lemp->nactiontab;
  while( i<nLookAhead ){
    if( j==0 ) fprintf(out,"   --  %5d\n   ", i);
    fprintf(out, " %4d,", lemp->nterminal);
    if( j==9 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
    i++;
  }
  if( j>0 ){ fprintf(out, "\n"); lineno++; }
  fprintf(out, ");\n"); lineno++;

  /* Output the yy_shift_ofst[] table */
  n = lemp->nxstate;
  while( n>0 && lemp->sorted[n-1]->iTknOfst==NO_OFFSET ) n--;
  fprintf(out, "YY_SHIFT_COUNT : constant := (%d);\n", n-1); lineno++;
  fprintf(out, "YY_SHIFT_MIN   : constant := (%d);\n", mnTknOfst); lineno++;
  fprintf(out, "YY_SHIFT_MAX   : constant := (%d);\n", mxTknOfst); lineno++;
  fprintf(out, "yy_shift_ofst : constant array (Natrual range <>) of %s := (\n",
       minimum_size_type(mnTknOfst, lemp->nterminal+lemp->nactiontab, &sz));
       lineno++;
  lemp->tablesize += n*sz;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iTknOfst;
    if( ofst==NO_OFFSET ) ofst = lemp->nactiontab;
    if( j==0 ) fprintf(out,"   --  %5d\n   ", i);
    fprintf(out, " %4d,", ofst);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "   );\n"); lineno++;

  /* Output the yy_reduce_ofst[] table */
  n = lemp->nxstate;
  while( n>0 && lemp->sorted[n-1]->iNtOfst==NO_OFFSET ) n--;
  fprintf(out, "YY_REDUCE_COUNT : constant := (%d);\n", n-1); lineno++;
  fprintf(out, "YY_REDUCE_MIN   : constant := (%d);\n", mnNtOfst); lineno++;
  fprintf(out, "YY_REDUCE_MAX   : constant := (%d);\n", mxNtOfst); lineno++;
  fprintf(out, "yy_reduce_ofst : constant array (Natural range <>) of %s := (\n",
          minimum_size_type(mnNtOfst-1, mxNtOfst, &sz)); lineno++;
  lemp->tablesize += n*sz;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iNtOfst;
    if( ofst==NO_OFFSET ) ofst = mnNtOfst - 1;
    if( j==0 ) fprintf(out,"   --  %5d\n   ", i);
    fprintf(out, " %4d,", ofst);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "   );\n"); lineno++;

  /* Output the default action table */
  fprintf(out, "   yy_default : constant array (Natural range <>) of YYACTIONTYPE := (\n"); lineno++;
  n = lemp->nxstate;
  lemp->tablesize += n*szActionType;
  for(i=j=0; i<n; i++){
    stp = lemp->sorted[i];
    if( j==0 ) fprintf(out,"      --  %5d\n      ", i);
    if( stp->iDfltReduce<0 ){
      fprintf(out, " %4d,", lemp->errAction);
    }else{
      fprintf(out, " %4d,", stp->iDfltReduce + lemp->minReduce);
    }
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "   );\n"); lineno++;
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the table of fallback tokens.
  */
  if( lemp->has_fallback ){
    int mx = lemp->nterminal - 1;
    /* 2019-08-28:  Generate fallback entries for every token to avoid
    ** having to do a range check on the index */
    /* while( mx>0 && lemp->symbols[mx]->fallback==0 ){ mx--; } */
    lemp->tablesize += (mx+1)*szCodeType;
    for(i=0; i<=mx; i++){
      struct symbol *p = lemp->symbols[i];
      if( p->fallback==0 ){
        fprintf(out, "    0,  -- %10s => nothing\n", p->name);
      }else{
        fprintf(out, "  %3d,  -- %10s => %s\n", p->fallback->index,
          p->name, p->fallback->name);
      }
      lineno++;
    }
  }
  tplt_xfer(lemp->name, in, out, &lineno);

  /* Generate a table containing the symbolic name of every symbol
  */
  int symbol_len_max = 0;
  for(i=0; i<lemp->nsymbol; i++){
    if(strlen(lemp->symbols[i]->name) > symbol_len_max){
      symbol_len_max = strlen(lemp->symbols[i]->name);
    }
  }
  for(i=0; i<lemp->nsymbol; i++){
    fprintf (out, "      \"%-*s\"", symbol_len_max, lemp->symbols[i]->name);
    fprintf (out, i == lemp->nsymbol - 1 ? ";" : ",");
    fprintf (out, "  --  %4d\n", i); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate a table containing a text string that describes every
  ** rule in the rule set of the grammar.  This information is used
  ** when tracing REDUCE actions.
  */
  for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
    assert( rp->iRule==i );
    fprintf (out, "      \"");
    writeRuleText(out, rp);
    fprintf (out, "\"%s  --  %3d\n", rp->next ? "," : " ", i); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes every time a symbol is popped from
  ** the stack while processing errors or while destroying the parser.
  ** (In other words, generate the %destructor actions)
  */
  if( lemp->tokendest ){
    int once = 1;
    for(i=0; i<lemp->nsymbol; i++){
      struct symbol *sp = lemp->symbols[i];
      if( sp==0 || sp->type!=TERMINAL ) continue;
      if( once ){
        fprintf(out, "      --  TERMINAL Destructor\n"); lineno++;
        once = 0;
      }
      fprintf(out,"    when %d =>  --  %s\n", sp->index, sp->name); lineno++;
    }
    for(i=0; i<lemp->nsymbol && lemp->symbols[i]->type!=TERMINAL; i++);
    if( i<lemp->nsymbol ){
      emit_destructor_code(out,lemp->symbols[i],lemp,&lineno);
      // fprintf(out,"      break;\n"); lineno++;
    }
  }
  if( lemp->vardest ){
    struct symbol *dflt_sp = 0;
    int once = 1;
    for(i=0; i<lemp->nsymbol; i++){
      struct symbol *sp = lemp->symbols[i];
      if( sp==0 || sp->type==TERMINAL ||
          sp->index<=0 || sp->destructor!=0 ) continue;
      if( once ){
        fprintf(out, "      --  Default NON-TERMINAL Destructor\n");lineno++;
        once = 0;
      }
      fprintf(out,"    when %d =>  --  %s\n", sp->index, sp->name); lineno++;
      dflt_sp = sp;
    }
    if( dflt_sp!=0 ){
      emit_destructor_code(out,dflt_sp,lemp,&lineno);
    }
    // fprintf(out,"      break;\n"); lineno++;
  }
  for(i=0; i<lemp->nsymbol; i++){
    struct symbol *sp = lemp->symbols[i];
    if( sp==0 || sp->type==TERMINAL || sp->destructor==0 ) continue;
    if( sp->destLineno<0 ) continue;  /* Already emitted */
    fprintf(out,"    when %d =>  --  %s\n", sp->index, sp->name); lineno++;

    /* Combine duplicate destructors into a single case */
    for(j=i+1; j<lemp->nsymbol; j++){
      struct symbol *sp2 = lemp->symbols[j];
      if( sp2 && sp2->type!=TERMINAL && sp2->destructor
          && sp2->dtnum==sp->dtnum
          && strcmp(sp->destructor,sp2->destructor)==0 ){
         fprintf(out,"    when %d =>  --  %s\n",
                 sp2->index, sp2->name); lineno++;
         sp2->destLineno = -1;  /* Avoid emitting this destructor again */
      }
    }

    emit_destructor_code(out,lemp->symbols[i],lemp,&lineno);
    // fprintf(out,"      break;\n"); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes whenever the parser stack overflows */
  tplt_print(out,lemp,lemp->overflow,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the tables of rule information.  yyRuleInfoLhs[] and
  ** yyRuleInfoNRhs[].
  **
  ** Note: This code depends on the fact that rules are number
  ** sequentually beginning with 0.
  */
  for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
    fprintf(out,"  %4d,  --  (%d) ", rp->lhs->index, i);
     rule_print(out, rp);
     fprintf(out,"\n"); lineno++; /* End comment */
  }
  tplt_xfer(lemp->name,in,out,&lineno);
  for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
    fprintf(out,"  %3d,  --  (%d) ", -rp->nrhs, i);
    rule_print(out, rp);
    fprintf(out,"\n"); lineno++; /* End comment */
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which execution during each REDUCE action */
  i = 0;
  for(rp=lemp->rule; rp; rp=rp->next){
    i += translate_code_Ada(lemp, rp);
  }
  if( i ){
    fprintf(out,"        YYMINORTYPE yylhsminor;\n"); lineno++;
  }
  /* First output rules other than the default: rule */
  for(rp=lemp->rule; rp; rp=rp->next){
    struct rule *rp2;               /* Other rules with the same action */
    if( rp->codeEmitted ) continue;
    if( rp->noCode ){
      /* No C code actions, so this will be part of the "default:" rule */
      continue;
    }
    fprintf(out,"      when %d =>  --  ", rp->iRule);
    writeRuleText(out, rp);
    fprintf(out, "\n"); lineno++;  /* End comment */
    for(rp2=rp->next; rp2; rp2=rp2->next){
      if( rp2->code==rp->code && rp2->codePrefix==rp->codePrefix
             && rp2->codeSuffix==rp->codeSuffix ){
        fprintf(out,"      when %d =>  -- ", rp2->iRule);
        writeRuleText(out, rp2);
        fprintf(out,"\n"); lineno++; /* End comment */
        fprintf(out,"   yytestcase(yyruleno==%d);\n", rp2->iRule); lineno++;
        rp2->codeEmitted = 1;
      }
    }
    emit_code(out,rp,lemp,&lineno);
    fprintf(out,"\n"); lineno++;  /* Blank line before next 'when' */
    rp->codeEmitted = 1;
  }
  /* Finally, output the default: rule.  We choose as the default: all
  ** empty actions. */
  fprintf(out,"      when others =>\n"); lineno++;
  for(rp=lemp->rule; rp; rp=rp->next){
    if( rp->codeEmitted ) continue;
    assert( rp->noCode );
    fprintf(out,"      /* (%d) ", rp->iRule);
    writeRuleText(out, rp);
    if( rp->neverReduce ){
      fprintf(out, " (NEVER REDUCES) */ assert(yyruleno!=%d);\n",
              rp->iRule); lineno++;
    }else if( rp->doesReduce ){
      fprintf(out, " */ yytestcase(yyruleno==%d);\n", rp->iRule); lineno++;
    }else{
      fprintf(out, " (OPTIMIZED OUT) */ assert(yyruleno!=%d);\n",
              rp->iRule); lineno++;
    }
  }
  // fprintf(out,"        break;\n"); lineno++;
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes if a parse fails */
  tplt_print(out,lemp,lemp->failure,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes when a syntax error occurs */
  tplt_print(out,lemp,lemp->error,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes when the parser accepts its input */
  tplt_print(out,lemp,lemp->accept,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Append any addition code the user desires */
  tplt_print(out,lemp,lemp->extracode,&lineno);

  fprintf(out,"end Calc_Ada;\n"); lineno++;

  acttab_free(pActtab);
  fclose(in);
  fclose(out);
  if( sql ) fclose(sql);
  return;
}

/* Generate parser specification */
void ReportHeader_Ada(struct lemon *lemp)
{
  FILE *out;
  const char *package_name = "Calc";
  const char *prefix;
  int i;

  if( lemp->tokenprefix ) prefix = lemp->tokenprefix;
  else                    prefix = "";

  out = file_open(lemp,".ads","wb");
  if( !out ) return;

  fprintf(out,"--  This file is generated by lemon.\n");
  fprintf(out,"--  DO NOT EDIT.\n\n");
  fprintf(out,"package %s is\n\n", package_name);
  for(i=1; i<lemp->nterminal; i++){
    fprintf(out,"   %s%-30s : constant := %3d;\n",prefix,lemp->symbols[i]->name,i);
  }
  fprintf(out,"\nend %s;\n", package_name);
  fclose(out);
}


/* Print a LINE comment to the output file. */
void tplt_linedir_Ada(FILE *out, int lineno, char *filename)
{
  fprintf(out,"--  LINE ");
  while( *filename ){
    if( *filename == '\\' ) putc('\\',out);
    putc(*filename,out);
    filename++;
  }
  fprintf(out,":%d\n",lineno);
}

/*
** Generate code which executes when the rule "rp" is reduced.  Write
** the code to "out".  Make sure lineno stays up-to-date.
*/
PRIVATE void emit_code_Ada(
  FILE *out,
  struct rule *rp,
  struct lemon *lemp,
  int *lineno
){
 const char *cp;

 /* Setup code prior to the #line directive */
 if( rp->codePrefix && rp->codePrefix[0] ){
   fprintf(out, "begin%s", rp->codePrefix);
   for(cp=rp->codePrefix; *cp; cp++){ if( *cp=='\n' ) (*lineno)++; }
 }

 /* Generate code to do the reduce action */
 if( rp->code ){
   if( !lemp->nolinenosflag ){
     (*lineno)++;
     tplt_linedir(out,rp->line,lemp->filename);
   }
   fprintf(out,"begin%s",rp->code);
   for(cp=rp->code; *cp; cp++){ if( *cp=='\n' ) (*lineno)++; }
   fprintf(out,"end;\n"); (*lineno)++;
   if( !lemp->nolinenosflag ){
     (*lineno)++;
     tplt_linedir(out,*lineno,lemp->outname);
   }
 }

 /* Generate breakdown code that occurs after the #line directive */
 if( rp->codeSuffix && rp->codeSuffix[0] ){
   fprintf(out, "%s", rp->codeSuffix);
   for(cp=rp->codeSuffix; *cp; cp++){ if( *cp=='\n' ) (*lineno)++; }
 }

 if( rp->codePrefix ){
   fprintf(out, "end;\n"); (*lineno)++;
 }

 return;
}

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
){
  int lineno = *plineno;    /* The line number of the output */
  char **types;             /* A hash table of datatypes */
  int arraysize;            /* Size of the "types" array */
  int maxdtlength;          /* Maximum length of any ".datatype" field. */
  char *stddt;              /* Standardized name for a datatype */
  int i,j;                  /* Loop counters */
  unsigned hash;            /* For hashing the name of a type */
  const char *name;         /* Name of the parser */

  /* Allocate and initialize types[] and allocate stddt[] */
  arraysize = lemp->nsymbol * 2;
  types = (char**)calloc( arraysize, sizeof(char*) );
  if( types==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }
  for(i=0; i<arraysize; i++) types[i] = 0;
  maxdtlength = 0;
  if( lemp->vartype ){
    maxdtlength = lemonStrlen(lemp->vartype);
  }
  for(i=0; i<lemp->nsymbol; i++){
    int len;
    struct symbol *sp = lemp->symbols[i];
    if( sp->datatype==0 ) continue;
    len = lemonStrlen(sp->datatype);
    if( len>maxdtlength ) maxdtlength = len;
  }
  stddt = (char*)malloc( maxdtlength*2 + 1 );
  if( stddt==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }

  /* Build a hash table of datatypes. The ".dtnum" field of each symbol
  ** is filled in with the hash index plus 1.  A ".dtnum" value of 0 is
  ** used for terminal symbols.  If there is no %default_type defined then
  ** 0 is also used as the .dtnum value for nonterminals which do not specify
  ** a datatype using the %type directive.
  */
  for(i=0; i<lemp->nsymbol; i++){
    struct symbol *sp = lemp->symbols[i];
    char *cp;
    if( sp==lemp->errsym ){
      sp->dtnum = arraysize+1;
      continue;
    }
    if( sp->type!=NONTERMINAL || (sp->datatype==0 && lemp->vartype==0) ){
      sp->dtnum = 0;
      continue;
    }
    cp = sp->datatype;
    if( cp==0 ) cp = lemp->vartype;
    j = 0;
    while( ISSPACE(*cp) ) cp++;
    while( *cp ) stddt[j++] = *cp++;
    while( j>0 && ISSPACE(stddt[j-1]) ) j--;
    stddt[j] = 0;
    if( lemp->tokentype && strcmp(stddt, lemp->tokentype)==0 ){
      sp->dtnum = 0;
      continue;
    }
    hash = 0;
    for(j=0; stddt[j]; j++){
      hash = hash*53 + stddt[j];
    }
    hash = (hash & 0x7fffffff)%arraysize;
    while( types[hash] ){
      if( strcmp(types[hash],stddt)==0 ){
        sp->dtnum = hash + 1;
        break;
      }
      hash++;
      if( hash>=(unsigned)arraysize ) hash = 0;
    }
    if( types[hash]==0 ){
      sp->dtnum = hash + 1;
      types[hash] = (char*)malloc( lemonStrlen(stddt)+1 );
      if( types[hash]==0 ){
        fprintf(stderr,"Out of memory.\n");
        exit(1);
      }
      lemon_strcpy(types[hash],stddt);
    }
  }

  /* Print out the definition of YYTOKENTYPE and YYMINORTYPE */
  name = lemp->name ? lemp->name : "Parse";
  lineno = *plineno;
  if( mhflag ){ fprintf(out,"#if INTERFACE\n"); lineno++; }
  fprintf(out,"subtype %sTOKENTYPE is %s;\n",name,
    lemp->tokentype?lemp->tokentype:"void*");  lineno++;
  if( mhflag ){ fprintf(out,"#endif\n"); lineno++; }
  fprintf(out,"type YYMINORTYPE is record\n"); lineno++;
  fprintf(out,"   yyinit : Integer;\n"); lineno++;
  fprintf(out,"   yy0    : %sTOKENTYPE;\n",name); lineno++;
  for(i=0; i<arraysize; i++){
    if( types[i]==0 ) continue;
    fprintf(out,"   yy%d : %s;\n",i+1,types[i]); lineno++;
    free(types[i]);
  }
  if( lemp->errsym && lemp->errsym->useCnt ){
    fprintf(out,"   yy%d : Integer;\n",lemp->errsym->dtnum); lineno++;
  }
  free(stddt);
  free(types);
  fprintf(out,"end record;\n"); lineno++;
  fprintf(out,"pragma Unchecked_Union (YYMINORTYPE);\n"); lineno++;
  *plineno = lineno;
}

/*
** Write and transform the rp->code string so that symbols are expanded.
** Populate the rp->codePrefix and rp->codeSuffix strings, as appropriate.
**
** Return 1 if the expanded code requires that "yylhsminor" local variable
** to be defined.
*/
PRIVATE int translate_code_Ada(struct lemon *lemp, struct rule *rp){
  char *cp, *xp;
  int i;
  int rc = 0;            /* True if yylhsminor is used */
  int dontUseRhs0 = 0;   /* If true, use of left-most RHS label is illegal */
  const char *zSkip = 0; /* The zOvwrt comment within rp->code, or NULL */
  char lhsused = 0;      /* True if the LHS element has been used */
  char lhsdirect;        /* True if LHS writes directly into stack */
  char used[MAXRHS];     /* True for each RHS element which is used */
  char zLhs[50];         /* Convert the LHS symbol into this string */
  char zOvwrt[900];      /* Comment that to allow LHS to overwrite RHS */

  for(i=0; i<rp->nrhs; i++) used[i] = 0;
  lhsused = 0;

  if( rp->code==0 ){
    static char newlinestr[2] = { '\n', '\0' };
    rp->code = newlinestr;
    rp->line = rp->ruleline;
    rp->noCode = 1;
  }else{
    rp->noCode = 0;
  }


  if( rp->nrhs==0 ){
    /* If there are no RHS symbols, then writing directly to the LHS is ok */
    lhsdirect = 1;
  }else if( rp->rhsalias[0]==0 ){
    /* The left-most RHS symbol has no value.  LHS direct is ok.  But
    ** we have to call the distructor on the RHS symbol first. */
    lhsdirect = 1;
    if( has_destructor(rp->rhs[0],lemp) ){
      append_str(0,0,0,0);
      append_str("  YY_Destructor (YYpParser, %d, YYmsp (%d).Minor'Access);\n", 0,
                 rp->rhs[0]->index,1-rp->nrhs);
      rp->codePrefix = Strsafe(append_str(0,0,0,0));
      rp->noCode = 0;
    }
  }else if( rp->lhsalias==0 ){
    /* There is no LHS value symbol. */
    lhsdirect = 1;
  }else if( strcmp(rp->lhsalias,rp->rhsalias[0])==0 ){
    /* The LHS symbol and the left-most RHS symbol are the same, so
    ** direct writing is allowed */
    lhsdirect = 1;
    lhsused = 1;
    used[0] = 1;
    if( rp->lhs->dtnum!=rp->rhs[0]->dtnum ){
      ErrorMsg(lemp->filename,rp->ruleline,
        "%s(%s) and %s(%s) share the same label but have "
        "different datatypes.",
        rp->lhs->name, rp->lhsalias, rp->rhs[0]->name, rp->rhsalias[0]);
      lemp->errorcnt++;
    }
  }else{
    lemon_sprintf(zOvwrt, "/*%s-overwrites-%s*/",
                  rp->lhsalias, rp->rhsalias[0]);
    zSkip = strstr(rp->code, zOvwrt);
    if( zSkip!=0 ){
      /* The code contains a special comment that indicates that it is safe
      ** for the LHS label to overwrite left-most RHS label. */
      lhsdirect = 1;
    }else{
      lhsdirect = 0;
    }
  }
  if( lhsdirect ){
    sprintf(zLhs, "YYmsp (%d).Minor.YY%d",1-rp->nrhs,rp->lhs->dtnum);
  }else{
    rc = 1;
    sprintf(zLhs, "YYlhsminor.YY%d",rp->lhs->dtnum);
  }

  append_str(0,0,0,0);

  /* This const cast is wrong but harmless, if we're careful. */
  for(cp=(char *)rp->code; *cp; cp++){
    if( cp==zSkip ){
      append_str(zOvwrt,0,0,0);
      cp += lemonStrlen(zOvwrt)-1;
      dontUseRhs0 = 1;
      continue;
    }
    if( ISALPHA(*cp) && (cp==rp->code || (!ISALNUM(cp[-1]) && cp[-1]!='_')) ){
      char saved;
      for(xp= &cp[1]; ISALNUM(*xp) || *xp=='_'; xp++);
      saved = *xp;
      *xp = 0;
      if( rp->lhsalias && strcmp(cp,rp->lhsalias)==0 ){
        append_str(zLhs,0,0,0);
        cp = xp;
        lhsused = 1;
      }else{
        for(i=0; i<rp->nrhs; i++){
          if( rp->rhsalias[i] && strcmp(cp,rp->rhsalias[i])==0 ){
            if( i==0 && dontUseRhs0 ){
              ErrorMsg(lemp->filename,rp->ruleline,
                 "Label %s used after '%s'.",
                 rp->rhsalias[0], zOvwrt);
              lemp->errorcnt++;
            }else if( cp!=rp->code && cp[-1]=='@' ){
              /* If the argument is of the form @X then substituted
              ** the token number of X, not the value of X */
              append_str("YYmsp (%d).Major",-1,i-rp->nrhs+1,0);
            }else{
              struct symbol *sp = rp->rhs[i];
              int dtnum;
              if( sp->type==MULTITERMINAL ){
                dtnum = sp->subsym[0]->dtnum;
              }else{
                dtnum = sp->dtnum;
              }
              append_str("YYmsp (%d).Minor.YY%d",0,i-rp->nrhs+1, dtnum);
            }
            cp = xp;
            used[i] = 1;
            break;
          }
        }
      }
      *xp = saved;
    }
    append_str(cp, 1, 0, 0);
  } /* End loop */

  /* Main code generation completed */
  cp = append_str(0,0,0,0);
  if( cp && cp[0] ) rp->code = Strsafe(cp);
  append_str(0,0,0,0);

  /* Check to make sure the LHS has been used */
  if( rp->lhsalias && !lhsused ){
    ErrorMsg(lemp->filename,rp->ruleline,
      "Label \"%s\" for \"%s(%s)\" is never used.",
        rp->lhsalias,rp->lhs->name,rp->lhsalias);
    lemp->errorcnt++;
  }

  /* Generate destructor code for RHS minor values which are not referenced.
  ** Generate error messages for unused labels and duplicate labels.
  */
  for(i=0; i<rp->nrhs; i++){
    if( rp->rhsalias[i] ){
      if( i>0 ){
        int j;
        if( rp->lhsalias && strcmp(rp->lhsalias,rp->rhsalias[i])==0 ){
          ErrorMsg(lemp->filename,rp->ruleline,
            "%s(%s) has the same label as the LHS but is not the left-most "
            "symbol on the RHS.",
            rp->rhs[i]->name, rp->rhsalias[i]);
          lemp->errorcnt++;
        }
        for(j=0; j<i; j++){
          if( rp->rhsalias[j] && strcmp(rp->rhsalias[j],rp->rhsalias[i])==0 ){
            ErrorMsg(lemp->filename,rp->ruleline,
              "Label %s used for multiple symbols on the RHS of a rule.",
              rp->rhsalias[i]);
            lemp->errorcnt++;
            break;
          }
        }
      }
      if( !used[i] ){
        ErrorMsg(lemp->filename,rp->ruleline,
          "Label %s for \"%s(%s)\" is never used.",
          rp->rhsalias[i],rp->rhs[i]->name,rp->rhsalias[i]);
        lemp->errorcnt++;
      }
    }else if( i>0 && has_destructor(rp->rhs[i],lemp) ){
      append_str("  YY_Destructor (YYpParser, %d, YYmsp (%d).Minor'Access);\n", 0,
         rp->rhs[i]->index,i-rp->nrhs+1);
    }
  }

  /* If unable to write LHS values directly into the stack, write the
  ** saved LHS value now. */
  if( lhsdirect==0 ){
    append_str("  YYmsp (%d).Minor.YY%d = ", 0, 1-rp->nrhs, rp->lhs->dtnum);
    append_str(zLhs, 0, 0, 0);
    append_str(";\n", 0, 0, 0);
  }

  /* Suffix code generation complete */
  cp = append_str(0,0,0,0);
  if( cp && cp[0] ){
    rp->codeSuffix = Strsafe(cp);
    rp->noCode = 0;
  }

  return rc;
}
