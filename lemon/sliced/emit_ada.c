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
  fprintf(out,"   YYCODETYPE   : constant := %s;\n",
    minimum_size_type(0, lemp->nsymbol, &szCodeType)); lineno++;
  fprintf(out,"   YYNOCODE     : constant := %d;\n",lemp->nsymbol);  lineno++;
  fprintf(out,"   YYACTIONTYPE : constant := %s;\n",
    minimum_size_type(0,lemp->maxAction,&szActionType)); lineno++;
  if( lemp->wildcard ){
    fprintf(out,"   YYWILDCARD : constant := %d;\n",
       lemp->wildcard->index); lineno++;
  }
  print_stack_union(out,lemp,&lineno,mhflag);
  fprintf(out, "#ifndef YYSTACKDEPTH\n"); lineno++;
  if( lemp->stacksize ){
    fprintf(out,"   YYSTACKDEPTH : constant := %s;\n",lemp->stacksize);  lineno++;
  }else{
    fprintf(out,"   YYSTACKDEPTH : constant := 100;\n");  lineno++;
  }
  fprintf(out, "#endif\n"); lineno++;
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
    fprintf(out,"   procedure %sARG_SDECL is null;\n",name); lineno++;
    fprintf(out,"   procedure %sARG_PDECL is null;\n",name); lineno++;
    fprintf(out,"   procedure %sARG_PARAM is null;\n",name); lineno++;
    fprintf(out,"   procedure %sARG_FETCH is null;\n",name); lineno++;
    fprintf(out,"   procedure %sARG_STORE is null;\n",name); lineno++;
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
    fprintf(out,"   procedure %sCTX_SDECL is null;\n",name); lineno++;
    fprintf(out,"   procedure %sCTX_PDECL is null;\n",name); lineno++;
    fprintf(out,"   procedure %sCTX_PARAM is null;\n",name); lineno++;
    fprintf(out,"   procedure %sCTX_FETCH is null;\n",name); lineno++;
    fprintf(out,"   procedure %sCTX_STORE is null;\n",name); lineno++;
  }
  if( mhflag ){
    fprintf(out,"#endif\n"); lineno++;
  }
  if( lemp->errsym && lemp->errsym->useCnt ){
    fprintf(out,"   YYERRORSYMBOL : constant := %d;\n",lemp->errsym->index); lineno++;
    fprintf(out,"   YYERRSYMDT    : constant := yy%d;\n",lemp->errsym->dtnum); lineno++;
  }
  if( lemp->has_fallback ){
    fprintf(out,"   YYFALLBACK : constant := 1;\n");  lineno++;
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
  fprintf(out,"   YYNSTATE             : constant := %d;\n",lemp->nxstate);  lineno++;
  fprintf(out,"   YYNRULE              : constant := %d;\n",lemp->nrule);  lineno++;
  fprintf(out,"   YYNRULE_WITH_ACTION  : constant := %d;\n",lemp->nruleWithAction);
         lineno++;
  fprintf(out,"   YYNTOKEN             : constant := %d;\n",lemp->nterminal); lineno++;
  fprintf(out,"   YY_MAX_SHIFT         : constant := %d;\n",lemp->nxstate-1); lineno++;
  i = lemp->minShiftReduce;
  fprintf(out,"   YY_MIN_SHIFTREDUCE   : constant := %d;\n",i); lineno++;
  i += lemp->nrule;
  fprintf(out,"   YY_MAX_SHIFTREDUCE   : constant := %d;\n", i-1); lineno++;
  fprintf(out,"   YY_ERROR_ACTION      : constant := %d;\n", lemp->errAction); lineno++;
  fprintf(out,"   YY_ACCEPT_ACTION     : constant := %d;\n", lemp->accAction); lineno++;
  fprintf(out,"   YY_NO_ACTION         : constant := %d;\n", lemp->noAction); lineno++;
  fprintf(out,"   YY_MIN_REDUCE        : constant := %d;\n", lemp->minReduce); lineno++;
  i = lemp->minReduce + lemp->nrule;
  fprintf(out,"   YY_MAX_REDUCE        : constant := %d;\n", i-1); lineno++;
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
  fprintf(out,"   YY_ACTTAB_COUNT : constant := (%d);\n", n); lineno++;
  fprintf(out,"   yy_action : constant array (Natural range <>) of YYACTIONTYPE := (\n"); lineno++;
  for(i=j=0; i<n; i++){
    int action = acttab_yyaction(pActtab, i);
    if( action<0 ) action = lemp->noAction;
    if( j==0 ) fprintf(out,"      --  %5d\n      ", i);
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
  fprintf(out,"   yy_lookahead : constant array (Natural range <>) of YYCODETYPE := (\n"); lineno++;
  for(i=j=0; i<n; i++){
    int la = acttab_yylookahead(pActtab, i);
    if( la<0 ) la = lemp->nsymbol;
    if( j==0 ) fprintf(out,"      --  %5d\n      ", i);
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
    if( j==0 ) fprintf(out,"      --  %5d\n      ", i);
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
  fprintf(out, "   );\n"); lineno++;

  /* Output the yy_shift_ofst[] table */
  n = lemp->nxstate;
  while( n>0 && lemp->sorted[n-1]->iTknOfst==NO_OFFSET ) n--;
  fprintf(out, "   YY_SHIFT_COUNT : constant := (%d);\n", n-1); lineno++;
  fprintf(out, "   YY_SHIFT_MIN   : constant := (%d);\n", mnTknOfst); lineno++;
  fprintf(out, "   YY_SHIFT_MAX   : constant := (%d);\n", mxTknOfst); lineno++;
  fprintf(out, "   yy_shift_ofst : constant array (Natrual range <>) of %s := (\n",
       minimum_size_type(mnTknOfst, lemp->nterminal+lemp->nactiontab, &sz));
       lineno++;
  lemp->tablesize += n*sz;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iTknOfst;
    if( ofst==NO_OFFSET ) ofst = lemp->nactiontab;
    if( j==0 ) fprintf(out,"      --  %5d\n      ", i);
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
  fprintf(out, "   YY_REDUCE_COUNT : constant := (%d);\n", n-1); lineno++;
  fprintf(out, "   YY_REDUCE_MIN   : constant := (%d);\n", mnNtOfst); lineno++;
  fprintf(out, "   YY_REDUCE_MAX   : constant := (%d);\n", mxNtOfst); lineno++;
  fprintf(out, "   yy_reduce_ofst : constant array (Natural range <>) of %s := (\n",
          minimum_size_type(mnNtOfst-1, mxNtOfst, &sz)); lineno++;
  lemp->tablesize += n*sz;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iNtOfst;
    if( ofst==NO_OFFSET ) ofst = mnNtOfst - 1;
    if( j==0 ) fprintf(out,"      --  %5d\n      ", i);
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
    fprintf(out," /* %3d */ \"", i);
    writeRuleText(out, rp);
    fprintf(out,"\",\n"); lineno++;
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
    i += translate_code(lemp, rp);
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
    // fprintf(out,"        break;\n"); lineno++;
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
