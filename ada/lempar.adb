--
--  2000-05-29
--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, never taking more than you give.
--
-------------------------------------------------------------------------
--  Driver template for the LEMON parser generator.
--
--  The "lemon" program processes an LALR(1) input grammar file, then uses
--  this template to construct a parser.  The "lemon" program inserts text
--  at each "%%" line.  Also, any "P-a-r-s-e" identifer prefix (without the
--  interstitial "-" characters) contained in this template is changed into
--  the value of the %name directive from the grammar.  Otherwise, the content
--  of this template is copied straight through into the generate parser
--  source file.
--
--  The following is the concatenation of all %include directives from the
--  input grammar file:
--
with Ada.Text_IO;
--#include <stdio.h>
--#include <assert.h>
------------- Begin %include sections from the grammar -------------------------
%%
----------------- End of %include directives -----------------------------------
--  These constants specify the various numeric values for terminal symbols
--  in a format understandable to "makeheaders".  This section is blank unless
--  "lemon" is run with the "-m" command-line option.
----------------- Begin makeheaders token definitions --------------------------
%%
----------------- End makeheaders token definitions ----------------------------

--  The next sections is a series of control #defines.
--  various aspects of the generated parser.
--    YYCODETYPE         is the data type used to store the integer codes
--                       that represent terminal and non-terminal symbols.
--                       "unsigned char" is used if there are fewer than
--                       256 symbols.  Larger types otherwise.
--    YYNOCODE           is a number of type YYCODETYPE that is not used for
--                       any terminal or nonterminal symbol.
--    YYFALLBACK         If defined, this indicates that one or more tokens
--                       (also known as: "terminal symbols") have fall-back
--                       values which should be used if the original symbol
--                       would not parse.  This permits keywords to sometimes
--                       be used as identifiers, for example.
--    YYACTIONTYPE       is the data type used for "action codes" - numbers
--                       that indicate what to do in response to the next
--                       token.
--    ParseTOKENTYPE     is the data type used for minor type for terminal
--                       symbols.  Background: A "minor type" is a semantic
--                       value associated with a terminal or non-terminal
--                       symbols.  For example, for an "ID" terminal symbol,
--                       the minor type might be the name of the identifier.
--                       Each non-terminal can have a different minor type.
--                       Terminal symbols all have the same minor type, though.
--                       This macros defines the minor type for terminal
--                       symbols.
--    YYMINORTYPE        is the data type used for all minor types.
--                       This is typically a union of many types, one of
--                       which is ParseTOKENTYPE.  The entry in the union
--                       for terminal symbols is called "yy0".
--    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
--                       zero the stack is dynamically sized using realloc()
--    ParseARG_SDECL     A static variable declaration for the %extra_argument
--    ParseARG_PDECL     A parameter declaration for the %extra_argument
--    ParseARG_PARAM     Code to pass %extra_argument as a subroutine parameter
--    ParseARG_STORE     Code to store %extra_argument into yypParser
--    ParseARG_FETCH     Code to extract %extra_argument from yypParser
--    ParseCTX_*         As ParseARG_ except for %extra_context
--    YYERRORSYMBOL      is the code number of the error symbol.  If not
--                       defined, then do no error processing.
--    YYNSTATE           the combined number of states.
--    YYNRULE            the number of rules in the grammar
--    YYNTOKEN           Number of terminal symbols
--    YY_MAX_SHIFT       Maximum value for shift actions
--    YY_MIN_SHIFTREDUCE Minimum value for shift-reduce actions
--    YY_MAX_SHIFTREDUCE Maximum value for shift-reduce actions
--    YY_ERROR_ACTION    The yy_action[] code for syntax error
--    YY_ACCEPT_ACTION   The yy_action[] code for accept
--    YY_NO_ACTION       The yy_action[] code for no-op
--    YY_MIN_REDUCE      Minimum value for reduce actions
--    YY_MAX_REDUCE      Maximum value for reduce actions
--
--#ifndef INTERFACE
--# define INTERFACE 1
--#endif
INTERFACE_C : constant Boolean := True;
-------------- Begin control #defines ------------------------------------------
%%
-------------- End control #defines --------------------------------------------
--#define YY_NLOOKAHEAD ((int)(sizeof(yy_lookahead)/sizeof(yy_lookahead[0])))
YY_NLOOKAHEAD : constant := yy_Lookahead'Length;

--  Define the yytestcase() macro to be a no-op if is not already defined
--  otherwise.
--
--  Applications can choose to define yytestcase() in the %include section
--  to a macro that can assist in verifying code coverage.  For production
--  code the yytestcase() macro should be turned off.  But it is useful
--  for testing.
--
--#ifndef yytestcase
--# define yytestcase(X)
--#endif


-- Next are the tables used to determine what action to take based on the
-- current state and lookahead token.  These tables are used to implement
-- functions that take a state number and lookahead value and return an
-- action integer.
--
-- Suppose the action integer is N.  Then the action is determined as
-- follows
--
--   0 <= N <= YY_MAX_SHIFT             Shift N.  That is, push the lookahead
--                                      token onto the stack and goto state N.
--
--   N between YY_MIN_SHIFTREDUCE       Shift to an arbitrary state then
--     and YY_MAX_SHIFTREDUCE           reduce by rule N-YY_MIN_SHIFTREDUCE.
--
--   N == YY_ERROR_ACTION               A syntax error has occurred.
--
--   N == YY_ACCEPT_ACTION              The parser accepts its input.
--
--   N == YY_NO_ACTION                  No such action.  Denotes unused
--                                      slots in the yy_action[] table.
--
--   N between YY_MIN_REDUCE            Reduce by rule N-YY_MIN_REDUCE
--     and YY_MAX_REDUCE
--
--  The action table is constructed as a single large table named yy_action[].
--  Given state S and lookahead X, the action is computed as either:
--
--    (A)   N = yy_action[ yy_shift_ofst[S] + X ]
--    (B)   N = yy_default[S]
--
--  The (A) formula is preferred.  The B formula is used instead if
--  yy_lookahead[yy_shift_ofst[S]+X] is not equal to X.
--
--  The formulas above are for computing the action when the lookahead is
--  a terminal symbol.  If the lookahead is a non-terminal (as occurs after
--  a reduce action) then the yy_reduce_ofst[] array is used in place of
--  the yy_shift_ofst[] array.
--
--  The following are the tables generated in this section:
--
--  yy_action[]        A single table containing all actions.
--  yy_lookahead[]     A table containing the lookahead for each entry in
--                     yy_action.  Used to detect hash collisions.
--  yy_shift_ofst[]    For each state, the offset into yy_action for
--                     shifting terminals.
--  yy_reduce_ofst[]   For each state, the offset into yy_action for
--                     shifting non-terminals after a reduce.
--  yy_default[]       Default action for each state.
--
----------- Begin parsing tables -----------------------------------------------
%%
----------- End of lemon-generated parsing tables ------------------------------

--  The next table maps tokens (terminal symbols) into fallback tokens.
--  If a construct like the following:
--
--      %fallback ID X Y Z.
--
--  appears in the grammar, then ID becomes a fallback token for X, Y,
--  and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
--  but it does not parse, the type of the token is changed to ID and
--  the parse is retried before an error is thrown.
--
--  This feature can be used, for example, to cause some keywords in a language
--  to revert to identifiers if they keyword does not apply in the context where
--  it appears.
--
--#ifdef YYFALLBACK
yyFallback : constant array (Natural range <>) of YYCODETYPE := (
%%
);
--#endif /* YYFALLBACK */

--  The following structure represents a single element of the
--  parser's stack.  Information stored includes:
--
--   +  The state number for the parser at this level of the stack.
--
--   +  The value of the token stored at this level of the stack.
--      (In other words, the "major" token.)
--
--   +  The semantic value stored at this level of the stack.  This is
--      the information used by the action routines in the grammar.
--      It is sometimes called the "minor" token.
--
--  After the "shift" half of a SHIFTREDUCE action, the stateno field
--  actually contains the reduce action for the second half of the
--  SHIFTREDUCE.
--
type yyStackEntry is record
  Stateno : YYACTIONTYPE;  -- The state-number, or reduce action in SHIFTREDUCE
  Major   : YYCODETYPE;    -- The major token value.  This is the code
                           -- number for the token at this stack level
  Minor   : YYMINORTYPE;   -- The user-supplied minor token value.  This
                           -- is the value of the token
end record;
--typedef struct yyStackEntry yyStackEntry;

--  The state of the parser is completely contained in an instance of
--  the following structure
type Stack_Array      is array (Natural range 0 .. YYSTACKDEPTH-1) of aliased YyStackEntry;
type YyStackEntry_Ptr is access YyStackEntry;
type yyParser is record
  Yytos    : YyStackEntry_Ptr;    --  Pointer to top element of the stack
--#ifdef YYTRACKMAXSTACKDEPTH
  Yyhwm    : Integer;             --  High-water mark of the stack
--#endif
--#ifndef YYNOERRORRECOVERY
  Yyerrcnt : Integer;             --  Shifts left before out of the error
--#endif
  ARG : ParseARG_SDECL;           --  A place to hold %extra_argument
  CTX : ParseCTX_SDECL;           --  A place to hold %extra_context
--#if YYSTACKDEPTH<=0
  Yystksz    : Integer;           --  Current side of the stack
--  yyStackEntry *yystack;        --  The parser's stack
  Yystk0     : yyStackEntry ;     --  First stack entry
--#else
  Yystack    : Stack_Array;       --  The parser's stack
  YystackEnd : YyStackEntry_Ptr;  --  Last entry in the stack
--#endif
end record;
--typedef struct yyParser yyParser;

--#ifndef NDEBUG
--#include <stdio.h>
--static FILE *yyTraceFILE = 0;
--static char *yyTracePrompt = 0;
--#endif /* NDEBUG */

--#ifndef NDEBUG
--
--  Turn parser tracing on by giving a stream to which to write the trace
--  and a prompt to preface each trace message.  Tracing is turned off
--  by making either argument NULL
--
--  Inputs:
--  <ul>
--  <li> A FILE* to which trace output should be written.
--       If NULL, then tracing is turned off.
--  <li> A prefix string written at the beginning of every
--       line of trace output.  If NULL, then tracing is
--       turned off.
--  </ul>
--
--  Outputs:
--  None.
--
procedure ParseTrace
  (TraceFILE    : in out FILE_Star;
   ZTracePrompt : in     String)
is
begin
  yyTraceFILE   := TraceFILE;
  yyTracePrompt := zTracePrompt;
  if YyTraceFILE = 0 then
     yyTracePrompt := 0;
  elsif YyTracePrompt = 0 then
     yyTraceFILE := 0;
  end if;
end ParseTrace;
--#endif /* NDEBUG */

--#if defined(YYCOVERAGE) || !defined(NDEBUG)
--  For tracing shifts, the names of all terminals and nonterminals
--  are required.  The following table supplies these names
type String_Access is access all String;
yyTokenName : constant array (Natural range <>) of String_Access := (
%%
);
--#endif /* defined(YYCOVERAGE) || !defined(NDEBUG) */

--#ifndef NDEBUG
--  For tracing reduce actions, the names of all rules are required.
--
yyRuleName : constant array (Natural range <>) of String_Access := (
%%
);
--#endif /* NDEBUG */


--#if YYSTACKDEPTH<=0
--
--  Try to increase the size of the parser stack.  Return the number
--  of errors.  Return 0 on success.
--
procedure YyGrowStack
  (P           : in out yyParser;
   Error_Count :    out Natural)
is
   New_Size : Integer;
   Idx      : Integer;
   Pnew     : YyStackEntry_Ptr;
begin
   New_Size := P.Yystksz * 2 + 100;
   Idx     := (if P.yytos then P.Yytos - P.Yystack else 0);
   if P.Yystack = P.Yystk0'Access then
      pNew := new Stack_Array'(0 .. New_Size - 1 => <>);
      PNew (0) := P.yystk0;
   else
      pNew := new Stack_Array'(0 .. New_Size - 1 => <>);
      Pnew (Pnew'First .. P.Yystack'Last) := P.Yystack;
      --pNew := Realloc (P.yystack, New_Size * sizeof(pNew[0]));
   end if;
   if pNew then
      P.Yystack := pNew;
      P.Yytos   := P.Yystack (Idx)'Access;
--#ifndef NDEBUG
      if YyTraceFILE then
         Fprintf (yyTraceFILE,"%sStack grows from %d to %d entries.\n",
                  yyTracePrompt, P.Yystksz, New_Size);
      end if;
--#endif
      P.Yystksz := New_Size;
   end if;
   Error_Count := (if PNew = 0 then 1 else 0);
end YyGrowStack;
--#endif

--  Datatype of the argument to the memory allocated passed as the
--  second argument to ParseAlloc() below.  This can be changed by
--  putting an appropriate #define in the %include section of the input
--  grammar.
--
--#ifndef YYMALLOCARGTYPE
--# define YYMALLOCARGTYPE size_t
--#endif

--  Initialize a new parser that has already been allocated.
--
procedure ParseInit
  (YypRawParser : System.Address;
   CTX          : ParseCTX_PDECL)
is
   YypParser : YyParser;
   for Yypparser'Address use YypRawParser'Address;
begin
  ParseCTX_STORE;
--#ifdef YYTRACKMAXSTACKDEPTH
  YypParser.yyhwm := 0;
--#endif
--#if YYSTACKDEPTH<=0
  YypParser.yytos   := null;
  YypParser.yystack := null;
  YypParser.yystksz := 0;
  if YyGrowStack (YypParser) then
    YypParser.Yystack := YypParser.Yystk0'Access;
    YypParser.Yystksz := 1;
  end if;
--#endif
--#ifndef YYNOERRORRECOVERY
  YypParser.Yyerrcnt := -1;
--#endif
  YypParser.Yytos := YypParser.Yystack;
  YypParser.Yystack (0).Stateno := 0;
  YypParser.Yystack (0).Major   := 0;
--#if YYSTACKDEPTH>0
  YypParser.YystackEnd := YypParser.Yystack (YYSTACKDEPTH-1)'Access;
--#endif
end ParseInit;

--#ifndef Parse_ENGINEALWAYSONSTACK
--
--  This function allocates a new parser.
--  The only argument is a pointer to a function which works like
--  malloc.
--
--  Inputs:
--  A pointer to the function used to allocate memory.
--
--  Outputs:
--  A pointer to a parser.  This pointer is used in subsequent calls
--  to Parse and ParseFree.
--
--  void *ParseAlloc(void *(*mallocProc)(YYMALLOCARGTYPE) ParseCTX_PDECL){
--    yyParser *yypParser;
--    yypParser = (yyParser*)(*mallocProc)( (YYMALLOCARGTYPE)sizeof(yyParser) );
--    if( yypParser ){
--      ParseCTX_STORE
--      ParseInit(yypParser ParseCTX_PARAM);
--    }
--    return (void*)yypParser;
--  }
--  #endif /* Parse_ENGINEALWAYSONSTACK */


--  The following function deletes the "minor type" or semantic value
--  associated with a symbol.  The symbol can be either a terminal
--  or nonterminal. "yymajor" is the symbol code, and "yypminor" is
--  a pointer to the value to be deleted.  The code used to do the
--  deletions is derived from the %destructor and/or %token_destructor
--  directives of the input grammar.
--
procedure Yy_Destructor
  (YypParser : in out YyParser;     -- The parser
   Yymajor   : in     YYCODETYPE;   -- Type code for object to destroy
   Yypminor  : in out YYMINORTYPE)  -- The object to be destroyed
is
begin
  ParseARG_FETCH;
  ParseCTX_FETCH;
  case yymajor is
    --  Here is inserted the actions which take place when a
    --  terminal or non-terminal is destroyed.  This can happen
    --  when the symbol is popped from the stack during a
    --  reduce or during error processing or when a parser is
    --  being destroyed before it is finished parsing.
    --
    --  Note: during a reduce, the only symbols destroyed are those
    --  which appear on the RHS of the rule, but which are *not* used
    --  inside the C code.
    --
---------- Begin destructor definitions ----------------------------------------
%%
---------- End destructor definitions ------------------------------------------
    when others => null; -- If no destructor action specified: do nothing
  end case;
end Yy_Destructor;

--
--  Pop the parser's stack once.
--
--  If there is a destructor routine associated with the token which
--  is popped from the stack, then call it.
--
--  static void yy_pop_parser_stack(yyParser *pParser){
--    yyStackEntry *yytos;
--    assert( pParser->yytos!=0 );
--    assert( pParser->yytos > pParser->yystack );
--    yytos = pParser->yytos--;
--  #ifndef NDEBUG
--    if( yyTraceFILE ){
--      fprintf(yyTraceFILE,"%sPopping %s\n",
--        yyTracePrompt,
--        yyTokenName[yytos->major]);
--    }
--  #endif
--    yy_destructor(pParser, yytos->major, &yytos->minor);
--  }

--
--  Clear all secondary memory allocations from the parser
--
--  void ParseFinalize(void *p){
--    yyParser *pParser = (yyParser*)p;
--    while( pParser->yytos>pParser->yystack ) yy_pop_parser_stack(pParser);
--  #if YYSTACKDEPTH<=0
--    if( pParser->yystack!=&pParser->yystk0 ) free(pParser->yystack);
--  #endif
--  }

--  #ifndef Parse_ENGINEALWAYSONSTACK
--
--  Deallocate and destroy a parser.  Destructors are called for
--  all stack elements before shutting the parser down.
--
--  If the YYPARSEFREENEVERNULL macro exists (for example because it
--  is defined in a %include section of the input grammar) then it is
--  assumed that the input pointer is never NULL.
--
--  void ParseFree(
--    void *p,                    /* The parser to be deleted */
--    void (*freeProc)(void*)     /* Function used to reclaim memory */
--  ){
--  #ifndef YYPARSEFREENEVERNULL
--    if( p==0 ) return;
--  #endif
--    ParseFinalize(p);
--    (*freeProc)(p);
--  }
--  #endif /* Parse_ENGINEALWAYSONSTACK */

--
--  Return the peak depth of the stack for a parser.
--
--  #ifdef YYTRACKMAXSTACKDEPTH
--  int ParseStackPeak(void *p){
--    yyParser *pParser = (yyParser*)p;
--    return pParser->yyhwm;
--  }
--  #endif

--  This array of booleans keeps track of the parser statement
--  coverage.  The element yycoverage[X][Y] is set when the parser
--  is in state X and has a lookahead token Y.  In a well-tested
--  systems, every element of this matrix should end up being set.
--
--  #if defined(YYCOVERAGE)
--  static unsigned char yycoverage[YYNSTATE][YYNTOKEN];
--  #endif

--
--  Write into out a description of every state/lookahead combination that
--
--   (1)  has not been used by the parser, and
--   (2)  is not a syntax error.
--
--  Return the number of missed state/lookahead combinations.
--
--  #if defined(YYCOVERAGE)
--  int ParseCoverage(FILE *out){
--    int stateno, iLookAhead, i;
--    int nMissed = 0;
--    for(stateno=0; stateno<YYNSTATE; stateno++){
--      i = yy_shift_ofst[stateno];
--      for(iLookAhead=0; iLookAhead<YYNTOKEN; iLookAhead++){
--        if( yy_lookahead[i+iLookAhead]!=iLookAhead ) continue;
--        if( yycoverage[stateno][iLookAhead]==0 ) nMissed++;
--        if( out ){
--          fprintf(out,"State %d lookahead %s %s\n", stateno,
--                  yyTokenName[iLookAhead],
--                  yycoverage[stateno][iLookAhead] ? "ok" : "missed");
--        }
--      }
--    }
--    return nMissed;
--  }
--  #endif

--
--  Find the appropriate action for a parser given the terminal
--  look-ahead token iLookAhead.
--
--  static YYACTIONTYPE yy_find_shift_action(
--    YYCODETYPE iLookAhead,    /* The look-ahead token */
--    YYACTIONTYPE stateno      /* Current state number */
--  ){
--    int i;

--    if( stateno>YY_MAX_SHIFT ) return stateno;
--    assert( stateno <= YY_SHIFT_COUNT );
--  #if defined(YYCOVERAGE)
--    yycoverage[stateno][iLookAhead] = 1;
--  #endif
--    do{
--      i = yy_shift_ofst[stateno];
--      assert( i>=0 );
--      assert( i<=YY_ACTTAB_COUNT );
--      assert( i+YYNTOKEN<=(int)YY_NLOOKAHEAD );
--      assert( iLookAhead!=YYNOCODE );
--      assert( iLookAhead < YYNTOKEN );
--      i += iLookAhead;
--      assert( i<(int)YY_NLOOKAHEAD );
--      if( yy_lookahead[i]!=iLookAhead ){
--  #ifdef YYFALLBACK
--        YYCODETYPE iFallback;            /* Fallback token */
--        assert( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0]) );
--        iFallback = yyFallback[iLookAhead];
--        if( iFallback!=0 ){
--  #ifndef NDEBUG
--          if( yyTraceFILE ){
--            fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
--               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
--          }
--  #endif
--          assert( yyFallback[iFallback]==0 ); /* Fallback loop must terminate */
--          iLookAhead = iFallback;
--          continue;
--        }
--  #endif
--  #ifdef YYWILDCARD
--        {
--          int j = i - iLookAhead + YYWILDCARD;
--          assert( j<(int)(sizeof(yy_lookahead)/sizeof(yy_lookahead[0])) );
--          if( yy_lookahead[j]==YYWILDCARD && iLookAhead>0 ){
--  #ifndef NDEBUG
--            if( yyTraceFILE ){
--              fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
--                 yyTracePrompt, yyTokenName[iLookAhead],
--                 yyTokenName[YYWILDCARD]);
--            }
--  #endif /* NDEBUG */
--            return yy_action[j];
--          }
--        }
--  #endif /* YYWILDCARD */
--        return yy_default[stateno];
--      }else{
--        assert( i>=0 && i<sizeof(yy_action)/sizeof(yy_action[0]) );
--        return yy_action[i];
--      }
--    }while(1);
--  }

--
--  Find the appropriate action for a parser given the non-terminal
--  look-ahead token iLookAhead.
--
--  static YYACTIONTYPE yy_find_reduce_action(
--    YYACTIONTYPE stateno,     /* Current state number */
--    YYCODETYPE iLookAhead     /* The look-ahead token */
--  ){
--    int i;
--  #ifdef YYERRORSYMBOL
--    if( stateno>YY_REDUCE_COUNT ){
--      return yy_default[stateno];
--    }
--  #else
--    assert( stateno<=YY_REDUCE_COUNT );
--  #endif
--    i = yy_reduce_ofst[stateno];
--    assert( iLookAhead!=YYNOCODE );
--    i += iLookAhead;
--  #ifdef YYERRORSYMBOL
--    if( i<0 || i>=YY_ACTTAB_COUNT || yy_lookahead[i]!=iLookAhead ){
--      return yy_default[stateno];
--    }
--  #else
--    assert( i>=0 && i<YY_ACTTAB_COUNT );
--    assert( yy_lookahead[i]==iLookAhead );
--  #endif
--    return yy_action[i];
--  }

--
--  The following routine is called if the stack overflows.
--
procedure YyStackOverflow (YypParser : in out yyParser)
is
begin
   ParseARG_FETCH;
   ParseCTX_FETCH;
--#ifndef NDEBUG
   if yyTraceFILE then
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   end if;
--#endif
   while YypParser.Yytos > YypParser.yystack loop
     Yy_Pop_Parser_Stack (yypParser);
   end loop;
   --  Here code is inserted which will execute if the parser
   --  stack every overflows
--------- Begin %stack_overflow code -------------------------------------------
%%
--------- End %stack_overflow code ---------------------------------------------
   ParseARG_STORE; --  Suppress warning about unused %extra_argument var
   ParseCTX_STORE;
end YyStackOverflow;

--
--  Print tracing information for a SHIFT action
--
--  #ifndef NDEBUG
--  static void yyTraceShift(yyParser *yypParser, int yyNewState, const char *zTag){
--    if( yyTraceFILE ){
--      if( yyNewState<YYNSTATE ){
--        fprintf(yyTraceFILE,"%s%s '%s', go to state %d\n",
--           yyTracePrompt, zTag, yyTokenName[yypParser->yytos->major],
--           yyNewState);
--      }else{
--        fprintf(yyTraceFILE,"%s%s '%s', pending reduce %d\n",
--           yyTracePrompt, zTag, yyTokenName[yypParser->yytos->major],
--           yyNewState - YY_MIN_REDUCE);
--      }
--    }
--  }
--  #else
--  # define yyTraceShift(X,Y,Z)
--  #endif

--
--  Perform a shift action.
--
--  static void yy_shift(
--    yyParser *yypParser,          /* The parser to be shifted */
--    YYACTIONTYPE yyNewState,      /* The new state to shift in */
--    YYCODETYPE yyMajor,           /* The major token to shift in */
--    ParseTOKENTYPE yyMinor        /* The minor token to shift in */
--  ){
--    yyStackEntry *yytos;
--    yypParser->yytos++;
--  #ifdef YYTRACKMAXSTACKDEPTH
--    if( (int)(yypParser->yytos - yypParser->yystack)>yypParser->yyhwm ){
--      yypParser->yyhwm++;
--      assert( yypParser->yyhwm == (int)(yypParser->yytos - yypParser->yystack) );
--    }
--  #endif
--  #if YYSTACKDEPTH>0
--    if( yypParser->yytos>yypParser->yystackEnd ){
--      yypParser->yytos--;
--      yyStackOverflow(yypParser);
--      return;
--    }
--  #else
--    if( yypParser->yytos>=&yypParser->yystack[yypParser->yystksz] ){
--      if( yyGrowStack(yypParser) ){
--        yypParser->yytos--;
--        yyStackOverflow(yypParser);
--        return;
--      }
--    }
--  #endif
--    if( yyNewState > YY_MAX_SHIFT ){
--      yyNewState += YY_MIN_REDUCE - YY_MIN_SHIFTREDUCE;
--    }
--    yytos = yypParser->yytos;
--    yytos->stateno = yyNewState;
--    yytos->major = yyMajor;
--    yytos->minor.yy0 = yyMinor;
--    yyTraceShift(yypParser, yyNewState, "Shift");
--  }

--  For rule J, yyRuleInfoLhs[J] contains the symbol on the left-hand side
--  of that rule
YyRuleInfoLhs : constant array (Natural range <>) of YYCODETYPE := (
%%
);

--  For rule J, yyRuleInfoNRhs[J] contains the negative of the number
--  of symbols on the right-hand side of that rule.
YyRuleInfoNRhs : constant array (Natural range <>) of Signed_Char := (
%%
);

procedure Yy_Accept (YypParser : in out yyParser);  --  Forward Declaration

--
--  Perform a reduce action and the shift that must immediately
--  follow the reduce.
--
--  The yyLookahead and yyLookaheadToken parameters provide reduce actions
--  access to the lookahead token (if any).  The yyLookahead will be YYNOCODE
--  if the lookahead token has already been consumed.  As this procedure is
--  only called from one place, optimizing compilers will in-line it, which
--  means that the extra parameters have no performance impact.
--
procedure Yy_Reduce is
   Yylhsminor : YYMINORTYPE;
   --!!! Kopieret hertil fra emit_ada.c!
   --!!! Kan ikke deklarere i case statement.
begin
--  static YYACTIONTYPE yy_reduce(
--    yyParser *yypParser,         /* The parser */
--    unsigned int yyruleno,       /* Number of the rule by which to reduce */
--    int yyLookahead,             /* Lookahead token, or YYNOCODE if none */
--    ParseTOKENTYPE yyLookaheadToken  /* Value of the lookahead token */
--    ParseCTX_PDECL                   /* %extra_context */
--  ){
--    int yygoto;                     /* The next state */
--    YYACTIONTYPE yyact;             /* The next action */
--    yyStackEntry *yymsp;            /* The top of the parser's stack */
--    int yysize;                     /* Amount to pop the stack */
--    ParseARG_FETCH
--    (void)yyLookahead;
--    (void)yyLookaheadToken;
--    yymsp = yypParser->yytos;
--  #ifndef NDEBUG
--    if( yyTraceFILE && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
--      yysize = yyRuleInfoNRhs[yyruleno];
--      if( yysize ){
--        fprintf(yyTraceFILE, "%sReduce %d [%s]%s, pop back to state %d.\n",
--          yyTracePrompt,
--          yyruleno, yyRuleName[yyruleno],
--          yyruleno<YYNRULE_WITH_ACTION ? "" : " without external action",
--          yymsp[yysize].stateno);
--      }else{
--        fprintf(yyTraceFILE, "%sReduce %d [%s]%s.\n",
--          yyTracePrompt, yyruleno, yyRuleName[yyruleno],
--          yyruleno<YYNRULE_WITH_ACTION ? "" : " without external action");
--      }
--    }
--  #endif /* NDEBUG */

--    /* Check that the stack is large enough to grow by a single entry
--    ** if the RHS of the rule is empty.  This ensures that there is room
--    ** enough on the stack to push the LHS value */
--    if( yyRuleInfoNRhs[yyruleno]==0 ){
--  #ifdef YYTRACKMAXSTACKDEPTH
--      if( (int)(yypParser->yytos - yypParser->yystack)>yypParser->yyhwm ){
--        yypParser->yyhwm++;
--        assert( yypParser->yyhwm == (int)(yypParser->yytos - yypParser->yystack));
--      }
--  #endif
--  #if YYSTACKDEPTH>0
--      if( yypParser->yytos>=yypParser->yystackEnd ){
--        yyStackOverflow(yypParser);
--        /* The call to yyStackOverflow() above pops the stack until it is
--        ** empty, causing the main parser loop to exit.  So the return value
--        ** is never used and does not matter. */
--        return 0;
--      }
--  #else
--      if( yypParser->yytos>=&yypParser->yystack[yypParser->yystksz-1] ){
--        if( yyGrowStack(yypParser) ){
--          yyStackOverflow(yypParser);
--          /* The call to yyStackOverflow() above pops the stack until it is
--          ** empty, causing the main parser loop to exit.  So the return value
--          ** is never used and does not matter. */
--          return 0;
--        }
--        yymsp = yypParser->yytos;
--      }
--  #endif
--    }

  case Yyruleno is
  -- Beginning here are the reduction cases.  A typical example
  -- follows:
  --   when 0 =>
  --  #line <lineno> <grammarfile>
  --     { ... }           --  User supplied code
  --  #line <lineno> <thisfile>
  --
----------- Begin reduce actions -----------------------------------------------
%%
----------- End reduce actions -------------------------------------------------
  end case;
--    assert( yyruleno<sizeof(yyRuleInfoLhs)/sizeof(yyRuleInfoLhs[0]) );
--    yygoto = yyRuleInfoLhs[yyruleno];
--    yysize = yyRuleInfoNRhs[yyruleno];
--    yyact = yy_find_reduce_action(yymsp[yysize].stateno,(YYCODETYPE)yygoto);

--    /* There are no SHIFTREDUCE actions on nonterminals because the table
--    ** generator has simplified them to pure REDUCE actions. */
--    assert( !(yyact>YY_MAX_SHIFT && yyact<=YY_MAX_SHIFTREDUCE) );

--    /* It is not possible for a REDUCE to be followed by an error */
--    assert( yyact!=YY_ERROR_ACTION );

--    yymsp += yysize+1;
--    yypParser->yytos = yymsp;
--    yymsp->stateno = (YYACTIONTYPE)yyact;
--    yymsp->major = (YYCODETYPE)yygoto;
--    yyTraceShift(yypParser, yyact, "... then shift");
--    return yyact;
end Yy_Reduce;

--
--  The following code executes when the parse fails
--
--#ifndef YYNOERRORRECOVERY
procedure Yy_Parse_Failed
  (YypParser : in out YyParser)            --  The parser
is
begin
  ParseARG_FETCH;
  ParseCTX_FETCH;
--  #ifndef NDEBUG
--    if( yyTraceFILE ){
--      fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
--    }
--  #endif
--    while( yypParser->yytos>yypParser->yystack ) yy_pop_parser_stack(yypParser);
  --  Here code is inserted which will be executed whenever the
  --  parser fails
------------- Begin %parse_failure code ----------------------------------------
%%
------------- End %parse_failure code ------------------------------------------
  ParseARG_STORE; -- Suppress warning about unused %extra_argument variable
  ParseCTX_STORE;
end Yy_Parse_Failed;
--#endif /* YYNOERRORRECOVERY */

--
--  The following code executes when a syntax error first occurs.
--
procedure Yy_Syntax_Error
    (YypParser : in out YyParser;         -- The parser
     Yymajor   : in     Integer;          -- The major type of the error token
     Yyminor   : in     ParseTOKENTYPE)   -- The minor type of the error token
is
begin
  ParseARG_FETCH;
  ParseCTX_FETCH;
--#define TOKEN yyminor
------------- Begin %syntax_error code -----------------------------------------
%%
------------- End %syntax_error code -------------------------------------------
  ParseARG_STORE; --  Suppress warning about unused %extra_argument variable
  ParseCTX_STORE;
end Yy_Syntax_Error;

--
--  The following is executed when the parser accepts
--
procedure Yy_Accept
  (YypParser : in out YyParser)            -- The parser
is
begin
  ParseARG_FETCH;
  ParseCTX_FETCH;
--#ifndef NDEBUG
  if yyTraceFILE then
    Fprintf (yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  end if;
--#endif
--#ifndef YYNOERRORRECOVERY
  YypParser.yyerrcnt := -1;
--#endif
  pragma Assert (YypParser.Yytos = YypParser.yystack);
  --  Here code is inserted which will be executed whenever the
  --  parser accepts
------------ Begin %parse_accept code ------------------------------------------
%%
------------ End %parse_accept code --------------------------------------------
  ParseARG_STORE; --  Suppress warning about unused %extra_argument variable
  ParseCTX_STORE;
end Yy_Accept;

--  The main parser program.
--  The first argument is a pointer to a structure obtained from
--  "ParseAlloc" which describes the current state of the parser.
--  The second argument is the major token number.  The third is
--  the minor token.  The fourth optional argument is whatever the
--  user wants (and specified in the grammar) and is available for
--  use by the action routines.
--
--  Inputs:
--  <ul>
--  <li> A pointer to the parser (an opaque structure.)
--  <li> The major token number.
--  <li> The minor token number.
--  <li> An option argument of a grammar-specified type.
--  </ul>
--
--  Outputs:
--  None.
--
--  void Parse(
--    void *yyp,                   /* The parser */
--    int yymajor,                 /* The major token code number */
--    ParseTOKENTYPE yyminor       /* The value for the token */
--    ParseARG_PDECL               /* Optional %extra_argument parameter */
--  ){
--    YYMINORTYPE yyminorunion;
--    YYACTIONTYPE yyact;   /* The parser action. */
--  #if !defined(YYERRORSYMBOL) && !defined(YYNOERRORRECOVERY)
--    int yyendofinput;     /* True if we are at the end of input */
--  #endif
--  #ifdef YYERRORSYMBOL
--    int yyerrorhit = 0;   /* True if yymajor has invoked an error */
--  #endif
--    yyParser *yypParser = (yyParser*)yyp;  /* The parser */
--    ParseCTX_FETCH
--    ParseARG_STORE

--    assert( yypParser->yytos!=0 );
--  #if !defined(YYERRORSYMBOL) && !defined(YYNOERRORRECOVERY)
--    yyendofinput = (yymajor==0);
--  #endif

--    yyact = yypParser->yytos->stateno;
--  #ifndef NDEBUG
--    if( yyTraceFILE ){
--      if( yyact < YY_MIN_REDUCE ){
--        fprintf(yyTraceFILE,"%sInput '%s' in state %d\n",
--                yyTracePrompt,yyTokenName[yymajor],yyact);
--      }else{
--        fprintf(yyTraceFILE,"%sInput '%s' with pending reduce %d\n",
--                yyTracePrompt,yyTokenName[yymajor],yyact-YY_MIN_REDUCE);
--      }
--    }
--  #endif

--    do{
--      assert( yyact==yypParser->yytos->stateno );
--      yyact = yy_find_shift_action((YYCODETYPE)yymajor,yyact);
--      if( yyact >= YY_MIN_REDUCE ){
--        yyact = yy_reduce(yypParser,yyact-YY_MIN_REDUCE,yymajor,
--                          yyminor ParseCTX_PARAM);
--      }else if( yyact <= YY_MAX_SHIFTREDUCE ){
--        yy_shift(yypParser,yyact,(YYCODETYPE)yymajor,yyminor);
--  #ifndef YYNOERRORRECOVERY
--        yypParser->yyerrcnt--;
--  #endif
--        break;
--      }else if( yyact==YY_ACCEPT_ACTION ){
--        yypParser->yytos--;
--        yy_accept(yypParser);
--        return;
--      }else{
--        assert( yyact == YY_ERROR_ACTION );
--        yyminorunion.yy0 = yyminor;
--  #ifdef YYERRORSYMBOL
--        int yymx;
--  #endif
--  #ifndef NDEBUG
--        if( yyTraceFILE ){
--          fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
--        }
--  #endif
--  #ifdef YYERRORSYMBOL
--        /* A syntax error has occurred.
--        ** The response to an error depends upon whether or not the
--        ** grammar defines an error token "ERROR".
--        **
--        ** This is what we do if the grammar does define ERROR:
--        **
--        **  * Call the %syntax_error function.
--        **
--        **  * Begin popping the stack until we enter a state where
--        **    it is legal to shift the error symbol, then shift
--        **    the error symbol.
--        **
--        **  * Set the error count to three.
--        **
--        **  * Begin accepting and shifting new tokens.  No new error
--        **    processing will occur until three tokens have been
--        **    shifted successfully.
--        **
--        */
--        if( yypParser->yyerrcnt<0 ){
--          yy_syntax_error(yypParser,yymajor,yyminor);
--        }
--        yymx = yypParser->yytos->major;
--        if( yymx==YYERRORSYMBOL || yyerrorhit ){
--  #ifndef NDEBUG
--          if( yyTraceFILE ){
--            fprintf(yyTraceFILE,"%sDiscard input token %s\n",
--               yyTracePrompt,yyTokenName[yymajor]);
--          }
--  #endif
--          yy_destructor(yypParser, (YYCODETYPE)yymajor, &yyminorunion);
--          yymajor = YYNOCODE;
--        }else{
--          while( yypParser->yytos >= yypParser->yystack
--              && (yyact = yy_find_reduce_action(
--                          yypParser->yytos->stateno,
--                          YYERRORSYMBOL)) > YY_MAX_SHIFTREDUCE
--          ){
--            yy_pop_parser_stack(yypParser);
--          }
--          if( yypParser->yytos < yypParser->yystack || yymajor==0 ){
--            yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
--            yy_parse_failed(yypParser);
--  #ifndef YYNOERRORRECOVERY
--            yypParser->yyerrcnt = -1;
--  #endif
--            yymajor = YYNOCODE;
--          }else if( yymx!=YYERRORSYMBOL ){
--            yy_shift(yypParser,yyact,YYERRORSYMBOL,yyminor);
--          }
--        }
--        yypParser->yyerrcnt = 3;
--        yyerrorhit = 1;
--        if( yymajor==YYNOCODE ) break;
--        yyact = yypParser->yytos->stateno;
--  #elif defined(YYNOERRORRECOVERY)
--        /* If the YYNOERRORRECOVERY macro is defined, then do not attempt to
--        ** do any kind of error recovery.  Instead, simply invoke the syntax
--        ** error routine and continue going as if nothing had happened.
--        **
--        ** Applications can set this macro (for example inside %include) if
--        ** they intend to abandon the parse upon the first syntax error seen.
--        */
--        yy_syntax_error(yypParser,yymajor, yyminor);
--        yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
--        break;
--  #else  /* YYERRORSYMBOL is not defined */
--        /* This is what we do if the grammar does not define ERROR:
--        **
--        **  * Report an error message, and throw away the input token.
--        **
--        **  * If the input token is $, then fail the parse.
--        **
--        ** As before, subsequent error messages are suppressed until
--        ** three input tokens have been successfully shifted.
--        */
--        if( yypParser->yyerrcnt<=0 ){
--          yy_syntax_error(yypParser,yymajor, yyminor);
--        }
--        yypParser->yyerrcnt = 3;
--        yy_destructor(yypParser,(YYCODETYPE)yymajor,&yyminorunion);
--        if( yyendofinput ){
--          yy_parse_failed(yypParser);
--  #ifndef YYNOERRORRECOVERY
--          yypParser->yyerrcnt = -1;
--  #endif
--        }
--        break;
--  #endif
--      }
--    }while( yypParser->yytos>yypParser->yystack );
--  #ifndef NDEBUG
--    if( yyTraceFILE ){
--      yyStackEntry *i;
--      char cDiv = '[';
--      fprintf(yyTraceFILE,"%sReturn. Stack=",yyTracePrompt);
--      for(i=&yypParser->yystack[1]; i<=yypParser->yytos; i++){
--        fprintf(yyTraceFILE,"%c%s", cDiv, yyTokenName[i->major]);
--        cDiv = ' ';
--      }
--      fprintf(yyTraceFILE,"]\n");
--    }
--  #endif
--    return;
--  }

--
--  Return the fallback token corresponding to canonical token iToken, or
--  0 if iToken has no fallback.
--
--  int ParseFallback(int iToken){
--  #ifdef YYFALLBACK
--    assert( iToken<(int)(sizeof(yyFallback)/sizeof(yyFallback[0])) );
--    return yyFallback[iToken];
--  #else
--    (void)iToken;
--    return 0;
--  #endif
--  }
