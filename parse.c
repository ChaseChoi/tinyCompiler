/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode * stmt_sequence(void);
static TreeNode * statement(void);
static TreeNode * if_stmt(void);
static TreeNode * repeat_stmt(void);
static TreeNode * assign_stmt(void);
static TreeNode * read_stmt(void);
static TreeNode * write_stmt(void);
static TreeNode * exp(void);
static TreeNode * simple_exp(void);
static TreeNode * term(void);
static TreeNode * factor(void);
static TreeNode * while_stmt(void); // 1. Add while statement declaration
static TreeNode * dowhile_stmt(void); // 2. Add do while statment declaration
static TreeNode * for_stmt(void); // 3. Add for statement declaration

static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token -> ");
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}

TreeNode * stmt_sequence(void)
{ TreeNode * t = statement();
  TreeNode * p = t;
  /* 1. Add 'WHILE' */
  /* 3. Add `ENDDO` */
  while ((token!=ENDFILE) && (token!=END) &&
         (token!=ELSE) && (token!=UNTIL) && (token!=WHILE) && (token!=ENDWHILE) && (token!=ENDDO))
  { TreeNode * q;
    match(SEMI);
    q = statement();
    if (q!=NULL) {
      if (t==NULL) t = p = q;
      else /* now p cannot be NULL either */
      { p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}

TreeNode * statement(void)
{ TreeNode * t = NULL;
  switch (token) {
    case IF : t = if_stmt(); break;
    case REPEAT : t = repeat_stmt(); break;
    case ID : t = assign_stmt(); break;
    case READ : t = read_stmt(); break;
    case WRITE : t = write_stmt(); break;
    case WHILE : t = while_stmt(); break;    // 1. Add While-stmt
    case DO : t = dowhile_stmt(); break;   // 2. Add do-while stmt
    case FOR : t = for_stmt(); break;    // 3. Add for stmt
    default : syntaxError("unexpected token -> ");
              printToken(token,tokenString);
              token = getToken();
              break;
  } /* end case */
  return t;
}

TreeNode * if_stmt(void)
{ TreeNode * t = newStmtNode(IfK);
  match(IF);
  match(LPAREN);
  if (t!=NULL) t->child[0] = exp();
  match(RPAREN);
  match(THEN);
  if (t!=NULL) t->child[1] = stmt_sequence();
  if (token==ELSE) {
    match(ELSE);
    if (t!=NULL) t->child[2] = stmt_sequence();
  }
  match(END);
  return t;
}

TreeNode * repeat_stmt(void)
{ TreeNode * t = newStmtNode(RepeatK);
  match(REPEAT);
  if (t!=NULL) t->child[0] = stmt_sequence();
  match(UNTIL);
  if (t!=NULL) t->child[1] = exp();
  return t;
}

TreeNode * assign_stmt(void)
{ TreeNode * t = newStmtNode(AssignK);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  match(ASSIGN);
  if (t!=NULL) t->child[0] = exp();
  return t;
}

TreeNode * read_stmt(void)
{ TreeNode * t = newStmtNode(ReadK);
  match(READ);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  return t;
}

TreeNode * write_stmt(void)
{ TreeNode * t = newStmtNode(WriteK);
  match(WRITE);
  if (t!=NULL) t->child[0] = exp();
  return t;
}

TreeNode * exp(void)
{ TreeNode * t = simple_exp();
  if ((token==LT)||(token==EQ)||(token==GT)) {  /* 5. Add support of greater than symbol */
    TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
    }
    match(token);
    if (t!=NULL)
      t->child[1] = simple_exp();
  }
  return t;
}

TreeNode * simple_exp(void)
{ TreeNode * t = term();
  while ((token==PLUS)||(token==MINUS))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      t->child[1] = term();
    }
  }
  return t;
}

TreeNode * term(void)
{ TreeNode * t = factor();
  while ((token==TIMES)||(token==OVER)||(token==MOD))  /* 5. Add support of MOD operator */
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      p->child[1] = factor();
    }
  }
  return t;
}

TreeNode * factor(void)
{ TreeNode * t = NULL;
  switch (token) {
    case NUM :
      t = newExpNode(ConstK);
      if ((t!=NULL) && (token==NUM))
        t->attr.val = atoi(tokenString);
      match(NUM);
      break;
    case ID :
      t = newExpNode(IdK);
      if ((t!=NULL) && (token==ID))
        t->attr.name = copyString(tokenString);
      match(ID);
      break;
    case LPAREN :
      match(LPAREN);
      t = exp();
      match(RPAREN);
      break;
    default:
      syntaxError("unexpected token -> ");
      printToken(token,tokenString);
      token = getToken();
      break;
    }
  return t;
}

/* 1. ADD WHILE STATEMENT
 * Description: add while statement
 * Parameters: void
 * Return: TreeNode
 * */
TreeNode * while_stmt(void) {
  TreeNode * node = newStmtNode(WhileK);
  match(WHILE);
  if (node != NULL) {
    node -> child[0] = exp();
  }
  match(DO);
  if (node != NULL) {
    node -> child[1] = stmt_sequence();
  }
  match(ENDWHILE);
  return node;
}

/* 2. ADD DO-WHILE STATEMENT
 * Description: add do while statement
 * Parameters: void
 * Return: TreeNode
 * */
TreeNode * dowhile_stmt(void) {
  TreeNode * node = newStmtNode(DoWhileK);
  match(DO);
  if (node != NULL) {
    node -> child[0] = stmt_sequence();
  }
  match(WHILE);
  match(LPAREN);  // left parenthesis
  if (node != NULL) {
    node -> child[1] = exp();
  }
  match(RPAREN); // right parenthesis
  return node;
}
/* 3. ADD FOR STATEMENT
 * Description: add for statement
 * Parameters: void
 * Return: TreeNode
 * */

 TreeNode * for_stmt(void) {
   TreeNode * node = newStmtNode(ForK);
   match(FOR);
   if (node != NULL && token == ID) {
     node -> attr.name = copyString(tokenString);
   }
   match(ID);
   match(ASSIGN);
   if (node != NULL) {
     node -> child[0] = simple_exp();
   }
   if (token == TO) {  // increment
     match(TO);
   }
   // 4. Add 'DOWNTO' reserved word
   if (token == DOWNTO) { // decrement
     match(DOWNTO);
   }
   if (node != NULL) {
     node -> child[1] = simple_exp();
   }
   match(DO);
   if (node != NULL) {
     node -> child[2] = stmt_sequence();
   }
   match(ENDDO);
   return node;
 }

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
  token = getToken();
  t = stmt_sequence();
  if (token!=ENDFILE)
    syntaxError("Code ends before file\n");
  return t;
}
