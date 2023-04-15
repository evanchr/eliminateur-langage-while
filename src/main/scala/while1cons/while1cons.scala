package fr.istic.cal.while1cons

import scala.util.Try

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object While1cons {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN ELIMINATEUR D'EXPRESSIONS COMPLEXES POUR LE LANGAGE WHILE
   *
   */

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et une expression qui contient le résultat
   */
  def while1ConsExprV(expression: Expression): (List[Command], Variable) = {
    expression match {
      case Nl => val nv = NewVar.make() ; (List(Set(nv, Nl)), nv)
      case Cst(name) => val nv = NewVar.make() ; (List(Set(nv, Cst(name))), nv)
      case VarExp(name) => (Nil, Var(name))
      case Cons(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1)
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2)
        val nv = NewVar.make()
        (lcarg1 ++ lcarg2 ++ List(Set(nv, Cons(VarExp(nvarg1),VarExp(nvarg2)))),nv)
      case Hd(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg)
        val nv = NewVar.make()
        (lcarg ++ List(Set(nv,Hd(VarExp(nvarg)))), nv)
      case Tl(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg)
        val nv = NewVar.make()
        (lcarg ++ List(Set(nv,Tl(VarExp(nvarg)))), nv)
      case Eq(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1)
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2)
        val nv = NewVar.make()
        (lcarg1 ++ lcarg2 ++ List(Set(nv, Eq(VarExp(nvarg1),VarExp(nvarg2)))),nv)
    }
  }

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et la variable qui contient le résultat
   */
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = {
    expression match {
      case Nl => (List(),Nl)
      case Cst(name) => (List(),Cst(name))
      case VarExp(name) => (List(),VarExp(name))
      case Cons(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1)
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2)
        (lcarg1 ++ lcarg2, Cons(VarExp(nvarg1),VarExp(nvarg2)))
      case Hd(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg)
        (lcarg, Hd(VarExp(nvarg)))
      case Tl(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg)
        (lcarg, Tl(VarExp(nvarg)))
      case Eq(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1)
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2)
        (lcarg1 ++ lcarg2, Eq(VarExp(nvarg1),VarExp(nvarg2)))
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @return une liste de commandes ayant un seul construteur par expression
   * et ayant le même effet que la commande
   */
  // TODO TP4
  def while1ConsCommand(command: Command): List[Command] = {
    command match {
      case Nop => ???
      case Set(variable, expression) => ???
      case While(condition, body) => ???
      case For(count, body) => ???
      case If(condition, then_commands, else_commands) => ???
    }
  }

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @return une liste de commandes ayant un seul construteur par expression
   * et ayant le même effet que la commande
   */
  // TODO TP4
  def while1ConsCommands(commands: List[Command]): List[Command] = ???

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  // TODO TP4
  def while1ConsProgr(program: Program): Program = ???

  def main(args: Array[String]): Unit = {

    // vous pouvez ici tester manuellement vos fonctions par des print

  }
}