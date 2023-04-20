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
      case Nl => val nv = NewVar.make() ; (List(Set(nv, Nl)), nv) // Création d'une variable temporaire afin de simplifier le constructeur Nil
      case Cst(name) => val nv = NewVar.make() ; (List(Set(nv, Cst(name))), nv) // Création d'une variable temporaire afin de simplifier le constructeur Cst
      case VarExp(name) => (Nil, Var(name)) // Pas besoin de variable temporaire, on transforme la variable d'expression en variable
      case Cons(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1) // Appel récursif sur le premier argument car il peut s'agir d'une liste d'expressions complexes
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2) // Appel récursif sur le deuxième élément pour la même raison
        val nv = NewVar.make() // Création d'une variable temporaire
        (lcarg1 ++ lcarg2 ++ List(Set(nv, Cons(VarExp(nvarg1),VarExp(nvarg2)))),nv) 
        // On concatène la liste de commandes de arg1, celle de arg2 et celle du Cons qu'on appelle avec les variables temporaires
      case Hd(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg) // Appel récursif sur l'argument car il peut s'agir d'une liste d'expressions complexes
        val nv = NewVar.make() // Création d'une variable temporaire
        (lcarg ++ List(Set(nv,Hd(VarExp(nvarg)))), nv) 
        // On concatène la liste de commandes de l'argument et celle de Hd qu'on appelle avec la variable temporaire de son argument
      case Tl(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg) // Appel récursif sur l'argument car il peut s'agir d'une liste d'expressions complexes
        val nv = NewVar.make() // Création d'une variable temporaire
        (lcarg ++ List(Set(nv,Tl(VarExp(nvarg)))), nv)
        // On concatène la liste de commandes de l'argument et celle de Tl qu'on appelle avec la variable temporaire de son argument
      case Eq(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1) // Appel récursif sur le premier argument car il peut s'agir d'une liste d'expressions complexes
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2) // Appel récursif sur le deuxième élément pour la même raison
        val nv = NewVar.make() // Création d'une variable temporaire
        (lcarg1 ++ lcarg2 ++ List(Set(nv, Eq(VarExp(nvarg1),VarExp(nvarg2)))),nv)
        // On concatène la liste de commandes de arg1, celle de arg2 et celle du Cons qu'on appelle avec les variables temporaires
    }
  }

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et la variable qui contient le résultat
   */
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = {
    expression match {
      case Nl => (List(),Nl) // On renvoie aucune commande et l'expression Nil
      case Cst(name) => (List(),Cst(name)) // On renvoie aucune commande et l'expression Cst(name)
      case VarExp(name) => (List(),VarExp(name)) // On renvoie aucune commande et l'expression VarExp(name)
      case Cons(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1) // Appel récursif sur le premier argument car il peut s'agir d'une liste d'expressions complexes
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2) // Appel récursif sur le deuxième élément pour la même raison
        (lcarg1 ++ lcarg2, Cons(VarExp(nvarg1),VarExp(nvarg2)))
        // On concatène la liste de commandes de arg1 et celle de arg2, et on renvoie l'expression Cons avec les variables temporaires en arguments
      case Hd(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg) // Appel récursif sur l'argument car il peut s'agir d'une liste d'expressions complexes
        (lcarg, Hd(VarExp(nvarg)))
        // On renvoie la liste de commandes de l'argument et l'expression Hd avec la variable temporaire en argument
      case Tl(arg) => 
        val (lcarg,Var(nvarg)) = while1ConsExprV(arg) // Appel récursif sur l'argument car il peut s'agir d'une liste d'expressions complexes
        (lcarg, Tl(VarExp(nvarg)))
        // On renvoie la liste de commandes de l'argument et l'expression Tl avec la variable temporaire en argument
      case Eq(arg1, arg2) => 
        val (lcarg1,Var(nvarg1)) = while1ConsExprV(arg1) // Appel récursif sur le premier argument car il peut s'agir d'une liste d'expressions complexes
        val (lcarg2,Var(nvarg2)) = while1ConsExprV(arg2) // Appel récursif sur le deuxième élément pour la même raison
        (lcarg1 ++ lcarg2, Eq(VarExp(nvarg1),VarExp(nvarg2)))
        // On concatène la liste de commandes de arg1 et celle de arg2, et on renvoie l'expression Eq avec les variables temporaires en arguments
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
  def while1ConsCommand(command: Command): List[Command] = {
    command match {
      case Nop => List(Nop)
      case Set(variable, expression) => 
        val (lcarg,expr) = while1ConsExprSE(expression)
        lcarg ++ List(Set(variable,expr))
      case While(condition, body) => 
        // XXX à optimiser ?
        val (lcarg,expr) = while1ConsExprSE(condition)
        val (lcarg_2,expr_2) = while1ConsExprV(expr)
        val list_com = while1ConsCommands(body) ++ lcarg ++ lcarg_2
        val vari = expr_2 match {case Var(name) => Var(name)}
        lcarg ++ lcarg_2 ++ List(While(VarExp(vari.name),list_com))
      case For(count, body) => 
        // XXX à optimiser ?
        val (lcarg,expr) = while1ConsExprSE(count)
        val (lcarg_2,expr_2) = while1ConsExprV(expr)
        val list_com = while1ConsCommands(body)
        val vari = expr_2 match {case Var(name) => Var(name)}
        lcarg ++ lcarg_2 ++ List(For(VarExp(vari.name),list_com))
      case If(condition, then_commands, else_commands) => 
        // XXX à optimiser ?
        val (lcarg,expr) = while1ConsExprSE(condition)
        val (lcarg_2,expr_2) = while1ConsExprV(expr)
        val vari = expr_2 match {case Var(name) => Var(name)}
        lcarg ++ lcarg_2 ++ List(If(VarExp(vari.name), while1ConsCommands(then_commands), while1ConsCommands(else_commands)))
    }
  }

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @return une liste de commandes ayant un seul construteur par expression
   * et ayant le même effet que la commande
   */
  def while1ConsCommands(commands: List[Command]): List[Command] = {
    commands match {
      case Nil => throw ExceptionListeVide
      case head :: Nil => while1ConsCommand(head)
      case head :: next => while1ConsCommand(head) ++ while1ConsCommands(next)
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param is : une liste de spécifications d'indentation
   * @return une liste de chaînes représentant la syntaxe concrète du programme
   */
  def while1ConsProgr(program: Program): Program = {
    program match {
      case Progr(in, body, out) => Progr(in, while1ConsCommands(body), out)
    }
  }

  def main(args: Array[String]): Unit = {

    // vous pouvez ici tester manuellement vos fonctions par des print
    println(while1ConsExprSE(Cons(Nl, Cons(Nl, Cons(Nl, Nl)))))

  }
}