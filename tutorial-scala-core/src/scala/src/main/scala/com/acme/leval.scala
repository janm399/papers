package com.acme

/**
  * Just like the numerical expression, it is possible to define logical expression
  * as the sum of the variants
  */
sealed trait LExpr
case class LAnd(left: LExpr, right: LExpr) extends LExpr
case class LOr(left: LExpr, right: LExpr) extends LExpr
case class LXor(left: LExpr, right: LExpr) extends LExpr
case class LNot(expr: LExpr) extends LExpr
case class Var(name: String) extends LExpr
case object Contradiction extends LExpr
case object Tautology extends LExpr

/**
  * Logical expression simplifier demonstrates somewhat advanced usage
  * of pattern matches.
  */
object Simplifier {

  /**
    * Simplifies the expression `expr`, if possible.
    *
    * @param expr the expression to be simplified
    * @return the simplified expression
    */
  def simplify(expr: LExpr): LExpr = expr match {
    // The rules for conjunction (e.g. false && _ => false; X && ~X => false; etc.)
    case LAnd(Contradiction, _)     ⇒ Contradiction
    case LAnd(_, Contradiction)     ⇒ Contradiction
    case LAnd(LNot(l), r) if l == r ⇒ Contradiction
    case LAnd(l, LNot(r)) if l == r ⇒ Contradiction
    case LAnd(l, r)       if l == r ⇒ l
    case LAnd(l, r)       if l != r ⇒ simplify(LAnd(simplify(l), simplify(r)))

    // The rules for disjunction (e.g. true || _ => true; X || ~X => true; etc.)
    case LOr(l, LNot(r))  if l == r ⇒ Tautology
    case LOr(LNot(l), r)  if l == r ⇒ Tautology
    case LOr(Tautology, _)          ⇒ Tautology
    case LOr(_, Tautology)          ⇒ Tautology
    case LOr(l, r)        if l == r ⇒ l
    case LOr(l, r)        if l != r ⇒ simplify(LOr(simplify(l), simplify(r)))

    case LXor(l, r)       if l == r ⇒ Contradiction
    case LXor(l, LNot(r)) if l == r ⇒ Tautology
    case LXor(LNot(l), r) if l == r ⇒ Tautology
    case LXor(l, r)       if l != r ⇒ simplify(LXor(simplify(l), simplify(r)))

    case LNot(LNot(x))              ⇒ x

    // Finally, the case where there no pattern matched => no rules in our simplifier
    // there is nothing to be simplified.
    case x                          ⇒ x
  }

}

object SimplifierMain extends App {

  // The usage of the simplifier is simple, though constructing the
  // expressions without any kind of DSL is somewhat clumsy
  val se = Simplifier.simplify(
    LAnd(
      LOr(Var("a"), LNot(Var("a"))),
      LXor(Var("a"), Var("a"))
    )
  )
  println(se)

}