package com.acme

sealed trait LExpr
case class LAnd(left: LExpr, right: LExpr) extends LExpr
case class LOr(left: LExpr, right: LExpr) extends LExpr
case class LXor(left: LExpr, right: LExpr) extends LExpr
case class LNot(expr: LExpr) extends LExpr
case class Var(name: String) extends LExpr
case object Contradiction extends LExpr
case object Tautology extends LExpr


object Simplifier {

  def simplify(expr: LExpr): LExpr = expr match {
    case LAnd(Contradiction, _)     ⇒ Contradiction
    case LAnd(_, Contradiction)     ⇒ Contradiction
    case LAnd(LNot(l), r) if l == r ⇒ Contradiction
    case LAnd(l, LNot(r)) if l == r ⇒ Contradiction
    case LAnd(l, r)       if l == r ⇒ l
    case LAnd(l, r)       if l != r ⇒ simplify(LAnd(simplify(l), simplify(r)))

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

    case x ⇒ x
  }

}

object SimplifierMain extends App {

  val se = Simplifier.simplify(
    LAnd(
      LOr(Var("a"), LNot(Var("a"))),
      LXor(Var("a"), Var("a"))
    )
  )
  println(se)

}