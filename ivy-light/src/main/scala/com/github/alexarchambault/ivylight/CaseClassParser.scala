package com.github.alexarchambault.ivylight

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object CaseClassParser {

  type Path = (Option[String], Int)

  sealed trait Item {
    def at(path: Path*): Option[Item]

    def valueAt(path: Path*): Option[String] =
      at(path: _*) .collect { case Value(repr) => repr }
    def valuesAt(path: Path*): Option[(String, Seq[String])] =
      at(path: _*) .collect { case Container(name, items) if items.forall{ case _: Value => true; case _ => false } =>
        name -> items.collect{case Value(repr) => repr}
      }
  }
  case class Container(name: String, items: Seq[Item]) extends Item {
    def at(path: Path*): Option[Item] =
      if (path.isEmpty)
        Some(this)
      else {
        val (nameOpt, idx) = path.head

        if (nameOpt.forall(name.==) && items.lengthCompare(idx) > 0)
          items(idx).at(path.tail: _*)
        else
          None
      }
  }
  case class Value(repr: String) extends Item {
    def at(path: Path*): Option[Item] =
      if (path.isEmpty)
        Some(this)
      else
        None
  }

  object Parser extends RegexParsers {
    def rawValue: Parser[String] = ("[^" + Regex.quote("()") + ",]+").r
    def parRawValue: Parser[String] = "(" ~> value <~ ")" ^^ { "(" + _.repr + ")" }
    def value: Parser[Value] = (rawValue | parRawValue).+ ^^ { l => Value(l.mkString) }
    
    def ccName: Parser[String] = "[A-Z][a-zA-Z]*".r | ""
    def emptyCC: Parser[Container] = ccName <~ (Regex.quote("(") + """\s*""" + Regex.quote(")")).r ^^ {
      case name  => Container(name, Nil)
    }
    def nonEmptyCC: Parser[Container] = ccName ~ ("(" ~> items <~ ")") ^^ {
      case name ~ items => Container(name, items)
    }

    def cc: Parser[Container] = emptyCC | nonEmptyCC

    def item: Parser[Item] = cc | value

    def sep: Parser[(List[Item], List[Item]) => List[Item]] = rep1sep(",", """\s*""".r) ^^ { sepList =>
      (a, b) => a ++ List.fill(sepList.length - 1 + (if (a.isEmpty) 1 else 0) + (if (b.isEmpty) 1 else 0))(Value("")) ++ b
    }

    def items: Parser[List[Item]] = chainl1(item.map(List(_)).?.map(_.getOrElse(Nil)), sep)
  }

  def parse(s: String): Option[Container] = {
    val res = Parser.parseAll(Parser.cc, s)

    Some(res).filter(_.successful).map(_.get)
  }

}
