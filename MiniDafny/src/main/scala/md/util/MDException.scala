package md.util

import scala.util.parsing.input._

class MDException(val msg: String, val pos: Position) extends Exception { 
  override def toString(): String = {
    if (pos != null) {
      val errPos = pos.line + ":" + pos.column + "\n"
      errPos + pos.longString + "\n\n" + msg
    } else msg
  }
}