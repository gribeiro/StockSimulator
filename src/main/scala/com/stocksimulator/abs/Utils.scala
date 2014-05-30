package com.stocksimulator.abs
/*Mover para helpers*/
object Utils {
  def md5Hash(text: String): String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }
  def perm[T](disc: List[(T, T)]): List[List[(T, T)]] = {

    def dist(disc: List[List[(T, T)]], elem: (T, T)): List[List[(T, T)]] = {
      disc.map {
        d => elem :: d
      }
    }
    def filterKey(f: T)(kv: (T, T)) = kv._1 != f

    disc match {
      case Nil => List(Nil)
      case (key, v) :: Nil => List(List((key, v)))
      case l @ ((key, v) :: xs) =>
        val filtered = l.filter(filterKey(key))
        val join1 = dist(perm(filtered), (key, v))
        val withRemoved = l.filter(_ != (key, v))
        if (withRemoved.exists(_._1 == key)) join1 ++ perm(withRemoved) else join1
        
    }
  } //> perm: (disc: List[(String, String)])List[List[(String, String)]]
  /*
  def currentPosition: String = macro _currentPosition;
  def _currentPosition(c: Context): c.Expr[String] = {
    import c.universe._
    val pos = c.enclosingPosition
    c.Expr(Literal(Constant(
      s"${pos.source.path}: line ${pos.line}, column ${pos.column}")))
  }*/
}