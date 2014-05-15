package com.stocksimulator.abs

object Utils {
def md5Hash(text: String) : String = java.security.MessageDigest.getInstance("MD5").digest(text.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
  def perm(disc: List[(String, String)]): List[List[(String, String)]] = {

    def dist(disc: List[List[(String, String)]], elem: (String, String)): List[List[(String, String)]] = {
      disc.map {
        d => elem :: d
      }
    }
    def filterKey(f: String)(kv: (String, String)) = kv._1 != f

    disc match {
      case Nil => List(Nil)
      case (key, v) :: Nil => List(List((key, v)))
      case l @ ((key, v) :: xs) =>
        val filtered = l.filter(filterKey(key))
        val join1 = dist(perm(filtered), (key, v))
        val withRemoved = l.filter(_ != (key, v))
        if (withRemoved.exists(_._1 == key)) join1 ++ perm(withRemoved) else join1
    }
  }  //> perm: (disc: List[(String, String)])List[List[(String, String)]]
  
  
}