/*
Proyecto Final:

Wilson Andrés Mosquera Zapata <202182116>
 <2021>

28/11/2023

Archivo: package.scala (Oraculo)

*/

package object Oraculo {
  val alfabeto=Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def crearOraculo(delay:Int)(c:Seq[Char]):Oraculo ={
    def esSubcadena(s:Seq[Char]):Boolean = {
      Thread.sleep(delay)
      c.containsSlice(s)
    }
    esSubcadena
  }
}
