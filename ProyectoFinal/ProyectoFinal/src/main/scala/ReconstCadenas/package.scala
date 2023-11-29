/*
Proyecto Final:

Wilson Andrés Mosquera Zapata <202182116>
 <2021>

28/11/2023

Archivo: package.scala (ReconstCadenas)

*/

import Oraculo._

package object ReconstCadenas {


  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

//3.1. Implementando la solución ingenua.
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida.

    // Generar todas las posibles secuencias de longitud n
    val secuencias = (1 to n).foldLeft(Set(Seq.empty[Char])) { (acc, _) =>
      acc.flatMap(seq => alfabeto.map(char => seq :+ char))
    }

    // Encontrar la primera subsecuencia que es parte de la cadena buscada según el oráculo
    val resultado = secuencias.to(LazyList).find(o)

    // Devolver la subsecuencia encontrada o una secuencia vacía si no se encuentra ninguna
    resultado.getOrElse(Seq.empty[Char])
  }

// 3.2. Implementando la solución mejorada.
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s.

      // Inicializar el conjunto de subcadenas generadas de longitud 0
      var subcadenasGeneradas: Set[Seq[Char]] = Set(Seq.empty[Char])

      // Generar subcadenas de longitud k para cada k <= n
      for (k <- 1 to n) {
        // Generar nuevas subcadenas concatenando con el alfabeto
        val nuevasSubcadenas = subcadenasGeneradas.flatMap(seq => alfabeto.map(char => seq :+ char))

        // Filtrar las subcadenas usando el oráculo
        val subcadenasValidas = nuevasSubcadenas.filter(o)

        // Si hay subcadenas válidas de longitud N, retornar la primera encontrada
        if (subcadenasValidas.exists(_.length == n)) {
          return subcadenasValidas.find(_.length == n).get
        }

        // Actualizar el conjunto de subcadenas generadas
        subcadenasGeneradas = subcadenasValidas
      }

      // Devolver una cadena vacía si no se encuentra ninguna solución
      Seq.empty[Char]
    }

// 3.3. Implementando la solución turbo.
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

      def generarCadenaTurbo(currentLength: Int, subcadenasActuales: Set[Seq[Char]]): Seq[Char] = {
        val nuevasSubcadenas = subcadenasActuales.flatMap(subcadena1 => subcadenasActuales.map(subcadena2 => subcadena1 ++ subcadena2))
        val subcadenasFiltradas = nuevasSubcadenas.filter(o)

        // Obtener la primera subcadena válida de longitud N
        subcadenasFiltradas.find(_.length == n).getOrElse {
          // Si el tamaño actual supera N, devolver una cadena vacía
          if (currentLength > n) Seq.empty[Char]
          else generarCadenaTurbo(currentLength * 2, subcadenasFiltradas)
        }
      }

      // Inicializar el conjunto de subcadenas generadas de longitud 1
      val subcadenasIniciales: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

      generarCadenaTurbo(2, subcadenasIniciales)
    }


// 3.4. Implementando la solución turbo mejorada.
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // Recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2),
    // y un oráculo para esa secuencia. Devuelve la secuencia reconstruida.
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s.
    // Usa el filtro para ir más rápido.

    // Función principal para generar la cadena turbo mejorada.
    def generarCadenaTurbo(currentLength: Int, subcadenasActuales: Set[Seq[Char]]): Seq[Char] = {
      // Verifica si una concatenación de dos subcadenas es válida según las restricciones dadas.
      def verificarConcatenacion(subcadena1: Seq[Char], subcadena2: Seq[Char]): Boolean = {
        val concatenacion = subcadena1 ++ subcadena2
        subcadenasActuales.exists { w =>
          w.sliding(currentLength, 1).forall(sub => concatenacion.containsSlice(sub))
        }
      }

      // Filtra las subcadenas actuales eliminando las que no cumplen con las restricciones.
      def filtrarSubcadenas(subcadenasActuales: Set[Seq[Char]], currentLength: Int): Set[Seq[Char]] = {
        subcadenasActuales.filter { subcadena1 =>
          subcadenasActuales.forall(subcadena2 => verificarConcatenacion(subcadena1, subcadena2))
        }
      }

      // Genera nuevas subcadenas concatenando las actuales consigo mismas.
      val nuevasSubcadenas = subcadenasActuales.flatMap(seq1 => subcadenasActuales.map(seq2 => seq1 ++ seq2))
      // Filtra las nuevas subcadenas con el oráculo.
      val subcadenasFiltradas = nuevasSubcadenas.filter(o)

      // Encuentra la primera subcadena válida de longitud N.
      val resultado = subcadenasFiltradas.to(LazyList).find(_.length == n)
      if (resultado.isDefined) resultado.get
      // Si no se encuentra y el tamaño actual supera N, devuelve una cadena vacía.
      else if (currentLength > n) Seq.empty[Char]
      // En caso contrario, realiza una llamada recursiva con el doble del tamaño y las subcadenas filtradas.
      else generarCadenaTurbo(currentLength * 2, filtrarSubcadenas(subcadenasFiltradas, currentLength))
    }

    // Conjunto inicial de subcadenas de longitud 1 del alfabeto.
    val subcadenasIniciales: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet

    // Llamada inicial a la función de generación.
    generarCadenaTurbo(2, subcadenasIniciales)
  }


}
