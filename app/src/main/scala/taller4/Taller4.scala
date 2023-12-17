/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.Key.verbose
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import org.scalameter._

import scala.Predef.->

object Taller4{

  // Funcion suma ingenua secuencial
  def sumaIngenuaSecuencial(a: Int, b: Int): Int = {
    // Se crea un vector 'vector' con 'b' elementos, todos iguales a 'a'.
    val vector = Vector.fill(b)(a)
    //Utilizamos el método 'foldLeft' para realizar la suma de los elementos en el vector.
    // El valor inicial es 0, y la función anónima (_ + _) suma dos elementos.
    vector.foldLeft(0)(_ + _)
  }
  //Funcion sumaIngenuaParalela
  def sumaIngenuaParalela(a: Int, b: Int, numThreads: Int): Int = {
    // Crear un vector de tamaño 'b' con todos los elementos iguales a 'a'.
    val vector = Vector.fill(b)(a)
    // Calcular el tamaño de cada "chunk" (trozo) del vector para distribuir el trabajo entre hilos.
    val chunkSize = vector.size / numThreads

    // Función para calcular la suma de un "chunk" (trozo) de elementos.
    def sumChunk(chunk: Vector[Int]): Int = {
      chunk.foldLeft(0)(_ + _) // Utilizar foldLeft para sumar los elementos del "chunk".
    }

    // Dividir el vector en "chunks" y mapear cada "chunk" a un Future que calcula su suma.
    val futures = vector.grouped(chunkSize).map { chunk =>
      Future(sumChunk(chunk)) // Calcular la suma de cada "chunk" en un Future.
    }.toList

    // Combinar todos los Futures en uno solo para esperar su finalización.
    val sumFuture = Future.sequence(futures)

    // Esperar hasta que todos los cálculos se completen y obtener los resultados parciales.
    val sumResult = Await.result(sumFuture, 10.seconds)

    // Sumar los resultados parciales para obtener el resultado final.
    sumResult.sum
  }

  // Funcion suma recursiva secuencial
  def sumaRecursivaSecuencial(a: Int, b: Int): Int = {
    // Si 'b' es igual a 0, retornamos 0 (caso base de la recursión)
    if (b == 0) {
      0
      // Si 'b' es impar, calculamos 'a' + sumaRecursivaSecuencial(a, b - 1)
    } else if (b % 2 == 1) {
      a + sumaRecursivaSecuencial(a, b - 1)
      // Si 'b' es par, calculamos la mitad de la suma y la sumamos consigo misma
    } else {
      val halfSum = sumaRecursivaSecuencial(a, b / 2)
      halfSum + halfSum
    }
  }

  // Funcion suma recursiva paralela
  def sumaRecursivaParalela(a: Int, b: Int): Int = {
    // Caso base: cuando b es 0, la suma es 0.
    if (b == 0) {
      0
    }
    // Si b es impar, calculamos a + sumaRecursivaParalela(a, b - 1) en un Future.
    else if (b % 2 == 1) {
      // Creamos un Future que calcula la suma de a y sumaRecursivaParalela(a, b - 1).
      val futureSum = Future(a + sumaRecursivaParalela(a, b - 1))
      // Esperamos el resultado del Future y lo retornamos.
      Await.result(futureSum, 10.seconds)
    }
    // Si b es par, dividimos el trabajo en dos mitades y calculamos cada mitad en paralelo.
    else {
      // Calculamos la mitad de la suma en paralelo utilizando un Future.
      val halfFuture = Future(sumaRecursivaParalela(a, b / 2))
      // Calculamos la suma completa combinando los resultados parciales en un Future con 'for'.
      val fullFuture = for {
        halfSum1 <- halfFuture
        halfSum2 <- halfFuture
      } yield halfSum1 + halfSum2
      // Esperamos el resultado del Future y lo retornamos.
      Await.result(fullFuture, 10.seconds)
    }
  }

  def main(args: Array[String]): Unit = {
    // Utiliza 2 hilos
/*    val resultadoParalelo = sumaIngenuaParalela(4, 3, 2)
    println(resultadoParalelo)
    val resultadorecurusivo = sumaRecursivaParalela(4, 3)
    println(resultadorecurusivo)*/
/*    val benchConfig = config(
      Key.exec.benchRuns -> 10, // Número de ejecuciones para cada prueba
      exec.independentSamples -> 1, // Número de muestras independientes
      verbose -> true             // Mostrar información detallada
    )
    // Medir el tiempo de ejecución de la solución ingenua secuencial
    val ingenuaSecuencialTime = withWarmer(new Warmer.Default) measure {
      sumaIngenuaSecuencial(4, 1000000)
    }

    // Medir el tiempo de ejecución de la solución ingenua paralela
    val ingenuaParalelaTime = withWarmer(new Warmer.Default) measure {
      sumaIngenuaParalela(4, 1000000, 2)
    }

    // Imprimir los resultados
    println(s"Tiempo de ejecución de la solución ingenua secuencial: $ingenuaSecuencialTime")
    println(s"Tiempo de ejecución de la solución ingenua paralela: $ingenuaParalelaTime")*/
  }
  }
