/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

object Taller4{

  // Funcion suma ingenua secuencial
  def sumaIngenuaSecuencial(a: Int, b: Int): Int = {
    val vector = Vector.fill(b)(a)
    vector.foldLeft(0)(_ + _)
  }
  //Funcion sumaIngenuaParalela
  def sumaIngenuaParalela(a: Int, b: Int, numThreads: Int): Int = {
    val vector = Vector.fill(b)(a)
    val chunkSize = vector.size / numThreads

    def sumChunk(chunk: Vector[Int]): Int = {
      chunk.foldLeft(0)(_ + _)
    }

    val futures = vector.grouped(chunkSize).map { chunk =>
      Future(sumChunk(chunk))
    }.toList

    val sumFuture = Future.sequence(futures)

    val sumResult = Await.result(sumFuture, 10.seconds)
    sumResult.sum
  }

  def sumaRecursivaSecuencial(a: Int, b: Int): Int = {
    if (b == 0) {
      0
    } else if (b % 2 == 1) {
      a + sumaRecursivaSecuencial(a, b - 1)
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
    val resultadoParalelo = sumaIngenuaParalela(4, 3, 2)
    println(resultadoParalelo)
    val resultadorecurusivo = sumaRecursivaParalela(4, 3)
    println(resultadorecurusivo)
  }
 }
