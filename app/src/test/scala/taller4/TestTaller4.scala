/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
    test("Caso 1: Prueba de solucionIngenua con valores pequeños") {
        val resultado = Taller4.sumaIngenuaSecuencial(2,3)
        assert(resultado == 6)
    }

    test("Caso 2: Prueba de solucionRecursiva con valores pequeños") {
        val resultado = Taller4.sumaRecursivaSecuencial(2, 3)
        assert(resultado == 6)
    }

    test("Caso 3: Prueba de solucionIngenua con valores grandes") {
        val resultado = Taller4.sumaIngenuaSecuencial(100, 1000)
        assert(resultado == 100000)
    }

    test("Caso 4: Prueba de solucionRecursiva con valores grandes") {
        val resultado = Taller4.sumaRecursivaSecuencial(100, 1000)
        assert(resultado ==  100000)
    }
}
