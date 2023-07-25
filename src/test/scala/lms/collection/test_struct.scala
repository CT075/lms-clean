package lms
package core

import core._
import core.stub._
import macros.SourceContext
import macros.RefinedManifest

import lms.collection.mutable._

class StructTest extends TutorialFunSuite {
  val under = "backend/"

  @CStruct case class Complex(real: Double, image: Double)

  test("basic_struct_is_OK") {
    val driver = new DslDriverC[Complex, Complex] with StructOps { q =>
      override val codegen = new DslGenC with CCodeGenStruct {
        val IR: q.type = q
      }

      CStructFields.fields[Complex](())

      /*
      implicit class ComplexOps(p: Rep[Complex]) {
        def real: Rep[Double] = StructOpsImpl.readField[Complex, Double](p, "real")
        def image: Rep[Double] = StructOpsImpl.readField[Complex, Double](p, "image")
        def real_=(v: Rep[Double]): Unit = StructOpsImpl.writeField[Complex, Double](p, "real", v)
        def image_=(v: Rep[Double]): Unit = StructOpsImpl.writeField[Complex, Double](p, "image", v)
      }
      */

      @virtualize
      def snippet(arg: Rep[Complex]) = {
        arg.real = 1.23 // that is s.writeField("real", 1.23)
        arg.image = arg.real
        arg
      }
    }
    check("basic_struct", driver.code, "c")
  }
}
