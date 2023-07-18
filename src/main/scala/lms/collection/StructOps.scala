package lms.collection.mutable

import lms.core._
import lms.util._
import lms.core.stub._
import lms.core.Backend._
import lms.core.virtualize
import lms.core.utils.time
import lms.macros.SourceContext
import lms.macros.RefinedManifest


import language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.util.matching.Regex

trait StructOps extends Base with ArrayOps {
  /*
  class Pointer[T <: Struct:RefinedManifest](val ptr: Rep[LongArray[T]], base: Rep[LongArray[T]]) {
    def readField[U: Manifest](field: String): Rep[U] =
      Wrap[U](Adapter.g.reflectRead("reffield_get", Unwrap(ptr), Unwrap(field))(Unwrap(base)))
    def writeField[U: Manifest](field: String, v: Rep[U]): Unit =
      Adapter.g.reflectWrite("reffield_set", Unwrap(ptr), Unwrap(field), Unwrap(v))(Unwrap(base))
    def deref: Rep[T] =
      Wrap[T](Adapter.g.reflectWrite("deref", Unwrap(ptr))(Adapter.CTRL))
  }
  */

  object StructOpsImpl {
    // TODO: add a better constraint on T
    def readField[T: RefinedManifest, U: Manifest](p: Rep[T], field: String): Rep[U] =
      Wrap[U](Adapter.g.reflectRead("struct_get", Unwrap(p), Unwrap(field))(Unwrap(p)))

    def writeField[T: RefinedManifest, U: Manifest](p: Rep[T], field: String, v: Rep[U]): Unit =
      Adapter.g.reflectWrite("struct_set", Unwrap(p), Unwrap(field), Unwrap(v))(Unwrap(p))
  }
}

object CStruct_Impl {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val List(a) = annottees
    a.tree match {
      case q"case class $name(..${fields: Seq[ValDef]})" =>
        val manifestName = internal.reificationSupport.freshTermName(name.toString+"Manifest")
        val fieldDecls = fields.map { f => q"""(${f.name.toString}, manifest[${f.tpt}])""" }
        val fieldsListName = internal.reificationSupport.freshTermName(name.toString+"Fields")
        val fieldsList = fields.map { f => q"""(${f.name.toString}, ${f.tpt.toString})""" }
        val res = c.Expr(q"""
          case class $name(..$fields)
          implicit val $manifestName = new RefinedManifest[$name] {
            def fields: List[(String, Manifest[_])] = List(..$fieldDecls)
            def runtimeClass = classOf[$name]
            override def name = Some(${name.toString})
          }
          val $fieldsListName = $fieldsList
        """)
        res
    }
  }
}

object CStruct_Fields {
  // TODO(cwong): This is gross. We need the class name as a string, and also
  // the magic name [FooFields] to invoke this macro. Instead, either use
  // reflection or a nested macro definition to emit this code.
  def impl(c: Context)(name: c.Expr[String], fields: c.Expr[(String, String)]*) = {
    import c.universe._

    val opsClassName = internal.reificationSupport.freshTypeName(name.toString+"Ops")

    val p = TermName("p")

    val getters = fields.map { field =>
      val (fname_, ty) = field
      val fname = TermName(fname_)
      q"""def $fname: Rep[$ty] = StructOpsImpl.readField[$ty]($p, $fname)"""
    }
    val setters = fields.map { field =>
      val (fname, ty) = field
      val setter = TermName(fname + "_$eq")
      q"""def $setter(v: Rep[$ty]): Unit = StructOpsImpl.writeField[$ty]($p, $fname, v)"""
    }
    val res = c.Expr(q"""
      implicit class $opsClassName($p: Rep[$name]) {
        ..$getters
        ..$setters
      }
    """)
    res
  }
}

class CStruct extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CStruct_Impl.impl
}

class CStructFields {
  def genStructFields(name: String, fields: List[(String, String)]) =
    macro CStruct_Fields.impl
}

trait CCodeGenStruct extends ExtendedCCodeGen {
  override def traverse(n: Node): Unit = n match {
    case n @ Node(s, "reffield_set", List(ptr, Const(field: String), v), _) =>
      shallowP(ptr, precedence("reffield_get"))
      esln"->$field = $v;"
    case n @ Node(s, "local_struct", Nil, _) =>
      val tpe = remap(typeMap.getOrElse(s, manifest[Unknown]))
      emitln(s"$tpe ${quote(s)} = { 0 };") // FIXME: uninitialized? or add it as argument?
    case _ => super.traverse(n)
  }

  override def shallow(n: Node): Unit = n match {
    case n @ Node(s, "ref_new", List(v), _) =>
      emit("&"); shallowP(v, precedence("ref_new"))
    case n @ Node(s, "reffield_get", List(ptr, Const(field: String)), _) =>
      shallowP(ptr, precedence("reffield_get")); emit("->"); emit(field)
    case n @ Node(s, "deref", List(ptr), _) =>
      es"*$ptr"
    case _ => super.shallow(n)
  }
}
