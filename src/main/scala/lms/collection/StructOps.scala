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
    // TODO: provenance
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
        val res = c.Expr(q"""
          case class $name(..$fields)
          implicit val $manifestName = new RefinedManifest[$name] {
            def fields: List[(String, Manifest[_])] = List(..$fieldDecls)
            def runtimeClass = classOf[$name]
            override def name = Some(${name.toString})
          }
        """)
        res
    }
  }
}

object CStructFields {
  import scala.reflect.runtime.universe._

  def impl[T: c.WeakTypeTag](c: Context)(_unit: c.Expr[Unit]): c.Expr[Any] = {
    import c.universe._

    val symbol = weakTypeOf[T].typeSymbol
    val name = symbol.name
    val opsClassName = internal.reificationSupport.freshTypeName(name.toString+"Ops")

    val getters =
      weakTypeOf[T].members.collect {
        case m: MethodSymbol if m.isCaseAccessor => {
          q"""
          def ${m.name}: Rep[${m.returnType}] =
            StructOpsImpl.readField[$symbol, ${m.returnType}](p, ${m.name.toString})
          """
        }
      }.toList

    val setters =
      weakTypeOf[T].members.collect {
        case m: MethodSymbol if m.isCaseAccessor => {
          val setterName = TermName(m.name + "_$eq")
          val setter = q"""
            def $setterName(v: Rep[${m.returnType}]): Unit =
              StructOpsImpl.writeField[$symbol, ${m.returnType}](p, ${m.name.toString}, v)
            """
          setter
        }
      }.toList

    val res = c.Expr(q"""
      implicit class $opsClassName(p: Rep[$symbol]) {
        ..$getters
        ..$setters
      }
    """)
    res
  }

  def fields[T](_unit: Unit): Any = macro impl[T]
}

class CStruct extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CStruct_Impl.impl
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
