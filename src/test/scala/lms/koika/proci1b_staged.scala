package lms.koika

import lms.core.stub._
import lms.core.virtualize
import lms.macros.SourceContext

@virtualize
class StagedProcInterp1bPC extends TutorialFunSuite {
  val under = "proci1b_staged_"

  val regfile_main = """
int main(int argc, char *argv[]) {
  int regfile[6] = {0, 0, 0, 0, 0, 0};
  Snippet(regfile);
  for (int i = 0; i < 6; i++) {
    printf("%d ", regfile[i]);
  }
  printf("\n");
  return 0;
}
"""
  override def exec(label: String, code: String, suffix: String = "c") =
    super.exec(label, code, suffix)

  override def check(label: String, code: String, suffix: String = "c") =
    super.check(label, code, suffix)

  val DEBUG = true

  trait Interp extends Dsl {

    abstract sealed class Instruction
    case class Add(rd: Reg, rs1: Reg, rs2: Reg) extends Instruction
    case class Addi(rd: Reg, rs1: Reg, imm: Int) extends Instruction
    case class BrNEZ(rs: Reg, imm: Int) extends Instruction


    type Program = List[Instruction]
    type RegFile = Rep[Array[Int]]
    type PC = Rep[Int]

    case class Reg(id: Int)
    val ZERO: Reg = Reg(0)
    val A0: Reg = Reg(1)
    val A1: Reg = Reg(2)
    val A2: Reg = Reg(3)
    val A3: Reg = Reg(4)
    val A4: Reg = Reg(5)
    val A5: Reg = Reg(6)

    val NOP = Addi(ZERO, ZERO, 0)

    implicit def reg2int(r: Reg): Int = r.id
    implicit def reg2rep(r: Reg): Rep[Int] = unit(r.id)

    type State = (RegFile, PC)

    def println(s: String) = if (DEBUG) Predef.println(s) else ()

    def run(prog: Program, state: (RegFile, PC)): RegFile = {
      val regfile: RegFile = state._1
      var pc: Var[Int] = __newVar(state._2)

      while (0 <= readVar(pc) && pc < prog.length) {
        for (i <- (0 until prog.length): Range) {
          if (i == pc) {
            prog(i) match {
              case Add(rd, rs1, rs2) =>
                regfile(rd) = regfile(rs1) + regfile(rs2)
                pc = pc + 1

              case Addi(rd, rs1, imm) =>
                regfile(rd) = regfile(rs1) + imm
                pc = pc + 1

              case BrNEZ(rs, target) =>
                if (regfile(rs) != 0) pc = pc + target
                else pc = pc + 1
            }
          }
        }
      }
      regfile
    }
  }

  abstract class DslDriverX[A: Manifest, B: Manifest] extends DslDriverC[A, B] {
    q =>
    val main: String = ""

    override val codegen = new DslGenC {
      val IR: q.type = q

      override def emitAll(
          g: lms.core.Graph,
          name: String
      )(m1: Manifest[_], m2: Manifest[_]): Unit = {
        val ng = init(g)
        val efs = "" // quoteEff(g.block.ein)
        val stt = dce.statics.toList.map(quoteStatic).mkString(", ")
        prepareHeaders
        emitln("""
        |/*****************************************
        |Emitting C Generated Code
        |*******************************************/
        """.stripMargin)
        val src = run(name, ng)
        emitDefines(stream)
        emitHeaders(stream)
        emitFunctionDecls(stream)
        emitDatastructures(stream)
        emitFunctions(stream)
        emitInit(stream)
        emitln(s"\n/**************** $name ****************/")
        emit(src)
        emitln("""
        |/*****************************************
        |End of C Generated Code
        |*******************************************/
        |""".stripMargin)
        emit(main)
      }
    }
  }

  test("proc 1") {
    val snippet = new DslDriverX[Array[Int], Int] with Interp {
      override val main = """
int main(int argc, char *argv[]) {
  int regfile[6] = {0, 0, 1, 0, 15, -1};
  int r = Snippet(regfile);
  printf("%d\n", r);
  return 0;
}
"""
      def snippet(initRegFile: RegFile) = {
        val N = A3
        val Temp = A2
        val F_n = A1
        val F_n_1 = A0
        val MinusOne = A4

        val Fibprog = List(
          Add(Temp, F_n, F_n_1),
          Add(F_n_1, F_n, ZERO),
          Add(F_n, Temp, ZERO),
          Add(N, N, MinusOne),
          BrNEZ(N, -4)
        )

        val res = run(Fibprog, (initRegFile, 0))
        val expected = Array(0, 610, 987, 987, 0, -1)

        res(F_n)
      }
    }
    check("1", snippet.code)
  }

  test("proc 2") {
    val snippet = new DslDriverX[Array[Int], Array[Int]] with Interp {
      override val main = regfile_main
      def snippet(initRegFile: RegFile) = {

        val N = A3
        val Temp = A2
        val F_n = A1
        val F_n_1 = A0
        val Fibprog = List(
          Addi(F_n, ZERO, 1),
          Addi(F_n_1, ZERO, 0),
          Addi(N, ZERO, 15),
          Addi(Temp, ZERO, 0),
          Add(Temp, F_n, F_n_1),
          Add(F_n_1, F_n, ZERO),
          Add(F_n, Temp, ZERO),
          Addi(N, N, -1),
          BrNEZ(N, -4)
        )

        run(Fibprog, (initRegFile, 0))
      }
    }
    check("2", snippet.code)
  }
}
