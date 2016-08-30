package eu.stratosphere.emma
package compiler.lang.holopt

import api._
import compiler.ir.ComprehensionSyntax._
import compiler.BaseCompilerSpec
import testschema.University
import testschema.University.{Student,Attends,Course}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @see [Google Slides](https://goo.gl/Tt1eHg)
 */
@RunWith(classOf[JUnitRunner])
class ExampleASpec extends BaseCompilerSpec {

  import compiler._
  import universe._

  val baseInputPath = "file:///tmp/testschema/students/"
  val baseOutputPath = "file:///tmp/testschema/output/"

  //@formatter:off
  implicit val studentConverter = materializeCSVConverters[Student]
  implicit val attendsConverter = materializeCSVConverters[Attends]
  implicit val courseConverter  = materializeCSVConverters[Course]
  implicit val resultonverter    = materializeCSVConverters[(Int, String, Int)]
  //@formatter:on

  // ---------------------------------------------------------------------------
  // Transformation pipelines
  // ---------------------------------------------------------------------------

  val lnfPipeline: Expr[Any] => Tree =
    compiler.pipeline(typeCheck = true)(
      Core.lnf
    ).compose(_.tree)

  val normalizePipeline: Expr[Any] => Tree =
    compiler.pipeline(typeCheck = true)(
      Core.lift
    ).compose(_.tree)

  // ---------------------------------------------------------------------------
  // Example Code
  // ---------------------------------------------------------------------------

  val exampleSrc = normalizePipeline(reify {
    // Compute all (student, attends, course) triples
    val studentsAttendingCourses = for {
      s <- University.students // read(s"$baseInputPath/students.csv", new CSVInputFormat[Student])
      c <- University.courses // read(s"$baseInputPath/attends.csv", new CSVInputFormat[Course])
      a <- University.attends // read(s"$baseInputPath/courses.csv", new CSVInputFormat[Attends])
      if a.sid == s.id
      if a.cid == c.id
    } yield (s, a, c)

    // Compute all student pairs (s1, s2) which have attended the same course, such that:
    //   1) s1's faculty IS NOT the same as the one of the attended course
    //   2) s2's faculty IS     the same as the one of the attended course
    //   3) s1 as scored MORE points in this course than s2
    val pairs = for {
      (s1, a1, c1) <- studentsAttendingCourses
      (s2, a2, c2) <- studentsAttendingCourses
      if c1.id == c2.id
      if s1.faculty != c1.faculty
      if s2.faculty == c2.faculty
      if a1.points > a2.points
    } yield (s1.id, s1.name.last, s2.id)

    // return the result
    write("file://", new CSVOutputFormat[(Int, String, Int)])(pairs)
  })

  val exampleCore = lnfPipeline(reify {
    val students$1 = University.students
    val courses$1 = University.courses
    val attends$1 = University.attends

    // Compute all (student, attends, course) triples
    val studentsAttendingCourses = comprehension[(Student, Attends, Course), DataBag] {
      val s = generator[Student, DataBag] {
        students$1
      }
      val c = generator[Course, DataBag] {
        courses$1
      }
      val a = generator[Attends, DataBag]  {
        attends$1
      }
      guard {
        val f$01 = a.sid
        val f$02 = s.id
        val x$01 = f$01 == f$02
        x$01
      }
      guard {
        val f$03 = a.cid
        val f$04 = c.id
        val x$02 = f$03 == f$04
        x$02
      }
      head {
        val x$03 = (s, a, c)
        x$03
      }
    }

    // Compute all student pairs (s1, s2) which have attended the same course, such that:
    //   1) s1's faculty IS NOT the same as the one of the attended course
    //   2) s2's faculty IS     the same as the one of the attended course
    //   3) s1 as scored MORE points in this course than s2
    val pairs = comprehension[(Int, String, Int), DataBag] {
      val sac1 = generator[(Student, Attends, Course), DataBag] {
        studentsAttendingCourses
      }
      val sac2 = generator[(Student, Attends, Course), DataBag] {
        studentsAttendingCourses
      }
      guard {
        val f$05 = sac1._3
        val f$06 = sac2._3
        val f$07 = f$05.id
        val f$08 = f$06.id
        val x$04 = f$07 == f$08
        x$04
      }
      guard {
        val f$09 = sac1._1
        val f$10 = sac1._3
        val f$11 = f$09.faculty
        val f$12 = f$10.faculty
        val x$05 = f$11 != f$12
        x$05
      }
      guard {
        val f$14 = sac2._1
        val f$15 = sac2._3
        val f$16 = f$14.faculty
        val f$17 = f$15.faculty
        val x$06 = f$16 == f$17
        x$06
      }
      guard {
        val f$18 = sac1._2
        val f$19 = sac2._2
        val f$20 = f$18.points
        val f$21 = f$19.points
        val x$07 = f$20 > f$21
        x$07
      }
      head {
        val f$22 = sac1._1
        val f$23 = sac2._1
        val f$24 = f$22.id
        val f$25 = f$22.name
        val f$26 = f$25.last
        val f$27 = f$23.id
        val x$08 = (f$24, f$26, f$27)
        x$08
      }
    }

    // return the result
    val resultConverter$1 = this.resultonverter
    val CSVOutputFormat$1 = new CSVOutputFormat()(resultConverter$1)
    val x$09 = write("file://", CSVOutputFormat$1)(pairs)
    x$09
  })

  "Expressions should be alpha-equal" in {
    exampleSrc shouldBe alphaEqTo (exampleCore)
  }

  "WIP" in {

    import Core.{Lang => core}

    val cs = new Comprehension.Syntax(API.bagSymbol)

    val bagTermsBuilder = Map.newBuilder[u.TermSymbol, ValDef]
    val inpElemsBuilder = Set.newBuilder[(u.TermSymbol, u.TermSymbol)]
    val outElemsBuilder = Set.newBuilder[(u.TermSymbol, u.TermSymbol)]
    val inpEdgesBuilder = Set.newBuilder[(u.TermSymbol, u.TermSymbol)]

    api.BottomUp.traverse {
      case vd@core.ValDef(outBag, rhs, flags) if outBag.tpe.typeConstructor =:= API.DATA_BAG =>
        // add bag term entry
        bagTermsBuilder += outBag -> vd
        rhs match {
          case cs.Comprehension(qs, cs.Head(core.Let(_, _, core.Ref(outElm)))) =>
            // add output element entry
            outElemsBuilder += outBag -> outElm
            // add input element entry
            for (cs.Generator(inpElm, core.Let(_, _, core.Ref(inpBag))) <- qs) {
              inpElemsBuilder += inpBag -> inpElm
              inpEdgesBuilder += outBag -> inpElm
            }
          case _ =>
            Unit
        }
    }(exampleCore)

    val bagTerms = bagTermsBuilder.result()
    val inpElems = inpElemsBuilder.result().groupBy { case (k, v) => k } mapValues (_.map { case (k, v) => v })
    val outElems = outElemsBuilder.result().groupBy { case (k, v) => k } mapValues (_.map { case (k, v) => v })
    val bttEdges = inpEdgesBuilder.result().groupBy { case (k, v) => k } mapValues (_.map { case (k, v) => v })
    val res = (bagTerms, inpElems, outElems)
  }
}
