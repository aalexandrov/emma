package eu.stratosphere.emma
package compiler.lang.holopt

import api._
import compiler.ir.ComprehensionSyntax._
import compiler.BaseCompilerSpec
import testschema.University
import testschema.University.{Student, Attends, Course}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @see [Google Slides](https://goo.gl/Tt1eHg)
 */
@RunWith(classOf[JUnitRunner])
class ExampleBSpec extends BaseCompilerSpec {

  import compiler._
  import universe._

  val baseInputPath = "file:///tmp/testschema/students/"
  val baseOutputPath = "file:///tmp/testschema/output/"

  //@formatter:off
  implicit val studentConverter = materializeCSVConverters[Student]
  implicit val attendsConverter = materializeCSVConverters[Attends]
  implicit val courseConverter  = materializeCSVConverters[Course]
  implicit val resultConverter  = materializeCSVConverters[(Int, String, Double)]
  //@formatter:on

  // ---------------------------------------------------------------------------
  // Transformation pipelines
  // ---------------------------------------------------------------------------

  val lnfPipeline: Expr[Any] => Tree =
    compiler.pipeline(typeCheck = true)(
      Core.lnf
    ).compose(_.tree)

  val liftPipeline: Expr[Any] => Tree =
    compiler.pipeline(typeCheck = true)(
      Core.lift
    ).compose(_.tree)

  // ---------------------------------------------------------------------------
  // Example Code
  // ---------------------------------------------------------------------------

  val exampleSrc = liftPipeline(reify {
    // Compute all (student, attends, course) triples
    val studentsAttendingCourses = for {
      s <- University.students // read(s"$baseInputPath/students.csv", new CSVInputFormat[Student])
      c <- University.courses // read(s"$baseInputPath/attends.csv", new CSVInputFormat[Course])
      a <- University.attends // read(s"$baseInputPath/courses.csv", new CSVInputFormat[Attends])
      if a.sid == s.id
      if a.cid == c.id
    } yield (s, a, c)

    val studentGrades = for {
      Group(s, cs) <- studentsAttendingCourses groupBy { case (s, _, _) => s }
    } yield {
      val sum = cs.map { case (_, a, _) => a.points }.sum
      val cnt = cs.size.toDouble
      (s.id, s.name.last, sum / cnt)
    }

    // return the result
    write("file://", new CSVOutputFormat[(Int, String, Double)])(studentGrades)
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
      val a = generator[Attends, DataBag] {
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

    val anonfun$6 = (x0$1: (Student, Attends, Course)) => {
      val x$9 = x0$1._1
      x$9
    }

    val groupBy$1 = studentsAttendingCourses groupBy anonfun$6

    val studentGrades = comprehension[(Int, String, Double), DataBag] {
      val check$ifrefutable$1 = generator[Group[Student, DataBag[(Student, Attends, Course)]], DataBag] {
        groupBy$1
      }
      head {
        val s = check$ifrefutable$1.key
        val cs$1 = check$ifrefutable$1.values
        val map$2 = comprehension[Int, DataBag] {
          val x0$2 = generator[(Student, Attends, Course), DataBag] {
            cs$1
          }
          head {
            val a = x0$2._2
            val x$10 = a.points
            x$10
          }
        }
        val zero$1 = Numeric.IntIsIntegral.zero
        val anonfun$9 = (x: Int) => {
          val x$11 = Predef identity x
          x$11
        }
        val anonfun$10 = (x$5: Int, y: Int) => {
          val x$12 = Numeric.IntIsIntegral.plus(x$5, y)
          x$12
        }
        val fold$1 = map$2.fold(zero$1)(anonfun$9, anonfun$10)
        val anonfun$11 = (x$3: (Student, Attends, Course)) => {
          1L
        }
        val anonfun$12 = (x$1: Long, x$2$1: Long) => {
          val x$13 = x$1 + x$2$1
          x$13
        }
        val fold$2 = cs$1.fold(0L)(anonfun$11, anonfun$12)
        val cnt = fold$2.toDouble
        val id$3 = s.id
        val name$1 = s.name
        val last$1 = name$1.last
        val `/$1` = fold$1 / cnt
        val x$14 = (id$3, last$1, `/$1`)
        x$14
      }
    }

    val resultConverter$1 = resultConverter
    val CSVOutputFormat$1 = new CSVOutputFormat()(resultConverter$1)
    val x$15 = write("file://", CSVOutputFormat$1)(studentGrades)
    x$15
  })

  "Expressions should be alpha-equal" in {
    exampleSrc shouldBe alphaEqTo(exampleCore)
  }
}
