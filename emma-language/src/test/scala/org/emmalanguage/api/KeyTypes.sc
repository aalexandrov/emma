import scala.reflect.runtime.universe._

//@formatter:off

// -----------------------------------------------------------------------------
// A family of traits denoting N-ary products
// -----------------------------------------------------------------------------

trait Product1[_T1] {
  type T1 = _T1
}

trait Product2[_T1, _T2] {
  type T1 = _T1
  type T2 = _T2
}

trait Product3[_T1, _T2, _T3] {
  type T1 = _T1
  type T2 = _T2
  type T3 = _T3
}

// -----------------------------------------------------------------------------
// A type-class for object identity
// -----------------------------------------------------------------------------

trait Key[T] {
  type K
  def apply(x: T): K

// -----------------------------------------------------------------------------
// Example 1: Concrete product types
// -----------------------------------------------------------------------------

case class Student (
  id    :  Student#T1,
  first :  Student#T2,
  last  :  Student#T3
) extends Product3[Int, String, String]

object Student {
  implicit object PK extends Key[Student] {
    override type K = Product3[_, _, _]#T1
    override def apply(x: Student): K = x.id
  }
}

case class Course (
  id: Course#T1, last: Course#T2
) extends Product2[Long, String]

object Course {
  implicit object PK extends Key[Course] {
    override type K = Product2[_, _]#T1
    override def apply(x: Course): K = x.id
  }
}

case class Attends (
  sid   :  Attends#T1,
  cid   :  Attends#T2,
  grade :  Attends#T3
) extends Product3[Int, Int, Option[Short]] // Student.PK.K, Course.PK.K

object Attends {
  implicit object PK extends Key[Attends] {
    override type K = (Product3[_, _, _]#T1, Product3[_, _, _]#T2)
    override def apply(x: Attends): K = (x.sid, x.cid)
    //override def apply(x: Attends): K = (x.cid, x.sid) // typecheck error (1)
  }
}


// -----------------------------------------------------------------------------
// A concrete product type
// -----------------------------------------------------------------------------


val john = Student(1, "John", "Steed")
val emma = Student(2, "Emma", "Peel")


val pkJohn = implicitly[Key[Student]].apply(john)
val pkEmma = implicitly[Key[Student]].apply(emma)
