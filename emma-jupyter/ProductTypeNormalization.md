# Type Normalization

In order to streamline subsequent optimizations, as a pre-processing step we perform *product type normalization* -- a transformation that simplifies the element type of `DataBag` terms.

## Benefits

The results of the transformation are twofold.
First, it eliminate user-defined product types (i.e, case classes) and substitutes them with isomorphic tuple types.
For example, instead of

```scala
case class Student(id: Int, name: String, surname: String)

for {
  s <- students // DataBag[Student]
} yield s.name
```

we will have the following code.

```scala
for {
  s <- students // DataBag[(Int, String, String)]
} yield s._1
```

Second, it flattens the product type structure. For example, instead of

```scala
case class Name(first: String, last: String)
case class Student(id: Int, name: Nanme)

for {
  s <- students // DataBag[Student]
} yield s.name

```

we will have the following code.

```scala
for {
  s <- students // DataBag[(Int, String, String)]
} yield s._1
```

## Corner Cases

Corner cases when applying the transformation arise in the following situations.

**Opaque type use**

When a case class is used as target or argument of a black-box method.
For example

```scala
case class Name(first: String, last: String)
case class Student(id: Int, name: Nanme)

def isMale(n: Name): Boolean = ??? // opaque function (rhs not known)

for {
  s <- students // DataBag[Student]
  if isMale(s.name)
} yield s.name
```
constrains the specification only to

```scala
for {
  s <- students // DataBag[(Int, Name)]
  if isMale(s._2)
} yield s._1
```

**Dataflow (Multiple Consumers)**

**Control Flow**

## Running Example

Schema.

```scala
case class Student(id: Int, name: String)
case class Course(id: Int, name: String, description: String)
case class Attends(sid: Int, cid: Int, points: Int)
```

Original quoted expression.

```scala
val studentCourses = for {
  s <- students
  c <- courses
  a <- attends
  if a.sid == s.id && a.cid == c.id
} yield (s, a, c)

val studentGrades = for {
  Group(s, cs) <- studentCourses groupBy { case (s, _, _) => s }
} yield {
  val sum = cs.map{ case(_, a, _) => a.points}.sum
  val cnt = cs.size.toDouble
  (s.name, sum / cnt)
}

studentGrades
```

Desugared and normalized expression.

```scala
{
  val students$1 = University.students
  val courses$1 = University.courses
  val attends$1 = University.attends
  val studentCourses = for {
    s$1 <- {
      students$1
    }
    c <- {
      courses$1
    }
    a$1 <- {
      attends$1
    }
    if {
      val sid$1 = a$1.sid
      val id$1 = s$1.id
      val `==$1` = sid$1 == id$1
      val cid$1 = a$1.cid
      val id$2 = c.id
      val `==$2` = cid$1 == id$2
      `==$1` && `==$2`
    }
  } yield {
    (s$1, a$1, c)
  }
  val $anonfun$5 = (x0$1: (Student, Attends, Course)) => {
    x0$1._1
  }
  val groupBy$1 = studentCourses groupBy $anonfun$5
  for {
    check$ifrefutable$1 <- {
      groupBy$1
    }
  } yield {
    val s$3 = check$ifrefutable$1.key
    val cs$1 = check$ifrefutable$1.values
    val map$2 = for {
      x0$2 <- {
        cs$1
      }
    } yield {
      val a$2 = x0$2._2
      a$2.points
    }
    val zero$1 = IntIsIntegral.zero
    val $anonfun$8 = (x$5: Int) => {
      Predef identity x$5
    }
    val $anonfun$9 = (x$6: Int, y: Int) => {
      IntIsIntegral.plus(x$6, y)
    }
    val fold$1 = map$2.fold(zero$1)($anonfun$8, $anonfun$9)
    val $anonfun$10 = (x$2$1: (Student, Attends, Course)) => {
      1L
    }
    val $anonfun$11 = (x$1$1: Long, x$3$2: Long) => {
      x$1$1 + x$3$2
    }
    val fold$2 = cs$1.fold(0L)($anonfun$10, $anonfun$11)
    val cnt = fold$2.toDouble
    val name$1 = s$3.name
    val `/$1` = fold$1 / cnt
    (name$1, `/$1`)
  }
}
```

## Approach

Ideally, we would define the transformation on all possible `DatBag` expressions that can occur in an Emma Core expression and perform it as a bottom-up or top-down transformation.
This approach, however, might not yield consistent results.
Consider the following example:

```scala
case class Name(first: String, last: String)
case class Student(id: Int, name: Nanme)

def isMale(n: Name): Boolean = ??? // opaque function (rhs not known)

val xs = for {
  s <- students // DataBag[Student]
} yield s.name


```
constrains the specification only to

```scala
for {
  s <- students // DataBag[(Int, Name)]
  if isMale(s._2)
} yield s._1
```
