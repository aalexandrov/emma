package eu.stratosphere.emma
package testschema

import api._
import api.model._

/** A simple domain for a university database. */
object University {

  // --------------------------------------------------------------------------
  // Schema
  // --------------------------------------------------------------------------

  //@formatter:off
  case class Name(
    first        : String,
    last         : String
  )

  case class Student(
    @id id       : Int,
    gender       : Symbol,
    faculty      : String,
    name         : Name
  )

  case class Course(
    @id id       : Int,
    title        : String,
    credits      : Short,
    faculty      : String,
    description  : String
  )

  case class Attends(
    @id sid      : Int,
    @id cid      : Int,
    points       : Int,
    term         : Symbol
  )
  //@formatter:on

  // --------------------------------------------------------------------------
  // Sample Data
  // --------------------------------------------------------------------------

  val students = DataBag(Vector(
    Student(1, 'm, "CS"        , Name("John", "Doe")),
    Student(2, 'f, "Sociology" , Name("Jane", "Doe")),
    Student(3, 'm, "Literature", Name("Max", "Mustermann"))))

  val courses = DataBag(Vector(
    Course(1, "CS 101",           9, "CS"         , "Long description..." ),
    Course(2, "DB 101",           9, "CS"         , "Long description..." ),
    Course(3, "Linear Algebra 1", 6, "Mathematics", "Long description..." ),
    Course(4, "Linear Algebra 2", 6, "Mathematics", "Long description..." )))

  val attends = DataBag(Seq(
    Attends(students.fetch()(0).id, courses.fetch()(0).id, 95, Symbol("2015 Spring")),
    Attends(students.fetch()(2).id, courses.fetch()(1).id, 95, Symbol("2015 Spring")),
    Attends(students.fetch()(3).id, courses.fetch()(2).id, 95, Symbol("2015 Summer")),
    Attends(students.fetch()(1).id, courses.fetch()(0).id, 95, Symbol("2016 Spring")),
    Attends(students.fetch()(2).id, courses.fetch()(3).id, 95, Symbol("2016 Spring"))))

  def isComputerScience(c: Course): Boolean = true

  def isNamedJohn(s: Student): Boolean = true
}
