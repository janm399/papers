package com.acme

import scala.util.Random

/**
  * Defines the case class which can be thought of as a simple "struct" for the three
  * properties, but with getters, `equals`, `hashCode`, and `toString`.
  *
  * @param firstName the FN property
  * @param lastName the LN property
  * @param age the age property
  */
case class Person(firstName: String, lastName: String, age: Int)

object PersonMain extends App {

  def randomPerson(): Person =
    Person(Random.nextString(10), Random.nextString(10), Random.nextInt(100))

  val people = List.fill(100)(randomPerson())

  // youngest; don't worry about efficiency, this is just to demonstrate the pattern
  val youngestAge = people.map(_.age).min
  val Some(youngestPerson) = people.find(_.age == youngestAge)

  // youngest 2; a bit more efficient by ordering the list by providing a comparison
  // function.
  val orderedPeople = people.sortWith(_.age < _.age)
  val youngestPerson2 = orderedPeople.head

  // youngest 3: since `Int`s already naturally compare it is possible to order the list
  // by projection to the age property
  val orderedPeople2 = people.sortBy(_.age)
  val youngestPerson3 = orderedPeople.head

  println(youngestPerson2)
  println(youngestPerson3)
}
