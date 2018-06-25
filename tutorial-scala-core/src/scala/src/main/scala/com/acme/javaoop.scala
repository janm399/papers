package com.acme

import java.util.{Calendar, Date}

/**
  * Scala can be used in a very Java-like way; where classes have fields, getters and setters,
  * and methods.
  *
  * Here, the fields `dob`, `id`, and `name` have getters and setters (that follow the Scala
  * naming; use `@BeanProperty` annotation to generate Java-like `get` and `set` methods).
  *
  * The fields have to be initialized to a value, or allowed to take on default value expressed
  * here as `_`.
  */
class User {
  var dob: Date = _
  var id: Long = _
  var name: String = _

  /**
    * A method looks like Java method with parameters and return type
    *
    * @param asOf the `asOf` date which defaults to _now_
    * @return `true` if `asOf` is the day of this instance's birthday
    */
  def hasBirthday(asOf: Date = new Date()): Boolean = {
    if (dob != null) {
      val c1 = Calendar.getInstance()
      c1.setTime(dob)
      val dobMonth = c1.get(Calendar.MONTH)
      val dobDay = c1.get(Calendar.DAY_OF_MONTH)

      val c2 = Calendar.getInstance()
      c2.setTime(asOf)
      if (c2.get(Calendar.MONTH) == dobMonth) {
        c2.get(Calendar.DAY_OF_MONTH) == dobDay
      } else false
    } else false
  }

}

object UserMain extends App {
  val user = new User
  user.dob = new Date()
  user.name = "Jan"
  user.id = 100

  println(user)

}
