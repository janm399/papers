package com.acme

import java.util.{Calendar, Date}

class User {
  var dob: Date = _
  var id: Long = _
  var name: String = _

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