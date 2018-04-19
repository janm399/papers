package com.acme.cam

import java.util.concurrent.locks.{Lock, ReentrantLock}

object ThreadingMain {

  implicit class RichLock(lock: Lock) {
    def locked[A](body: ⇒ A): A = {
      lock.lock()
      val a = body
      lock.unlock()
      a
    }
  }

  def main(args: Array[String]): Unit = {
    var state: Long = 0
    val lock = new ReentrantLock()

    def oneStepForward(): Unit = lock.locked(state += Int.MaxValue * 2L)
    def oneStepBack(): Unit = lock.locked(state -= Int.MaxValue * 2L)

    while (true) {
      val t1 = new Thread(() ⇒ oneStepForward())
      val t2 = new Thread(() ⇒ oneStepBack())
      t1.start()
      t2.start()

      lock.locked {
        if (state != 0) println(state)
      }

      Thread.sleep(10)
    }
  }

}
