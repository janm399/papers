package com.acme.protocolchaos.chaos.extractors

import com.google.common.reflect.ClassPath
import com.trueaccord.scalapb.GeneratedMessageCompanion

object MessageExtractor {
  private val messagesPackage = "com.acme"

  lazy val messages: Seq[GeneratedMessageCompanion[_]] = {
    import scala.collection.JavaConverters._
    val tlci = ClassPath.from(this.getClass.getClassLoader).getTopLevelClassesRecursive(messagesPackage).asScala
    import scala.reflect.runtime.universe._

    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val gmcType = mirror.typeOf[GeneratedMessageCompanion[_]]

    def allCompanionsOfType(root: ModuleSymbol, requiredType: Type): Seq[ModuleSymbol] = {
      val modules = root :: root.info.members.toList.flatMap { member ⇒
        if (member.isModule) allCompanionsOfType(member.asModule, requiredType) else Nil
      }

      modules.filter(_.info <:< requiredType)
    }

    def generatedMessageCompanion(ci: ClassPath.ClassInfo): Seq[GeneratedMessageCompanion[_ <: AnyRef]] = {
      val module = mirror.staticModule(ci.getName)
      allCompanionsOfType(module, gmcType)
        .map(module ⇒ mirror.reflectModule(module).instance.asInstanceOf[GeneratedMessageCompanion[_ <: AnyRef]])
    }

    tlci.flatMap(generatedMessageCompanion).toSeq
  }

}
