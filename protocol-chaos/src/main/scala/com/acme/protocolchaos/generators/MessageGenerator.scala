package com.acme.protocolchaos.generators

import com.google.protobuf.Descriptors.FieldDescriptor
import com.google.protobuf.{ByteString, Descriptors}
import com.trueaccord.scalapb.GeneratedMessageCompanion
import org.scalacheck.{Arbitrary, Gen}

object MessageGenerator {

  object hints {
    implicit def default[A]: Hint[A] = new Hint[A] {
      private val uriPattern = """.*_uri$""".r

      override def hint(fieldDescriptor: FieldDescriptor): Option[Gen[Any]] = {
        fieldDescriptor.getName.toLowerCase match {
          case "date" ⇒ Some(DateGenerators.randomDate)
          case "id" ⇒ Some(IdGenerator.id)
          case uriPattern() ⇒ Some(UriGenerators.uri)
          case _ ⇒ None
        }
      }
    }
  }

  trait Hint[+A] {
    def hint(fieldDescriptor: FieldDescriptor): Option[Gen[Any]]
  }

  private def noHint[A]: Hint[A] = new Hint[A] {
    override def hint(fieldDescriptor: FieldDescriptor): Option[Gen[Any]] = None
  }

  lazy val genByteString: Gen[ByteString] = {
    Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary).map(ByteString.copyFrom)
  }

  /**
    * Returns a generator for the ScalaPB-based message defined by its ``companion``. The companion is typically
    * generated during the ``protobuf:protobuf-generate`` sbt task, which also typically runs during the ``package``
    * task.
    *
    * @param companion the companion for the message type ``M``
    * @return generator for arbitrary messages of type ``M``
    */
  def message[A](companion: GeneratedMessageCompanion[_])(implicit hint: Hint[A] = noHint): Gen[A] = {
    import collection.JavaConverters._

    /**
      * Maps the list of ``FieldDescriptor``s into a list of pairs of the descriptor with the matching
      * ``Gen[_]`` for that field.
      * @param fields the field descriptors
      * @return the fd and matching generator
      */
    def generatorsFromFields(fields: List[FieldDescriptor]): List[(FieldDescriptor, Gen[Any])] = {
      fields.map { field ⇒
        import Descriptors.FieldDescriptor._

        val fieldGen: Gen[Any] = hint.hint(field).getOrElse(field.getType match {
          case Type.BOOL ⇒ Arbitrary.arbBool.arbitrary
          case Type.BYTES ⇒ genByteString
          case Type.DOUBLE ⇒ Arbitrary.arbDouble.arbitrary
          case Type.ENUM ⇒ Gen.oneOf(companion.enumCompanionForField(field).javaDescriptor.getValues.asScala)
          case Type.FIXED32 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.FIXED64 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.FLOAT ⇒ Arbitrary.arbFloat.arbitrary
          case Type.GROUP ⇒
            val cf = companion.messageCompanionForField(field)
            if (cf == companion) Gen.const(cf.defaultInstance) else message(cf)
          case Type.INT32 ⇒ Arbitrary.arbInt.arbitrary
          case Type.INT64 ⇒ Arbitrary.arbLong.arbitrary
          case Type.MESSAGE ⇒
            val cf = companion.messageCompanionForField(field)
            if (cf == companion) Gen.const(cf.defaultInstance) else message(cf)
          case Type.SFIXED32 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.SFIXED64 ⇒ Arbitrary.arbDouble.arbitrary
          case Type.SINT32 ⇒ Arbitrary.arbInt.arbitrary
          case Type.SINT64 ⇒ Arbitrary.arbLong.arbitrary
          case Type.STRING ⇒ Arbitrary.arbString.arbitrary
          case Type.UINT32 ⇒ Arbitrary.arbInt.arbitrary
          case Type.UINT64 ⇒ Arbitrary.arbLong.arbitrary
        })

        if (field.isRepeated) {
          (field, Gen.listOf(fieldGen))
        } else {
          (field, fieldGen)
        }
      }
    }

    def flatten(generators: List[(FieldDescriptor, Gen[Any])]): Gen[Map[FieldDescriptor, Any]] = {
      if (generators.isEmpty) Gen.const(Map.empty)
      else {
        val (ffd, ffg) = generators.head
        val firstFieldGenerator = ffg.map(x ⇒ Map(ffd → x))
        val remainingFieldGenerators = generators.tail

        remainingFieldGenerators.foldLeft(firstFieldGenerator) {
          case (result, (fd, fg)) ⇒ result.flatMap(m ⇒ fg.map(x ⇒ Map(fd → x) ++ m))
        }
      }
    }

    /**
      * Constructs a generator for instances generated from generated fields from the ``companion``.
      * @param companion the companion
      * @return the generator of messages constructed using the companion
      */
    def existentialMessage(companion: GeneratedMessageCompanion[_]): Gen[_] = {
      val oneOfFields = companion.javaDescriptor.getOneofs.asScala.toList
      val fields = companion.javaDescriptor.getFields.asScala.toList

      val plainFields = fields.filterNot(oneOfFields.flatMap(_.getFields.asScala).contains)
      // first, construct generators for plain fields: i.e. those that are not algebraic
      val plainGenerator = flatten(generatorsFromFields(plainFields))
      // next, construct generators for algebraic fields by constructing a generator that selects one field from each one-of group,
      // then constructing a generator for that field and folding the generators
      val oneOfGenerators = oneOfFields.map { oneOf ⇒
        Gen.oneOf(oneOf.getFields.asScala).flatMap { fd ⇒
          val (_, generator) = generatorsFromFields(List(fd)).head
          generator.map(x ⇒ Map(fd → x))
        }
      }

      // finally, combine the generators for the one-of fields with the plain fields
      val combinedGenerators = oneOfGenerators match {
        case Nil    ⇒ plainGenerator
        case (h::t) ⇒
          val combinedOneOfGenerator = t.foldLeft(h)((result, gen) ⇒ result.flatMap(m ⇒ gen.map(x ⇒ m ++ x)))
          combinedOneOfGenerator.flatMap(o ⇒ plainGenerator.map(x ⇒ o ++ x))
      }

      // use the companion to construct an instance from the field map
      combinedGenerators.map(companion.fromFieldsMap)
    }

    // the innards of ScalaPB do not allow to express properly compile-time type checking: it would be only
    // possible to check the top-level type, but possible inner messages would be left "untyped", so for the
    // sake of commonality, we use ``existentialMessage(...).map(_.asInstanceOf[M])`` here.
    existentialMessage(companion).asInstanceOf[Gen[A]]
  }

}
