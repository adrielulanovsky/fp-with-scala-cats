import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

object Tests {
  trait ByteEncoder[A] {
    def encode(a: A): Array[Byte]
  }

  trait ByteDecoder[A] {
    def decode(arr: Array[Byte]): Option[A]
  }

  trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]

  trait ByteCodecLaws[A] {
    def codec: ByteCodec[A]
    def isomorphism(a: A): Boolean = codec.decode(codec.encode(a)).contains(a)
  }

  trait ByteCodecTests[A] extends Laws {
    def laws: ByteCodecLaws[A]

    def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
      name = "byteCodec",
      parent = None,
      "isomorphism" -> forAll(laws.isomorphism _)
    )
  }

  object StringByteCodec extends ByteCodec[String] {
    override def encode(s: String): Array[Byte] = s.getBytes

    override def decode(arr: Array[Byte]): Option[String] = Some(arr.map(_.toChar).mkString)
  }

  object StringByteCodecLaws extends ByteCodecLaws[String] {
    override def codec: ByteCodec[String] = StringByteCodec
  }

  object StringByteCodecTests extends ByteCodecTests[String] {
    override def laws: ByteCodecLaws[String] = StringByteCodecLaws
  }

}

import Tests._

class ByteCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  checkAll("ByteCodec[String]", StringByteCodecTests.byteCodec)
}
