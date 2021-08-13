import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit val stringByteEncoder: ByteEncoder[String] = instance[String](_.getBytes)

  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = (a: A) => f(a)
}

trait ByteDecoder[A] {
  def decode(arr: Array[Byte]): Option[A]
}

object ByteDecoder {
  implicit val stringByteDecoder: ByteDecoder[String] = instance[String](arr => Some(arr.map(_.toChar).mkString))

  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev

  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(arr: Array[Byte]) = f(arr)
  }
}

implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = new ByteEncoder[Option[A]] {
  override def encode(a: Option[A]): Array[Byte] = a.map(x => encA.encode(x)).toList.flatten.toArray
}

implicit class ByteEncoderOps[A](a: A){
  def encode(implicit encoder: ByteEncoder[A]): Array[Byte] =
    encoder.encode(a)
}

/*
implicit object StringReverseByteDecoder extends ByteDecoder[String] {
  override def decode(arr: Array[Byte]): Option[String] = Some(arr.reverse.map(_.toChar).mkString)
}
*/
val example = Array(98,105,101,110,32,58,41).map(_.toByte)

"hola".encode

val s1 = ByteDecoder[String].decode(example)
val a = ByteEncoder[String].encode("hola")
val b = ByteDecoder[String].decode(a)

val c = Some("hola") == b

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit

  def read[A]()(implicit dec: ByteDecoder[A]): Option[A]
}

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("fp-course/test")) {os =>
      os.write(bytes)
      os.flush()
    }
  }

  override def read[A]()(implicit dec: ByteDecoder[A]): Option[A] = ???
}

trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]

def isomorphism[A](a: A)(implicit codec: ByteCodec[A]): Boolean = codec.decode(codec.encode(a)).contains(a)

