package base64

object Base64 {
  import java.nio.charset.Charset
  private val DefaultCharset = Charset.forName("utf8")

  val Alphabet  = ('A' to 'Z') ++ ('a' to 'z') ++ (0 to 9) ++ Seq('+', '/')

  def encode(str: String, charset: Charset = DefaultCharset) =
    str.getBytes(charset.name())
      .map(b => "%8s".format(Integer.toBinaryString(b)).replace(" ", "0"))
      .flatten.grouped(6)
      .map(_ match {
        case g if(g.size % 3 == 0) =>
          Alphabet(Integer.parseInt(String.valueOf(g), 2))
        case g if(g.size % 2 == 0) =>
          Alphabet(Integer.parseInt(String.valueOf(g), 2)) + "="
        case g =>
          Alphabet(Integer.parseInt(String.valueOf(g), 2)) + "=="
      }) mkString("")
}
