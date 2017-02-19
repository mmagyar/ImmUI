package mmagyar.parsing

import java.io.File
import java.nio.{ByteBuffer, ByteOrder, IntBuffer}

import mmagyar.parsing.PcfFormat.PcfFormatModifiers

import scala.collection.BitSet
import scala.io.Source

/** Created by Magyar Máté on 2017-02-18, All rights reserved. */
object PcfType {
  case object PCF_PROPERTIES       extends PcfType { val value: Int = 1 << 0 }
  case object PCF_ACCELERATORS     extends PcfType { val value: Int = 1 << 1 }
  case object PCF_METRICS          extends PcfType { val value: Int = 1 << 2 }
  case object PCF_BITMAPS          extends PcfType { val value: Int = 1 << 3 }
  case object PCF_INK_METRICS      extends PcfType { val value: Int = 1 << 4 }
  case object PCF_BDF_ENCODINGS    extends PcfType { val value: Int = 1 << 5 }
  case object PCF_SWIDTHS          extends PcfType { val value: Int = 1 << 6 }
  case object PCF_GLYPH_NAMES      extends PcfType { val value: Int = 1 << 7 }
  case object PCF_BDF_ACCELERATORS extends PcfType { val value: Int = 1 << 8 }
  def apply(int: Int): PcfType = int match {
    case PCF_PROPERTIES.value       => PCF_PROPERTIES
    case PCF_ACCELERATORS.value     => PCF_ACCELERATORS
    case PCF_METRICS.value          => PCF_METRICS
    case PCF_BITMAPS.value          => PCF_BITMAPS
    case PCF_INK_METRICS.value      => PCF_INK_METRICS
    case PCF_BDF_ENCODINGS.value    => PCF_BDF_ENCODINGS
    case PCF_SWIDTHS.value          => PCF_SWIDTHS
    case PCF_GLYPH_NAMES.value      => PCF_GLYPH_NAMES
    case PCF_BDF_ACCELERATORS.value => PCF_BDF_ACCELERATORS
  }
}
sealed trait PcfType

object PcfFormat {
  def checkBit(value: Int, x: Int): Boolean = (value & (1L << x)) != 0

  /**
    *
#define PCF_GLYPH_PAD_MASK	(3<<0)		/* See the bitmap table for explanation */
#define PCF_BYTE_MASK		(1<<2)		/* If set then Most Sig Byte First */
#define PCF_BIT_MASK		(1<<3)		/* If set then Most Sig Bit First */
#define PCF_SCAN_UNIT_MASK	(3<<4)		/* See the bitmap table for explanation */
    */
  object PcfFormatModifiers {
    def apply(value: Int): List[PcfFormatModifiers] = {
      (if (checkBit(value, 0) && checkBit(value, 1)) List(PCF_GLYPH_PAD_MASK) else List()) ++
        (if (checkBit(value, 2)) List(PCF_GLYPH_PAD_MASK) else List()) ++
        (if (checkBit(value, 3)) List(PCF_BIT_MASK) else List()) ++
        (if (checkBit(value, 4) && checkBit(value, 5)) List(PCF_SCAN_UNIT_MASK_MASK) else List())
    }
  }
  sealed trait PcfFormatModifiers

  case object PCF_GLYPH_PAD_MASK      extends PcfFormatModifiers
  case object PCF_BYTE_MASK           extends PcfFormatModifiers
  case object PCF_BIT_MASK            extends PcfFormatModifiers
  case object PCF_SCAN_UNIT_MASK_MASK extends PcfFormatModifiers

  val value = BitSet(3 << 0)
//#define PCF_DEFAULT_FORMAT	0x00000000
//#define PCF_INKBOUNDS		0x00000200
//#define PCF_ACCEL_W_INKBOUNDS	0x00000100
//#define PCF_COMPRESSED_METRICS	0x00000100
  case class PCF_DEFAULT_FORMAT(modifiers: List[PcfFormatModifiers])     extends PcfFormat
  case class PCF_INKBOUNDS(modifiers: List[PcfFormatModifiers])          extends PcfFormat
  case class PCF_ACCEL_W_INKBOUNDS(modifiers: List[PcfFormatModifiers])  extends PcfFormat
  case class PCF_COMPRESSED_METRICS(modifiers: List[PcfFormatModifiers]) extends PcfFormat

  def apply(int: Int): PcfFormat = {
    val modifiers = PcfFormatModifiers(int)
    int match {
      case a if checkBit(a, 8) => PCF_ACCEL_W_INKBOUNDS(modifiers)
      case a if checkBit(a, 9) => PCF_INKBOUNDS(modifiers)
      case _                   => PCF_DEFAULT_FORMAT(modifiers)
    }
  }
}
sealed trait PcfFormat { def modifiers: List[PcfFormatModifiers] }
object Pcf {

  def main(args: Array[String]): Unit = {

//   Source.
    import java.nio.file.{Files, Paths}

    val path = new File(".").getAbsolutePath
    //      println(path)
    val byteArray = Files.readAllBytes(Paths.get("gohufont-11.pcf"))

    val intBuf =
      ByteBuffer
        .wrap(byteArray)
        .order(ByteOrder.LITTLE_ENDIAN)
        .asIntBuffer()
    val pcf = new Array[Int](intBuf.remaining())
    intBuf.get(pcf)

    val bytes = String.valueOf(ByteBuffer.allocate(4).putInt(pcf(0)).array().map(x => x.toChar))

    val headerValidStart = "\1fcp" == bytes

    case class Header(pcfType: PcfType, format: PcfFormat, size: Int, offset: Int)
    var headerNum = pcf(1)
    var i         = 0

    var headers = List[Header]()
    while (i < headerNum) {

      val offset = (i * 4) + 2

      val header = Header(PcfType(pcf(offset)), PcfFormat(pcf(1 + offset)), pcf(offset + 2), pcf(offset + 3))
      headers = header :: headers
      i += 1
    }

//    val header = (pcf(1), pcf(2), pcf(3), pcf(4))

    println(headerValidStart)
    headers.reverse.foreach(println)
//        println(intBuf.size *4)

  }
}
class Pcf {}
