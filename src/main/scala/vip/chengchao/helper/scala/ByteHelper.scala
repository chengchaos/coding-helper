package vip.chengchao.helper.scala

import java.text.DecimalFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Objects
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  *
  */
object ByteHelper {


  /**
    * 一个空 byte 数组
    */
  val EMPTY_ARRAY: Array[Byte] = new Array[Byte](0)

  /**
    * 一个空字符串
    */
  val EMPTY_STRING: String = ""

  /**
    * 0x00
    */
  val BIT_0X00: Byte = 0x00.asInstanceOf[Byte]

  /**
    * 0x01
    */
  val BIT_0X01: Byte = 0x01.asInstanceOf[Byte]

  /**
    * 0xFF
    */
  val BIT_0XFF: Byte = 0xff.asInstanceOf[Byte]

  /**
    * [0xFF,0xFF]
    */
  val BYTES_0XFF_2: Array[Byte] = Array(BIT_0XFF, BIT_0XFF)


  /**
    * yyyy-MM-dd HH:mm:ss
    */
  val GENERAL_DATETIME_PATTERN: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  /**
    * 连接一些 byte 数组为一个大的 byte 数组
    *
    * @param arraySeq 那些 byte 数组组成的序列
    * @return
    */
  def concatBytes(arraySeq: Array[Byte]*): Array[Byte] = {

    @tailrec
    def copyArrays(dest: Array[Byte], index: Int, destPos: Int): Array[Byte] = {
      if (index < arraySeq.length) {
        val src: Array[Byte] = arraySeq(index)
        val length: Int = src.length
        System.arraycopy(src, 0, dest, destPos, length)
        copyArrays(dest, index + 1, destPos + length)
      } else {
        dest
      }
    }

    if (Objects.nonNull(arraySeq) && arraySeq.nonEmpty) {
      val length = arraySeq.map(_.length).sum
      val resultArrays: Array[Byte] = new Array[Byte](length)
      copyArrays(resultArrays, 0, 0)
    } else {
      EMPTY_ARRAY
    }
  }

  /**
    * 从他们的 java 的代码中复制过来稍作修改的
    *
    * @param localDateTime LocalDateTime
    * @return
    */
  def time2TboxTimeArray(localDateTime: LocalDateTime): Array[Byte] = {

    CodingHelper.csar[Array[Byte]](new Array[Byte](6)) {
      resultArrays => {
        resultArrays(0) = (localDateTime.getYear % 100).toByte
        resultArrays(1) = localDateTime.getMonthValue.toByte
        resultArrays(2) = localDateTime.getDayOfMonth.toByte
        resultArrays(3) = localDateTime.getHour.toByte
        resultArrays(4) = localDateTime.getMinute.toByte
        resultArrays(5) = localDateTime.getSecond.toByte
      }}

  }


  /**
    * 将一个 Byte 数组转成一个 Int 数据
    *
    * @param index0 Byte 0
    * @param index1 Byte 1
    * @param index2 Byte 2
    * @param index3 Byte 3
    * @return
    */
  def bytes2int(index0: Byte = 0x00, index1: Byte = 0x00, index2: Byte = 0x00, index3: Byte): Int = {

    val bytes: Array[Byte] = Array(index0, index1, index2, index3)
    val bytesLength: Int = bytes.length

    val indexedSeq: IndexedSeq[Int] = for (i <- 0 until bytesLength) yield {
      val shift:Int = (bytesLength - 1 - i) * 8
      (bytes(i) & 0x000000FF) << shift
    }

    indexedSeq.sum
  }


  /**
    * 将一个 Int 数据转成一个  Byte 数组
    *
    * @param input Int
    * @return
    */
  def int2bytes(input: Int): Array[Byte] = {

    val temp = if (input < 0) ~input else input
    val byteNum = (40 - Integer.numberOfLeadingZeros(temp)) / 8

//    val bytes: Array[Byte] = new Array[Byte](4)
//    for (i <- 0 until byteNum) {
//      bytes(3 - i) = (input >>> (i * 8)).asInstanceOf[Byte]
//    }
//    bytes

    CodingHelper.csar[Array[Byte]](new Array[Byte](4)) {
      bytes => {
        for (i <- 0 until byteNum) {
          bytes(3 - i) = (input >>> (i * 8)).asInstanceOf[Byte]
        }
      }
    }
  }

  /**
    * 将一个 Int 数据转成一个  Byte 数组
    *
    * @param input  待转换的整数
    * @param length 转换后的数组大小
    * @return
    */
  def int2bytes(input: Int, length: Int): Array[Byte] = {
//    val bytes: Array[Byte] = new Array[Byte](length)
//    for (i <- 0 until length) {
//      bytes(i) = (input >>> ((length - 1 - i) * 8)).asInstanceOf[Byte]
//    }
//    bytes
    CodingHelper.csar[Array[Byte]](new Array[Byte](length)) {

      bytes => {
        for (i <- 0 until length) {
          bytes(i) = (input >>> ((length - 1 - i) * 8)).asInstanceOf[Byte]
        }
      }
    }
  }

  def int2bytes1(input: Int): Array[Byte] = int2bytes(input, 1)

  def int2bytes2(input: Int): Array[Byte] = int2bytes(input, 2)

  /**
    * 这个方法要比下面的拼接字符串的快一点
    *
    * @param data byte Array
    * @return
    */
  def bytes2DateTimeString(data: Array[Byte]): String = {

    GENERAL_DATETIME_PATTERN.format(bytes2DateTime(data))

  }

  /**
    *
    * @param data byte Array
    * @return
    */
  def bytes2DateTime(data: Array[Byte]): LocalDateTime = {
    LocalDateTime.of(
      data(0).toInt + 2000,
      data(1).toInt,
      data(2).toInt,
      data(3).toInt,
      data(4).toInt,
      data(5).toInt)

  }

  /**
    * 取出时间的字符串
    *
    * @param data byte Array
    * @return
    */
  def bytes2DatetimeString2(data: Array[Byte]): String = {
    val df = new DecimalFormat("00")

    "20" + df.format(data(0) & 0xFF) +
      "-" + df.format(data(1) & 0xFF) +
      "-" + df.format(data(2) & 0xFF) +
      " " + df.format(data(3) & 0xFF) +
      ":" + df.format(data(4) & 0xFF) +
      ":" + df.format(data(5) & 0xFF)
  }

  def hexstr2bytes(src: String): Array[Byte] = {

    @tailrec
    def loop(input: String, index: Int, arrayBuffer: ArrayBuffer[Byte]): Array[Byte] = {

      if (index >= input.length / 2) {
        arrayBuffer.toArray
      } else {
        import java.lang.{Integer => JavaInteger}

        val suffix = input.substring(index * 2, index * 2 + 2)
        // https://blog.csdn.net/fivestar2009/article/details/78625202
        // 但是如何这个数是大于0x8x（不能大于127）就会有问题
        // java.lang.NumberFormatException: Value 254 out of range from 0xFE ...
        //
        // val nm = "0x" + suffix
        // val theByte = JavaByte.decode(nm).asInstanceOf[Byte]
        // 先转成整数
        val theByte = JavaInteger.valueOf(suffix, 16).byteValue()

        arrayBuffer += theByte
        loop(input, index + 1, arrayBuffer)
      }
    }

    if (src == null || src.trim().isEmpty) ByteHelper.EMPTY_ARRAY
    else loop(src, 0, ArrayBuffer[Byte]())

  }

  def bytes2hexstr(src: Array[Byte], separator: String = EMPTY_STRING): String = {

    @tailrec
    def loop(input: Array[Byte], index: Int, stringBuilder: StringBuilder): String = {
      if (index >= input.length) {
        stringBuilder.toString()
      } else {
        val tmp = Integer.toHexString(java.lang.Byte.toUnsignedInt(input(index)))
        if (tmp.length == 1) {
          stringBuilder ++= "0"
        }
        stringBuilder ++= tmp

        if (separator != null && separator.length > 0) {
          stringBuilder ++= separator
        }
        loop(input, index + 1, stringBuilder)
      }
    }

    if (src == null || src.length == 0) ByteHelper.EMPTY_STRING
    else loop(src, 0, new StringBuilder(src.length * 3, EMPTY_STRING))

  }


}
