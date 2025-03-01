package Utils

import java.io.{FileInputStream, IOException}
import java.util
import java.util.StringTokenizer
import scala.collection.mutable.ArrayBuffer

/**
 * TODO This class was imported using IntelliJ Idea feature of translating Java <br> 
 * code to Scala. It must be adapted to Scala principles 
 * 
 * Class for obtaining the algorithm configuration parameters from a configuration file
 *
*/
class ConfigurationParser {
  final private val _inputFiles: ArrayBuffer[String] = new ArrayBuffer[String]()
  final private val _outputFiles: ArrayBuffer[String] = new ArrayBuffer[String]()
  final private val _parameters: ArrayBuffer[String] = new ArrayBuffer[String]()
  private var _algorithmName: String = _

  def parseConfigurationFile(fileName: String): Unit = {
    val line: StringTokenizer = new StringTokenizer(readFile(fileName), "\n\r")
    readName(line)
    readInputFiles(line)
    readOutputFiles(line)
    readAllParameters(line)
  }

  private def readFile(fileName: String): String = {
    val content: StringBuilder = new StringBuilder
    try {
      val fis: FileInputStream = new FileInputStream(fileName)
      val piece: Array[Byte] = new Array[Byte](4096)
      var readBytes: Int = 0
      while (readBytes != -1) {
        readBytes = fis.read(piece)
        if (readBytes != -1) content.append(new String(piece, 0, readBytes))
      }
      fis.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
        System.exit(-1)
    }
    content.toString
  }

   private def readName(line: StringTokenizer): Unit = {
    val data: StringTokenizer = new StringTokenizer(line.nextToken, " =\"")
    data.nextToken
    val sb: StringBuilder = new StringBuilder(data.nextToken)
    while (data.hasMoreTokens)
      sb.append(" ").append(data.nextToken)
    _algorithmName = sb.toString
  }

   private def readInputFiles(line: StringTokenizer): Unit = {
    val data: StringTokenizer = new StringTokenizer(line.nextToken, " =\"")
    data.nextToken
    while (data.hasMoreTokens) 
      _inputFiles.addOne(data.nextToken)
  }

  private def readOutputFiles(line: StringTokenizer): Unit = {
    val data: StringTokenizer = new StringTokenizer(line.nextToken, " =\"")
    data.nextToken
    while (data.hasMoreTokens) 
      _outputFiles.addOne(data.nextToken)
  }

   private def readAllParameters(line: StringTokenizer): Unit = {
    while (line.hasMoreTokens) {
      val data: StringTokenizer = new StringTokenizer(line.nextToken, " =\"")
      var string: String = ""
      while (data.hasMoreTokens) string = data.nextToken
      _parameters.addOne(string)
    }
  }

  def algorithmName: String = _algorithmName

  def parameters: Array[String] = _parameters.toArray

  def getParameter(pos: Int): String = _parameters(pos)

  def inputFiles: Array[String] = _inputFiles.toArray

  def getInputFile(pos: Int): String = _inputFiles(pos)

  def outputFiles: Array[String] = _outputFiles.toArray

  def getOutputFile(pos: Int): String = _outputFiles(pos)
}
