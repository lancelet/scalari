package ri

import java.io.{BufferedReader, File, InputStream, InputStreamReader, OutputStream, OutputStreamWriter}


/** Writer of RIB files to Aqsis. 
 * TODO: Handle streams from the renderer much better; provide feedback on rendering progress, etc.
 */
class AqsisRibWriter extends RibWriter {
  
  val process: Process = {
    val cmdArray: Array[String] = Array("aqsis")
    val curDir: File = new File(System.getProperty("user.dir"))
    Runtime.getRuntime().exec(cmdArray, null, curDir)
  }
  val stdin: OutputStream = process.getOutputStream()
  val stderr: InputStream = process.getErrorStream()
  val stdout: InputStream = process.getInputStream()
  
  val inReader: BufferedReader = new BufferedReader(new InputStreamReader(stdout))
  val errReader: BufferedReader = new BufferedReader(new InputStreamReader(stderr))
  val writer: OutputStreamWriter = new OutputStreamWriter(stdin)
  
  def incrementIndent(): Unit = { }
  def decrementIndent(): Unit = { }
  def writeln(str: String): Unit = { 
    writer.write("%s\n" format(str))
    writer.flush()
    if (errReader.ready()) {
      throw new RiRendererException(errReader.readLine())
    }
    while (inReader.ready()) {
      println(inReader.readLine())
    }
  }
  def close(): Unit = {
    writer.close()
    process.waitFor()
    if (errReader.ready()) {
      val sb = new StringBuffer()
      while (errReader.ready()) {
        sb.append(errReader.readLine())
      }
      throw new RiRendererException(sb.toString)
    }
    inReader.close()
    errReader.close()
    stdin.close()
    stderr.close()
    stdout.close()
  }
}