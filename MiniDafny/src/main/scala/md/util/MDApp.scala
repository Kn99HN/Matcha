package md.util

import java.io.File
import java.io.FileNotFoundException


abstract class MDApp extends App {
  def processFile(file: File)
  def init(): Unit = ()
  
  case class Config(debug: Boolean = false, verif: Boolean = false,
       files: List[File] = Nil)
  
  val usage = """MiniDafny Verifier
    Usage: run [options] [<file>...]
      
      -d  | --debug
            Print debug messages
      -h  | --help
            prints this usage text
      -ve | --verify
            verifying
      <file>...
            programs to be verified
    """
  
  val config = ((Some(Config()): Option[Config]) /: args) {
    case (Some(c), "-d") => Some(c.copy(debug = true))
    case (Some(c), "--debug") => Some(c.copy(debug = true))
    case (Some(c), "-h") => None
    case (Some(c), "--help") => None
    case (Some(c), "-ve") => Some(c.copy(verif = true))
    case (Some(c), f) => 
      println(c)
      Some(c.copy(files = c.files :+ new File(f)))
    case (None, _) => None
  } getOrElse {
    println(usage)
    System.exit(1)
    Config()
  }
     
  var debug: Boolean = config.debug
  var verif: Boolean = config.verif
    
  var optFile: Option[File] = None 
  
  def handle[T](default: => T)(e: => T): T = 
    try e catch {
      case ex: MDException =>
        val fileName = optFile map (_.getName()) getOrElse "[eval]" 
        println(s"$fileName:$ex")
        default
      case ex: FileNotFoundException =>
        optFile match {
          case Some(f) =>
            println("Error: cannot find file '" + f.getCanonicalPath + "'")
            default
          case None =>  
            ex.printStackTrace(System.out)
            default
        }
      case ex: Throwable =>
        ex.printStackTrace(System.out)
        default
    }
  
  def fail(): Nothing = scala.sys.exit(1)
  
  init()
  for (f: File <- config.files) {
    optFile = Some(f)
    handle(fail())(processFile(f))
  }
}