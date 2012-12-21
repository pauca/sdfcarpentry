
import scala.io._
import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter

object addProperty2SDF{
  def main(args: Array[String]) {
    val usage = """
scala addProperty2SDF.scala -isdf <inputSDF> -ipropertyf <inputProperyFile> -opropertyName <NewPropertyName> -osdf <OutputSdf>
                """      
  /* Input parsing & processing */
    if (args.length == 0) { 
      println(usage)
      sys.exit(1)
    }
    val arglist = args.toList
    type OptionMap = Map[String, Any]
  //  recursive reading of command line parameters into Map
    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      list match {
        case Nil => map
        case "-help"   :: tail       =>   println(usage)
                                          sys.exit(1)
        case "-isdf"          :: value :: tail => nextOption(map ++ Map( "isdf" -> value ), tail)
        case "-osdf"          :: value :: tail => nextOption(map ++ Map( "osdf" -> value ), tail)
        case "-ipropertyf"    :: value :: tail => nextOption(map ++ Map( "ipropertyf"    -> value ), tail)
        case "-opropertyName" :: value :: tail => nextOption(map ++ Map( "ipropertyName" -> value ), tail)
        case option :: tail => println("Unknown option:\t"+option) 
                               println(usage) 
                               sys.exit(1) 
      }
    }
  // mapping of parameters to variables
    val options = nextOption(Map(),arglist)

    try{
      var ifileNameSdf      = options("isdf").toString 
      var ofileNameSdf      = options("osdf").toString  
      var ifileNameProperty = options("ipropertyf").toString  
      var propertyName      = options("ipropertyName").toString 

     if(ifileNameSdf.size == 0 | ofileNameSdf.size == 0 | ifileNameProperty.size == 0 | propertyName.size == 0){
        throw new Exception("Error: Missing input.")
      }
  /* new file creation*/

      var output = new java.io.FileWriter( ofileNameSdf )
      var lineBuffer = new ArrayBuffer[String]() 
      try{ 
        var prop = io.Source.fromFile(ifileNameProperty).getLines().toList
        var imol : Int = 0;
        for( line <- io.Source.fromFile(ifileNameSdf).getLines()){
          if( line.startsWith("$$$$") ){
            lineBuffer.append( "" )
            lineBuffer.append(">  <"+ propertyName + ">")
            lineBuffer.append( prop(imol) )
            imol = imol + 1
            lineBuffer.append( "" )
            lineBuffer.append( line )
            
            for( lineB <- lineBuffer ){
              output.write(lineB+"\n")
            }
            lineBuffer.clear
          }else{
            lineBuffer.append(line)
          } 
        }
        if(imol != prop.size ) 
          throw new Exception("Error: A mismatch between numer of properties and number of molecules.")
        println("\nDone!")
      }catch{
        case e: java.lang.IndexOutOfBoundsException => println("Error: Possibly a mismatch between numer of properties and number of molecules.");
        case e: Exception                           => println("exception caught:\n" + e);
      }finally{ output.close 
                
}
   }catch{
      case e: java.util.NoSuchElementException    => println( "Incorrect input!\n" + usage)
      case e: Exception                           => println( "Exception caught:\n" + e);
   }
  }

}