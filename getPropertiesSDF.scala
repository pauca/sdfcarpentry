
import scala.io._
import scala.collection.mutable.ArrayBuffer
import java.io.FileWriter

object getPropertiesSDF{
  def main(args: Array[String]) {
    val usage = """
scala getPropertiesSDF.scala -isdf <inputSDF> -ocsv <OutputCSV>
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
        case "-ocsv"          :: value :: tail => nextOption(map ++ Map( "ocsv" -> value ), tail)
        case option :: tail => println("Unknown option:\t"+option) 
                               println(usage) 
                               sys.exit(1) 
      }
    }
  // mapping of parameters to variables
    val options = nextOption(Map(),arglist)
    
    try{
      var ifileNameSdf      = options("isdf").toString 
      var ofileNameCsv      = options("ocsv").toString  

     if(ifileNameSdf.size == 0 | ofileNameCsv.size == 0 ){
        throw new Exception("Error: Missing input.")
      }
  /* new file creation*/
      var output = new java.io.FileWriter( ofileNameCsv )
      try{ 
        var map = scala.collection.mutable.Map[java.lang.String,java.lang.String]()
        var mapsBuffer = ArrayBuffer[Map[java.lang.String,java.lang.String]] ()
        var label = new String()
        for( line <- io.Source.fromFile(ifileNameSdf).getLines()){
          if( line.startsWith("$$$$") ){
            mapsBuffer = mapsBuffer ++  ArrayBuffer(map.toMap )
            map.clear
          }else{
            if( line.startsWith(">  <") ){
              label = line.split("[<>]")(2)
            }else{
              if( label.size > 0 ){
                map = map ++ Map((label , line))
                label = ""
              }
            }
          } 
        }
        
       var header = mapsBuffer.map( x => x.keys ).reduceLeft((x,y)=> x ++ y) 
       var line = "" 
       for(col <- header){ line += col + "\t"  }
       output.write(line + "\n")
       for( map <- mapsBuffer ){
          line = ""
          for(col <- header){
             try{
                line += map(col) + "\t" 
              }catch{
                  case e: java.util.NoSuchElementException => line += "" 
              }
           }
           output.write(line + "\n")
        }
      }catch{
        case e: java.lang.UnsupportedOperationException => println("Unsupported Operation: Check input files.")
        case e: Exception                           => println("Exception caught:\n" + e);
      }finally{ output.close}
   }catch{
      case e: Exception                           => println( "Exception caught:\n" + e);
   }
  }

}