package ppl.ert.scripts

import _root_.scala.collection.mutable.ListBuffer
import _root_.scala.collection.mutable.Map
import java.io._

object netflixERTgen {
  val beginTime = System.currentTimeMillis
  

  def main(args: Array[String]) {
    log("Generating Netflix ERT input set")
    
    //val n = java.lang.Integer.parseInt(args(0))
    //log("Will generate input based on " + n + " most commonly rated movies")
    
    val netflixDir = args(0)
    log("Using netflix data directory:" + netflixDir)    
       
    //Step 0: Load up the movies
    val movieData = loadMovieData(netflixDir+"/movie_titles.txt");
    val movieCount = movieData.size - 1
    log("loaded " + movieCount +" movies ")
    
    //Step 1: Generate or load master customer file
    //  this includes the preferences of all the customers for all the movies
    val customerRows = loadOrGenCustomerData(movieCount, netflixDir);
    log("loaded " + customerRows.size + " customer movie rating rows")
    
    //For now after talking with nathan, we are simply are going to print 
    //the customer data 
    printCustomerData(customerRows, movieCount, netflixDir)
    
    
  }
  
  def printCustomerData(cd: Map[Int, Map[Int,Int]], count: Int, dir: String ) {   
    var suffixCount = 1
    var output = getBufferedWriter(new FileWriter(dir+"/customer_data_" + getSuffixString(suffixCount) + ".txt"))        
    var lineCount = 0
    
    try {
      cd.foreach( row => {
        var line = new StringBuilder
        line.append( row._1)
        val ratings = row._2
        for( i <- 1 to count) {
          if(ratings.contains(i)) {
            line.append("," + ratings(i))
          } else {
            line.append(",-1")
          }          
        }
        line.append("\n")
        output.append(line)
        lineCount += 1        
        if(lineCount % 10000 == 0) {
          //Skip to next file
          suffixCount += 1
          output.close
          output = getBufferedWriter(new FileWriter(dir+"/customer_data_" + getSuffixString(suffixCount) + ".txt"))
          log("Switching to :" + dir+"/customer_data_" + getSuffixString(suffixCount) + ".txt")
        }
      })
    }
    finally {
      output.close
    }
  }
  
  def getSuffixString(count : Int): String =  {
    var charCount = 3
    var c = count
    while (c / 10 != 0) {
      charCount -= 1
      c = c / 10
    }
    return "0" * charCount + count
  }
   
  def loadOrGenCustomerData(count: Int, dir: String): Map[Int, Map[Int,Int]] = {
    var customerData = Map.empty[Int, Map[Int,Int]]
    //if(loadCustomerData(customerData, dir))
    //  return customerData
    //Regenerate customer data
    log("no customer data cache found, need to generate it")
    for(i <- 1 to count) {
      //figure out how many zeros to add to the file name
      var zeros = 6;
      var a = i
      while (a / 10 != 0)  {
        a = a / 10
        zeros -= 1
      }
      var filename = dir + "/training_set/mv_" + ("0"*zeros) + i + ".txt"
      log("loading movie " + filename)
      processMovieForCustomers(customerData,filename,i, count)
    }
    //log("saving customer data cache file")
    //saveCustomerDataCache(customerData, dir)
    customerData
  }
  
  def saveCustomerDataCache(cd: Map[Int, Map[Int,Int]], dir: String) {
    val file = new FileWriter(dir+"/cached_customer_data.txt")
    val output = getBufferedWriter(file)
    try {
      
      cd.foreach(e => {
          
        val rattingAttr = e._2.map(e => {
          val str = ""+ e._1 + ":" + e._2
          str
        }).mkString(";",",",";")
        val line: String = ""+ e._1  + rattingAttr+ "\n"        
        output.append(line)
        }                 
      )        
      
    } finally {
      output.close
    }
  }
  
  def loadCustomerData(cd: Map[Int, Map[Int, Int]], dir: String): Boolean = {
    val customerDataFile = dir + "/cached_customer_data.txt"
    val file = try {
      new FileReader(customerDataFile)
    } catch {
      case ex: FileNotFoundException =>
        return false
    }
    val input = getBufferedReader(file)
    log("found customer cache, loading it")
    try {      
      var line: String = input.readLine
      while(line!=null) {
        //Do Stuff
        val values = line.split(";")
        val custId = java.lang.Integer.parseInt(values(0))
        cd += (custId -> Map.empty[Int,Int])
        val ratings = values(1).split(",")
        ratings.foreach(r => {
          val rt = r.split(":")
          val movieID = java.lang.Integer.parseInt(rt(0))
          val movieRating = java.lang.Integer.parseInt(rt(1))
          cd(custId)(movieID) = movieRating          
        })
        line = input.readLine
      }
    } finally {
      input.close
    }
    return true;
  }
  
  def getBufferedReader(file: FileReader):BufferedReader = {
    if (file.isInstanceOf[BufferedReader]) {
        file.asInstanceOf[BufferedReader]
      } else {
      new BufferedReader(file)
      }
  }
  
  def getBufferedWriter(file: FileWriter):BufferedWriter = {
    if (file.isInstanceOf[BufferedWriter]) {
        file.asInstanceOf[BufferedWriter]
      } else {
      new BufferedWriter(file)
      }
  }
  
  def processMovieForCustomers(cd: Map[Int, Map[Int,Int]], f: String, movieID: Int, c: Int) {
    val file = new FileReader(new File(f))
    val input = getBufferedReader(file)    
    try {
      
      //throw the first line out
      val idLine = input.readLine
      val parsedId =  java.lang.Integer.parseInt(idLine.split(":")(0)) 
      require(parsedId == movieID)
      var line: String = input.readLine
      while(line!=null) {
        //log(line)
        val values = line.split(",")
        val custId = java.lang.Integer.parseInt(values(0))
        val rating = java.lang.Integer.parseInt(values(1))
        //add rating to customer
        if(cd.contains(custId) == false){                  
          cd += (custId -> Map.empty[Int,Int])
        }
        cd(custId)(movieID) = rating
        line = input.readLine
      }
    }
    finally {
      input.close
    }
  }
  
  
  def log(msg: String) {
    val now = System.currentTimeMillis - beginTime
    printf("T+%6d: %s\n", now, msg)
  }
  
  
  
  def loadMovieData(filename:String) = loadMovies(new File(filename))
  
  def loadMovies(file:File): Array[(Int, String)] = {
    val xi = new FileReader(file)
    try {
      loadMovies(xi)
    }
    finally {
      xi.close
    }
  } 
  
    
  def loadMovies(xi: InputStreamReader): Array[(Int, String)] = {
    val input = if (xi.isInstanceOf[BufferedReader]) {
      xi.asInstanceOf[BufferedReader]
    } else {
      new BufferedReader(xi)
    }

    val buf = new ListBuffer[(Int, String)]
    var line: String = input.readLine
    var idx = 1;
    buf += (0, "NETFLIX MOVIE IDS START AT 1")
    while (line != null) {     
      val values = line.split(",");      
      val id = java.lang.Integer.parseInt(values(0))
      val year = 
        if(values(1) == "NULL")
          0		
        else		
          java.lang.Integer.parseInt(values(1))
      val title = values(2)
      require(id == idx)
      buf += (year,title)
      idx += 1
      line = input.readLine
    }
    buf.toArray
    
  }
}
