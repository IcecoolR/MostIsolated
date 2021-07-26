import java.nio.file.{Paths, Files}
import java.io.File
import scala.io.Source
import math. {sqrt, pow}


object MostIsolated {
  
  def main(args: Array[String]): Unit = {
    
    val fileName = scala.io.StdIn.readLine("Please enter the name of the .txt file: ")
    val path = System.getProperty("user.dir") + "\\data\\" + fileName + ".txt"
    
    println("Looking for: " + path)
   
    if (Files.exists(Paths.get(path))) {
      val file = new File(path)
      if (!Source.fromFile(file).getLines().isEmpty){
        mostIsolated(file)
      } else {
        println("File is empty")
      }
    } else {
      println("File not found")
    }    
  }
  
  
  def mostIsolated(file: File) {
    
    val text = Source.fromFile(file).getLines.toList
    
    var data: List[(String, Int, Int)] = List()
    
    text.foreach(line => {
      val name = line.takeWhile(char => char != ' ')
      val x = line.dropWhile(char => char != ' ').tail.takeWhile(char => char != ' ')
      val y = line.dropWhile(char => char != ' ').tail.dropWhile(char => char != ' ').tail
      data = data:+((name, x.toInt, y.toInt))
    })
    
    if (data.size == 1){
      
      println(data(0)._1 + ", " + data(0)._2 + ", " + data(0)._3)
      
    } else if (data.size == 2) {
      
      println(data(0)._1 + ", " + data(0)._2 + ", " + data(0)._3)
      
    } else {
      
      var radius = 1
      var nearest = (("", -1, -1))
      var distance = 0.0
      var distances: List[Double] = List()
      var isFound = false
            
      data.foreach(x => {
        
        radius = 1
        isFound = false
        
        while (!isFound) { 
          
          nearest = data.find(check => pow ((check._2 - x._2), 2 ) + pow ((check._3 - x._3), 2 ) <= pow(radius, 2) && check != x).getOrElse(null)
              
          if (nearest != null) {
            
            distance = sqrt ( pow ( x._2 - nearest._2, 2 ) + pow ( x._3 - nearest._3, 2 ))
            distances = distances.:+(distance)
            isFound = true
            
          } else {
            radius = radius + 1
          }            
        }
      })
      
      var mostIsolated = data.zip(distances).maxBy(_._2)
      
      println()
      println(mostIsolated._1._1 + ", " + mostIsolated._1._2 + ", " + mostIsolated._1._3)

    }
    
    
  }
  
}