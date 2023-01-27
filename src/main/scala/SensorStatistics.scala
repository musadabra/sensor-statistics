import java.io.{BufferedReader, File, FileReader}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object SensorStatistics {

  def main(args: Array[String]): Unit = {

    if(args.length < 1){
      println("*No Command line argument passed")
      sys.exit(1)
    }

    val directoryPath = args(0)

    // return an array of csv file in directory the directory
    val files = getCSVFiles(directoryPath)

    if(files == null){
      println("*Closing program")
      sys.exit(1)
    }

    var processedMeasurements = 0
    var failedMeasurements = 0
    val sensorData = mutable.Map[String, List[Int]]()
    val failedSensorData = mutable.Map[String, List[String]]()

    for (file <- files) {
      val reader = new BufferedReader(new FileReader(file))
      reader.readLine() // skip the header line
      var line = reader.readLine()

      // while the line contains data and its not the end of csv lines.
      while (line != null && !line.startsWith(",,")) {
        try {
          val data = line.split(",")
          val sensorId = data(0)
          val humidity = data(1)

          if (humidity != "NaN"){
            // Count processed mesurments and recode sensor data.
            processedMeasurements += 1
            val currentData = sensorData.getOrElse(sensorId, List())
            sensorData(sensorId) = humidity.toInt :: currentData
          } else {
            // Count failed measuments
            failedMeasurements += 1
            // Recode Failed sensor
            val currentFailedData = failedSensorData.getOrElse(sensorId, List())
            failedSensorData(sensorId) = humidity :: currentFailedData
          }
          line = reader.readLine()
        } catch {
          case e: Exception =>{
            println(s"An error occurred while reading csv: $e \n File name ${file.getName} \n *Check column format")
            sys.exit(1)
          }
        }
      }
    }

    println(s"Num of processed files: ${files.length}")
    println(s"Num of processed measurements: $processedMeasurements")
    println(s"Num of failed measurements: $failedMeasurements")

    val sortedStatistics = doStatistics(sensorData).toList.sortBy(_._3.toString.toInt).reverse

    println("\nSensors with highest avg humidity:\n")
    println("sensor-id,min,avg,max")

    // All sorted statistics
    sortedStatistics.foreach((sensorId, min, avg, max) => println(s"$sensorId,$min,$avg,$max"))

    // All failed sensor
    filterAllFailedSensor(failedSensorData, sensorData).foreach(x => println(s"$x,NaN,NaN,NaN"))
  }

  // ReadCSV files
  def getCSVFiles(path: String): Array[File] = {
    val dir = new File(path)

    if (!dir.exists() || !dir.isDirectory) {
      println(s"Error: $path is not a valid directory")
      return null
    }

    val csvFiles = dir.listFiles().filter(_.getName.endsWith(".csv"))
    if (csvFiles.length < 1) {
      println(s"Error: no .csv file in this directory: $path")
      return null
    }
    csvFiles
  }

  // Returns With Min, Average and max Iterable[(String, Any, Any, Any)]
  def doStatistics(sensorData: mutable.Map[String, List[Int]]) = for ((sensorId, data) <- sensorData) yield {
    if (data.isEmpty) {
      (sensorId, "NaN", "NaN", "NaN")
    } else {
      val min = data.min
      val max = data.max
      val avg = data.sum / data.length
      (sensorId, min, avg, max)
    }
  }

  private def filterAllFailedSensor(failedData: mutable.Map[String, List[String]], sensorData: mutable.Map[String, List[Int]]) = {
    val sensorIds = sensorData.toList.map(_._1)
    failedData.filterNot(x => sensorIds.contains(x._1)).collect(x => x._1)
  }

}