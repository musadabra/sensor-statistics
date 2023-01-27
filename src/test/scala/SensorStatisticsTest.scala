import SensorStatistics.{doStatistics, getCSVFiles}
import org.mockito.Mockito.*
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import scala.collection.immutable.List
import scala.collection.mutable

class SensorStatisticsTest extends AnyFlatSpec with Matchers {

  "getCSVFiles" should "return an array of CSV files in the given directory" in {
    // Create a mock File object for the directory
    val mockDir = mock(classOf[File])
    val resourceDirectory = new File(".").getCanonicalPath.concat("\\data")

    when(mockDir.exists()).thenReturn(true)
    when(mockDir.isDirectory()).thenReturn(true)
    when(mockDir.getPath()).thenReturn(resourceDirectory)

    // Create a mock File object for a CSV file
    val mockCSVFile = mock(classOf[File])
    when(mockCSVFile.getName()).thenReturn("data.csv")

    // Create a mock File object for non CSV file
    val nonCsv = mock(classOf[File])
    when(nonCsv.getName()).thenReturn("noncsv.pdf")

    // Set the mock directory's listFiles method to return an array containing the mock CSV file
    when(mockDir.listFiles()).thenReturn(Array(mockCSVFile, nonCsv))

    val csvFiles = getCSVFiles(mockDir.getPath())

    csvFiles should not be null
    csvFiles should have length 1
  }

  it should "return null if the given path is not a valid directory" in {
    // Create a mock File object for a non-existent directory
    val mockDir = mock(classOf[File])
    when(mockDir.exists()).thenReturn(false)
    when(mockDir.isDirectory()).thenReturn(false)

    val csvFiles = getCSVFiles("/path/to/dir")
    csvFiles should be (null)
  }

  it should "return null if there are no CSV files in the given directory" in {
    // Create a mock File object for the directory
    val mockDir = mock(classOf[File])
    when(mockDir.exists()).thenReturn(true)
    when(mockDir.isDirectory()).thenReturn(true)

    // Create a mock File object for a non-CSV file
    val mockFile = mock(classOf[File])
    when(mockFile.getName()).thenReturn("data.txt")

    // Set the mock directory's listFiles method to return an array containing the mock non-CSV file
    when(mockDir.listFiles()).thenReturn(Array(mockFile))

    val csvFiles = getCSVFiles("/path/to/dir")
    csvFiles should be (null)
  }

  "doStatistics" should "return correct statistics for sensor data" in {
    // Create a mock sensor data map
    var mockSensorData = mutable.Map[String, List[Int]]()

    // Set up mock data for sensors data
    val sensor1Data = List(10, 20, 30)
    val sensor2Data = List(10, 20, 30, 40, 50)
    mockSensorData("sensor2") = sensor2Data
    mockSensorData("sensor1") = sensor1Data
    // Call the function under test
    val statistics = doStatistics(mockSensorData)

    // Verify the results
    statistics.headOption.get shouldBe ("sensor2", 10, 30, 50)
    statistics.last shouldBe ("sensor1", 10, 20, 30)
  }
}

