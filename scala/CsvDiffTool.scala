package difftool

case class LineDiff(key: String, val1: String, val2: String)

object CsvDiffTool {
  def main(args: Array[String]) {
    val (file1, file2, keystr, except) = (args(0), args(1), args(2), args(3))
    val keys = keystr.split(",")
    val(arr1, arr2) = (io.Source.fromFile(file1).getLines.toArray, io.Source.fromFile(file2).getLines.toArray)
    val headers = arr1(0).split(",")
    val (keyIndices, exceptIndices) = (getIndices(headers, keys), getIndices(headers, except.split(",")))
    
    if (!checkUniqueKey(arr1, keyIndices) || !checkUniqueKey(arr2, keyIndices))
      throw new RuntimeException("Key [" + keystr + "] does not identify unique rows")
    
    test1(arr1.drop(1), arr2.drop(1), headers, keyIndices, exceptIndices)
  }

  def getKey(line: String, keys: Array[Int]): String= {
    val arr = line.split(",")
    keys.map(arr(_)).mkString(":")
  }
  
  def diffTwoCSVFileContents(arr1: Array[String], arr2: Array[String], keys: Array[Int], except: Array[Int]) :  Array[LineDiff] = {
    val allKeys = arr1.union(arr2).map(x => getKey(x, keys)).distinct
    for {
      key <- allKeys
      val line1 = arr1.find(x => getKey(x, keys) == key)
      val line2 = arr2.find(x => getKey(x, keys) == key)
      if (hasDiff(line1, line2, except))
    } yield LineDiff(key, line1.getOrElse("NONE"), line2.getOrElse("NONE"))
  }
  
  def hasDiff(line1: Option[String], line2: Option[String], except: Array[Int]): Boolean = {
    if (!line1.isEmpty && !line2.isEmpty) {
      val (arr1, arr2) = (line1.get.split(","), line2.get.split(","))
      val newline1 = (0 until arr1.length).toArray.diff(except).map(arr1(_)).mkString(":")
      val newline2 = (0 until arr2.length).toArray.diff(except).map(arr2(_)).mkString(":")
      newline1 != newline2
    }
    else
      line1 != line2
  }
  
  def getIndices(headers: Array[String], keys: Array[String]) : Array[Int] = {
    headers.zipWithIndex.filter{case (col, index) => keys.contains(col)}.map(x => x._2)
  }

  def checkUniqueKey(lines: Array[String], keys: Array[Int]) : Boolean = {
    val keyList = lines map (x => getKey(x, keys))
    return keyList.distinct.length == lines.length
  }
  
  def test1(arr1: Array[String], arr2: Array[String], headers: Array[String], keys: Array[Int], except: Array[Int]) = {
    printDiffs(diffTwoCSVFileContents(arr1, arr2, keys, except))
  }
  
  def printDiffs(diffs: Array[LineDiff]) = {
    diffs.foreach(ld => println("Key: " + ld.key + "\n  Input1: " + ld.val1 + "\n  Input2: " + ld.val2))
  }
  
}
