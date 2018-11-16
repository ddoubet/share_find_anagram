
/*
 *  # Author: Dustin Doubet
    # Description: Find if two words can be found to be an anagram
    # Scala 2.11.8
    # Java 1.8
    # 
    # Requirements:
    #
    # No 3rd party libs or special functions
    #
    # Case 1: Find if the words are an anagram: 'Cat' and 'Act'
    # Case 2: Apply for words: 'Caat' and 'Aact'
    # Case 3: Use only one dictionary and performance optimization is important
    # 
    # Unknown: Not sure if white space is allowed which would produce a phrase and not a word
    #
 */

object FindAnagram {
  
  type CharMap = Map[Char,Long]
  type TupleCharArray = Tuple2[Array[Char],Array[Char]]
  
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
  
  def getCharMap(word: String): Map[Char,Long] = {
    /* Creates a Map[Char,Long] of unique character keys with each
        character count as the value.
        { 'c' : 1, 'a' : 2, 't' : 1}
    */
    var charMap = Map.empty[Char,Long]
    for ( char <- word.toCharArray() ) {
      if ( !charMap.contains(char) ) {
        charMap = charMap.updated(char, 1)
      } else {
        charMap = charMap.updated(char, charMap(char) + 1)
      }
    }
    charMap
  }
  
  def getCharMap(charArray: Array[Char]): Map[Char,Long] = {
    /* Creates a Map[Char,Long] of unique character keys with each
        character count as the value.
        { 'c' : 1, 'a' : 2, 't' : 1}
    */
    var charMap = Map.empty[Char,Long]
    for ( char <- charArray ) {
      if ( !charMap.contains(char) ) {
        charMap = charMap.updated(char, 1)
      } else {
        charMap = charMap.updated(char, charMap(char) + 1)
      }
    }
    /*
     * Alternate style of looping:
     * charArray.foreach{ char => if (!charMap.contains(char)) charMap = charMap.updated(char, 1) 
     * 														else charMap = charMap.updated(char, charMap(char) + 1)
     * 									 }
     */
    charMap
  }
  
  def testForUniqueChars(word1: String, word2: String) : Either[Boolean,TupleCharArray] = {
    val charArray1 = word1.toCharArray()
    val charArray2 = word2.toCharArray()
    if ( charArray1.filter(char => charArray2.contains(char)).length == 
         charArray2.filter(char => charArray1.contains(char)).length
       ) {
      Right((charArray1,charArray2))
    } else {
      Left(false)
    }
  }
  
  def getCharMapMultiWord(word1: String, word2: String) : CharMap = {
    /* Creates a Map[Char,Long] of unique character keys with each
        character count as the value from two words.
        { 'c' : 1, 'a' : 2, 't' : 1}
    */
    val charArray = word1.toCharArray() ++ word2.toCharArray()
    var charMap = Map.empty[Char,Long]
    for ( char <- charArray ) {
      if ( !charMap.contains(char) ) {
        charMap = charMap.updated(char, 1)
      } else {
        charMap = charMap.updated(char, charMap(char) + 1)
      }
    }
    charMap
  }
  
  def getCharMapMultiWord(charArray1: Array[Char], charArray2: Array[Char]) : CharMap = {
    /* Creates a Map[Char,Long] of unique character keys with each
        character count as the value from two words.
        { 'c' : 1, 'a' : 2, 't' : 1}
        
        This method was added so there would not be a collection conversion
    */
    val charArray = charArray1 ++ charArray2
    var charMap = Map.empty[Char,Long]
    for ( char <- charArray ) {
      if ( !charMap.contains(char) ) {
        charMap = charMap.updated(char, 1)
      } else {
        charMap = charMap.updated(char, charMap(char) + 1)
      }
    }
    charMap
  }
  
  def compareCrossCount(charMap: CharMap): Boolean = {
    /*
     * This is taking the CharMap that has the unique characters
     * from both words and then counts, which we will test for an
     * even amount of character counts.
     */
    return (charMap.keys.size == charMap.filter{ case (k,v) => (v % 2 == 0) }.keys.size)
  }
  
  
  def sortCompare(word1: String, word2: String) : Boolean = {
    /*
     * Convert both lower cased words to a Array[Char] then sort them
     * and then compare the sorted character arrays against each other.
     * If the same then both arrays will have the equal character in 
     * each position.
     * 
     * array1 = ['a','a','c','t']
     * 
     * equal if a anagram
     * 
     * array2 = ['a','a','c','t']
     * 
     * The filter option could be done a few ways but was simple and
     * allows to filter out mismatches which will be detected onthe length
     * comparision.
     * 
     */
    val sorted1: Array[Char] = word1.toCharArray().sorted
    val sorted2: Array[Char] = word2.toCharArray().sorted
    return sorted1.length == sorted1.zipWithIndex.filter{ case (item, i) => item == sorted2(i)}.length
  }

  def main(args: Array[String]) {
    // args.foreach(i => println(i))
    
    var word1: String = args(0)
    //var word1: String = "Caaatt"
    println("Word 1: " + word1)
    var word2: String = args(1)
    //var word2: String = "Aaactt"
    println("Word 2: " + word2)
    
    word1 = word1.toLowerCase()
    word2 = word2.toLowerCase()
    
    val noMsg : String = "These words are not an anagram"
    var yesMsg : String = s"These words $word1, $word2 are an anagram"
    
    if ( word1.length != word2.length ) {
      println(noMsg)
    } else {
      /////////////////// 1st Method /////////////////////////
      println("\nRunning 1st method for sort compare...")
      if ( (time {sortCompare(word1, word2)}) ) {
        println(yesMsg)
      } else {
        println(noMsg)
      }
      
      /////////////////// 2st Method /////////////////////////
      println("\nRunning 2nd method for CharMap cross count compare...")
      time { 
            testForUniqueChars(word1, word2) match {
              case Left(failedTest) => 
                // Same length but not the same characters so short circuit.
                println(noMsg)
        
              case Right(tupleArray) =>
                // We know the two words have the same length and same characters at this point
                if ( compareCrossCount(getCharMapMultiWord(tupleArray._1,tupleArray._2)) ) {
                  println(yesMsg)
                } else{
                  println(noMsg)
                }
            }
           }
  
    }
  }
}
  
