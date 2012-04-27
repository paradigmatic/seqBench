package org.streum

import org.example._
import annotation.tailrec
import com.google.caliper.Param
import scala.collection._

object WordsGenerator {

  val wordLength = 8 
  val big = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val small = big.toLowerCase
  val letters = ( big + small ).map( _.toString )

  val rng = new util.Random(0)

  def randomWord =  {
    var str = ""
    while( str.length < wordLength ) {
      str += letters( rng.nextInt( letters.size ) )
    }
    str
  }
  
  lazy val stream = {
    def drawWord(): Stream[String] =
      Stream.cons( randomWord, drawWord() )
    drawWord()
  }
}


class TraversalVariations extends SimpleScalaBenchmark {
  
  @Param(Array("10", "100", "1000", "10000", "100000"))
  val length: Int = 0
  
  var words: Stream[String] = _
  var wordsList: List[String] = _
  var wordsArray: Array[String] = _
  var wordsIndexedSeq: IndexedSeq[String] = _
  
  override def setUp() {
    words = WordsGenerator.stream.take(length)
    wordsList = words.toList
    wordsArray = words.toArray
    wordsIndexedSeq = words.toIndexedSeq
  }
  
  def isCapitalized( s: String ) =
    java.lang.Character.isUpperCase(s.charAt(0))
  
  def timeOldSchool(reps: Int) = repeat(reps) {
    val n = words.length
    val wLength = Array.ofDim[Int](n)
    val wCaps = Array.ofDim[Boolean](n)
    var i = 0
    val it = wordsArray.iterator
    while( it.hasNext ) {
      val w = it.next
      wLength(i) = w.length
      wCaps(i) = isCapitalized(w)
      i += 1
    }
    ( wLength, wCaps )
  }   
  
  def timeOldSchoolSafe(reps: Int) = repeat(reps) {
    val n = words.length
    val wLength = Array.ofDim[Int](n)
    val wCaps = Array.ofDim[Boolean](n)
    var i = 0
    val it = wordsArray.iterator
    while( it.hasNext ) {
      val w = it.next
      wLength(i) = w.length
      wCaps(i) = isCapitalized(w)
      i += 1
    }
    ( wLength.toList, wCaps.toList )
  } 
      
  def timeFunctList(reps: Int) = repeat(reps) {
    val wLength = wordsList.map( _.length )
    val wCaps = wordsList.map( isCapitalized )
    (wLength, wCaps) 
  }
  
  def timeFunctVector(reps: Int) = repeat(reps) {
    val wLength = wordsIndexedSeq.map( _.length )
    val wCaps = wordsIndexedSeq.map( isCapitalized )
    (wLength, wCaps) 
  }    
     
  def timeReassign(reps: Int) = repeat(reps) {
    var wLength = List[Int]()
    var wCaps = List[Boolean]()
    for( word <- wordsList.reverse ) {
      wLength ::= word.length
      wCaps ::= isCapitalized(word)
    }
    (wLength, wCaps)
  }
  
  def timeRecurs(reps: Int) = repeat(reps) {
    
    @tailrec
    def lengthAndCaps( ws: List[String], ls: List[Int], cs: List[Boolean] ): (List[Int],List[Boolean]) = 
      if( ws.isEmpty ) 
        (ls, cs)
      else {
        val w = ws.head
        lengthAndCaps( ws.tail, w.length::ls, isCapitalized(w)::cs )
      }

    val (wLength,wCaps) = lengthAndCaps( wordsList.reverse, Nil, Nil )
    ( wLength, wCaps )
  }  
  
  def timeFold(reps: Int) = repeat(reps) {
    val (wLength,wCaps) = wordsList.foldLeft( List[Int]() -> List[Boolean]() ){ (lsts,w) =>
      ( w.length :: lsts._1, isCapitalized(w) :: lsts._2 )  
    }
    ( wLength, wCaps )
  }
  
  def timeBuffer(reps: Int) = repeat(reps) {
    import collection.mutable._
    val n = wordsArray.length
    val wLength = new ListBuffer[Int]()
    wLength.sizeHint(n)
    val wCaps = new ListBuffer[Boolean]()
    wCaps.sizeHint(n)
    for( word <- wordsList ) {
      wLength.append(word.length)
      wCaps.append(isCapitalized(word))
    }
    ( wLength, wCaps)
  }
  
  def timeBufferSafe(reps: Int) = repeat(reps) {
    import collection.mutable._
    val n = wordsArray.length
    val wLength = new ListBuffer[Int]()
    wLength.sizeHint(n)
    val wCaps = new ListBuffer[Boolean]()
    wCaps.sizeHint(n)
    for( word <- wordsList ) {
      wLength.append(word.length)
      wCaps.append(isCapitalized(word))
    }
    ( wLength.toList, wCaps.toList )
  }
  
  def timeBuilder(reps: Int) = repeat(reps) {
    val n = wordsList.length
    val wLength = List.newBuilder[Int]
    wLength.sizeHint(n)
    val wCaps = List.newBuilder[Boolean]
    wCaps.sizeHint(n)
    for( word <- wordsList ) {
      wLength += word.length
      wCaps += isCapitalized(word)
    }
    ( wLength.result(), wCaps.result() )
  }

  override def tearDown() {
    // clean up after yourself if required
  }
  
}

