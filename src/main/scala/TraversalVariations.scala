package org.example

import annotation.tailrec
import com.google.caliper.Param


class TraversalVariations extends SimpleScalaBenchmark {
  
  @Param(Array("1", "10", "100", "1000" ))
  val length: Int = 0

  val wordLength = 8 
  val letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".map(_.toString)
  val rng = new util.Random

  def randomWord =  {
    var str = ""
    while( str.length < wordLength ) {
      str += letters( rng.nextInt( letters.size ) )
    }
    str
  }
  
  
  val randomStream = {
    def drawWord(): Stream[String] =
      Stream.cons( randomWord, drawWord() )
    drawWord()
  }

  var words: List[String] = _
  
  override def setUp() {
    words = randomStream.take(length).toList
  }

  def isCapitalized( s: String ) =
    java.lang.Character.isUpperCase(s.charAt(0))
  
  def timeFunctional(reps: Int) = repeat(reps) {
    val (wlengthsMapUnzip, wcapsMapUnzip) =
      words.map(word => (word.length, word(0).isUpper)).unzip
    (wlengthsMapUnzip.head,wcapsMapUnzip.head) 
  }

  def timeReassign(reps: Int) = repeat(reps) {
    var wlengthsReassign = List[Int]()
    var wcapsReassign = List[Boolean]()
    words.foreach { word =>
      wlengthsReassign = word.length :: wlengthsReassign
		   wcapsReassign = isCapitalized(word) :: wcapsReassign
		 }
    (wlengthsReassign.head, wcapsReassign.head)
  }

 def timeBuffer(reps: Int) = repeat(reps) {
   import collection.mutable._
   val wlengthsBuffer = ArrayBuffer[Int]()
   val wcapsBuffer = ArrayBuffer[Boolean]()
   words.foreach { word =>
     wlengthsBuffer.append(word.length)
		  wcapsBuffer.append(isCapitalized(word))
		}
   ( wlengthsBuffer.head, wcapsBuffer.head )
 }

  def timeFixedSizeArray(reps: Int) = repeat(reps) {

    val wlengthsArray2 = Array.fill(words.length)(0)
    val wcapsArray2 = Array.fill(words.length)(false)
    var index = 0
    words.foreach { word =>
      wlengthsArray2(index) = word.length
		   wcapsArray2(index) = isCapitalized(word)
		   index += 1
		 }
    ( wlengthsArray2.head, wcapsArray2.head )
  }

  def timeOldSchool(reps: Int) = repeat(reps) {
    val n = words.length
    val lengthAry = Array.ofDim[Int](n)
    val capAry = Array.ofDim[Boolean](n)
    var i = 0
    val it = words.iterator
    while( it.hasNext ) {
      val w = it.next
      lengthAry(i) = w.length
      capAry(i) = isCapitalized(w)
      i += 1
    }
    ( lengthAry.head, capAry.head )
  }


  def timeRecurs(reps: Int) = repeat(reps) {
    def lengthCapRecurWrap(inputWords: List[String]): (List[Int], List[Boolean]) = {
      
      // This function is hidden from code that doesn't
      def lengthCapRecurHelp(
	inputWords: List[String],
	lengths: List[Int],
	caps: List[Boolean]): (List[Int], List[Boolean]) = inputWords match {
	case Nil =>
	  (lengths, caps)
	  case head :: tail =>
	    lengthCapRecurHelp(tail, head.length :: lengths, isCapitalized(head) :: caps)
      }
      val (l,c) = lengthCapRecurHelp(words, List[Int](), List[Boolean]())
      (l.reverse, c.reverse)
    }
    val (wlengthsRecurWrap, wcapsRecurWrap) = lengthCapRecurWrap(words)
    (wlengthsRecurWrap.head, wcapsRecurWrap.head)
  }

  
  
  override def tearDown() {
    // clean up after yourself if required
  }
  
}

