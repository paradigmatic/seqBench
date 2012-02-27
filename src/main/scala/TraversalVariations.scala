package org.example

import annotation.tailrec
import com.google.caliper.Param


class TraversalVariations extends SimpleScalaBenchmark {
  
  @Param(Array("10", "100" ))
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
		   wcapsReassign = word(0).isUpper :: wcapsReassign
		 }
    (wlengthsReassign.head, wcapsReassign.head)
  }

 def timeBuffer(reps: Int) = repeat(reps) {
   import collection.mutable._
   val wlengthsBuffer = ArrayBuffer[Int]()
   val wcapsBuffer = ArrayBuffer[Boolean]()
   words.foreach { word =>
     wlengthsBuffer.append(word.length)
		  wcapsBuffer.append(word(0).isUpper)
		}
   ( wlengthsBuffer.head, wcapsBuffer.head )
 }

  def timeFixedSizeArray(reps: Int) = repeat(reps) {

    val wlengthsArray2 = Array.fill(words.length)(0)
    val wcapsArray2 = Array.fill(words.length)(false)
    var index = 0
    words.foreach { word =>
      wlengthsArray2(index) = word.length
		   wcapsArray2(index) = word(0).isUpper
		   index += 1
		 }
    ( wlengthsArray2.head, wcapsArray2.head )
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
	    lengthCapRecurHelp(tail, head.length :: lengths, head(0).isUpper :: caps)
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

