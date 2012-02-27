package org.example

import com.google.caliper.{Runner => CaliperRunner}

object Runner {

  def main(args: Array[String]) {
 
    CaliperRunner.main(classOf[TraversalVariations], args: _*)
  }
  
}
