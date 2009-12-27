package segcala

import com.google.inject.AbstractModule

/**
 * Created by IntelliJ IDEA.
 * User: rockmaple
 */

object MMSeg{

  def seg(fragment: TextFragment): List[Word] = {
      var words:List[Word] = List()
      while(!fragment.isFinish){
        val chunk = Algorithm.seg(fragment)
        chunk.words.foreach(w => words = w::words)
      }
      words
  }

}

class SegModule extends AbstractModule {
  override def configure() {
            
  }
}


object SegTest {
  def main(args: Array[String]) {

    Dict.addWord("abcd")
    Dict.addWord("abc")
    Dict.addWord("de")
    Dict.addWord("defg")
    val words = MMSeg.seg(new TextFragment(List('a', 'b', 'c', 'd', 'e'), 0))
    words.foreach(w => println(w.value))
  }
}
