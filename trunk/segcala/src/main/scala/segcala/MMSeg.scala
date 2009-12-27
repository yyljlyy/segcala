package segcala

import java.util.Properties
import com.google.inject.name.Names
import com.google.inject.{Module, Injector, Guice, AbstractModule}

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

    val properties = new Properties()
    val loader = classOf[SegModule].getClassLoader
    properties.load(loader.getResource("wordseg.properties").openStream)
    Names.bindProperties(binder(), properties);
    
  }

}


object SegTest {
  def main(args: Array[String]) {

    val inj:Injector = Guice.createInjector(new SegModule())
    inj.getInstance(classOf[Dict])

    Dict.addWord("abcd")
    Dict.addWord("abc")
    Dict.addWord("de")
    Dict.addWord("defg")
    val words = MMSeg.seg(new TextFragment(List('a', 'b', 'c', 'd', 'e'), 0))
    words.foreach(w => println(w.value))
  }
}
