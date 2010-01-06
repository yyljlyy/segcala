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

    /*Dict.addWord("研究")
    Dict.addWord("生命")
    Dict.addWord("科学")
    Dict.addWord("科")
    Dict.addWord("研究生")
    Dict.addWord("命", 1234)*/

    val words = MMSeg.seg(new TextFragment("研究生命科学"))
    words.foreach(w => println(w.value))
  }
}
