package segcala

import com.google.inject.AbstractModule

/**
 * Created by IntelliJ IDEA.
 * User: rockmaple
 */

object MMSeg{

  def seg(textFragment: TextFragment): List[Word] = {
      List()
  }

}

class SegModule extends AbstractModule {
  override def configure() {
            
  }
}
