package segcala

import org.junit._
import Assert._

@Test
class AppTest {
  @Test
  def testDict() = {
    Dict.addWord("研究")
    Dict.addWord("生命")
    Dict.addWord("科学")
    Dict.addWord("研究生")
    Dict.addWord("命", 1234)

    val words = Dict.findMatchWords("研究生命科学".toList, 0)

    words.foreach(w => println(w.value))

    assertTrue(true)
  }


}


