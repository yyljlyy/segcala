package segcala

import collection.mutable.Queue

/**
 * Created by IntelliJ IDEA.
 * User: rockmaple
 */

object Constants {
  val MAX_WORD_NO = 3
}

object Algorithm {
  private def createChunks(fragment: List[Char], offset: Int): List[Chunk] = {

    var q: Queue[Word] = new Queue[Word]()
    findMatches(q, fragment, offset, 0)

    List()
  }

  private def applyRules(chunks: List[Chunk]): Chunk = {

    var tmpChunks = Rules.maxMatchRule(chunks)

    if (tmpChunks.length > 1) {
      tmpChunks = Rules.largestAvgWordLenRule(chunks)
      if (tmpChunks.length > 1) {
        tmpChunks = Rules.smallestVarianceRule(chunks)
        if (tmpChunks.length > 1) {
          tmpChunks = Rules.largestSumMorphemicFreedomDegreeRule(chunks)
        }
      }
    }

    tmpChunks(0)
  }

  def findMatches(q: Queue[Word], fragment: List[Char], offset: Int, wordNo: Int) {

    if (wordNo < Constants.MAX_WORD_NO && offset < fragment.length) {
      val words = Dict.findMatchWords(fragment, offset)
      words.foreach(w => {
        q.enqueue(w)
        findMatches(q, fragment, offset + w.length, wordNo + 1)
      })
      if (wordNo == 1) {
        q.enqueue(new Word(List('|')))
      }
    }

  }


  object Rules {
    def largestAvgWordLenRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.averageLength > c2.averageLength) c1 else c2})
      chunks.filter(chunk => (chunk.averageLength == c.averageLength))
    }

    def maxMatchRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.length > c2.length) c1 else c2})
      chunks.filter(chunk => (chunk.length == c.length))
    }

    def smallestVarianceRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.variance < c2.variance) c1 else c2})
      chunks.filter(chunk => (chunk.variance == c.variance))
    }

    def largestSumMorphemicFreedomDegreeRule(chunks: List[Chunk]): List[Chunk] = {
      val c = chunks.reduceLeft((c1, c2) => {if (c1.degreeOfMorphemicFreedom > c2.degreeOfMorphemicFreedom) c1 else c2})
      chunks.filter(chunk => (chunk.degreeOfMorphemicFreedom == c.degreeOfMorphemicFreedom))
    }

  }
}

object AlgoTest{
  def main(args: Array[String]){

    Dict.addWord("abcd")
    Dict.addWord("abc")
    Dict.addWord("de")
    Dict.addWord("defg")

    var q: Queue[Word] = new Queue[Word]()

    Algorithm.findMatches(q, List('a', 'b', 'c', 'd', 'e'), 0, 0)
    println("-- words --")
    var w = q.dequeue
    while( !q.isEmpty ){
      print(w.value)
      print("::")
      w = q.dequeue
    }
  }
}