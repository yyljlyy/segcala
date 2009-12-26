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

    var q: Queue[Array[Int]] = new Queue[Array[Int]]()
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

  private def findMatches(q: Queue[Array[Int]], fragment: List[Char], offset: Int, wordNo: Int) {

    if (wordNo < Constants.MAX_WORD_NO && offset < fragment.length) {
      if (wordNo == 0) q.enqueue(Array(-1, -1))
      val indexes = Dict.findMatchedWordIndexArray(fragment, offset)
      indexes.foreach(i => {
        q.enqueue(Array(offset, i))
        findMatches(q, fragment, i, wordNo + 1)
      })
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