package segcala

import com.google.inject.name.Named
import com.google.inject.Inject

/**
 * Created by IntelliJ IDEA.
 * User: rockmaple
 */

@Inject
class Dict(@Named("wordFiles") val dictFiles: String) {
  Dict.loadDic(dictFiles)
}

object Dict {
  import scala.collection.mutable.Map
  val dictionary = Map[Char, TreeNode]()

  def loadDic(path: String) {

  }

  def addWord(word: String) {
    val l = word.toList
    var opNode = search(l)
    if (opNode == None) {
      val tn = if (l.length == 1) new TreeNode(l.head, 0, true) else new TreeNode(l.head, 0, false)
      dictionary(l.head) = tn
      opNode = Some(tn)
    }

    var node = opNode.get
    if (node.level == l.length - 1) {
      node.leaf = true
    } else {
      for (i <- node.level + 1 until l.length) {
        val tn = if (i == l.length - 1) new TreeNode(l(i), i, true) else new TreeNode(l(i), i, false)
        node.addSubNode(tn)
        node = tn
      }
    }
  }

  def findMatchWords(fragment: List[Char], offset: Int): List[Word] = {
    val c = fragment(offset)
    var opNode = dictionary.get(c)
    var wordList: List[Word] = List()
    if (opNode != None) {
      if (opNode.get.leaf) wordList = new Word(fragment, 0, 1, opNode.get.frequency) :: wordList
      for (i <- offset + 1 until fragment.length) {
        opNode = opNode.get.searchSubNodesForChar(fragment(i))
        if (opNode != None) {
          if (opNode.get.leaf){
            //println("add word: " + fragment.slice(offset, i+1))
            wordList = new Word(fragment, offset, i+1-offset) :: wordList
          }
        }
      }
    }
    wordList
  }

  //查找某词语在树中的位置，返回已存在的公共前缀的最后一个节点。返回None表示首字在词典中也不存在
  def search(l: List[Char]): Option[TreeNode] = {

    def searchTree(n: TreeNode, l: List[Char]): Option[TreeNode] = {
      l.length match {
        case 0 => Some(n)
        case _ => {
          val tn = n.searchSubNodesForChar(l.head)
          tn match {
            case None => Some(n)
            case _ => searchTree(tn.get, l.tail)
          }
        }
      }
    }

    var node = dictionary.get(l.head)
    node match {
      case None => None
      case _ => searchTree(node.get, l.tail)
    }
  }

}

//frequency: Degree of Morphemic Freedom of One-Character, 单字才有
class TreeNode(val c: Char, val level: Int, var leaf: Boolean, var frequency: Int) {
  def this(c: Char, level: Int, leaf: Boolean) = this (c, level, leaf, 0)

  private var subNodes = List[TreeNode]()

  def addSubNode(node: TreeNode) {
    subNodes = node :: subNodes
    //TODO: sort
    //subNodes.sort((n1, n2) => (n1.c - n2.c)<0)
  }

  def searchSubNodesForChar(ch: Char): Option[TreeNode] = {
    //TODO: binary search
    subNodes.find(n => n.c == ch)
  }

}