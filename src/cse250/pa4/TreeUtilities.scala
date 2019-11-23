/**
 * cse250.pa4.TreeUtilities.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: kendinge
 * Person#: 50264521
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa4

import cse250.objects.{Empty, Node, Tree}

import scala.collection.mutable
import scala.reflect.ClassTag

object TreeUtilities {
  def buildHeapTreeFromHeapArray[A](heapArray: Array[A]): Tree[A] = {
    var heapTemp: mutable.ArrayBuffer[A] = mutable.ArrayBuffer()
    var OutputNode: Node[A] = null

    if (heapArray.length > 1) {
      var root: A = heapArray(0)
      heapArray.foreach(heapTemp.append)
      heapTemp = heapTemp.reverse
      heapTemp.remove(heapTemp.length - 1)

      var depth = heapTemp.length / 2
      //println("Depth: " + depth)

      var nodesFromFull: Int = ((Math.pow(2, depth) - 1) - (heapTemp.length + 1)).toInt
      var nodeArray: mutable.ArrayBuffer[Node[A]] = mutable.ArrayBuffer()

      var heapArrayTemp: mutable.ArrayBuffer[mutable.ArrayBuffer[Node[A]]] = mutable.ArrayBuffer()
      if(depth > 2) {
        for (i <- depth to 2 by -1) {
          var bottomRow: Boolean = false
          var secBotRow: Boolean = false
          var nodesToSkip: Int = 0
          var numNodesinRow: Int = ((Math.pow(2, i) - 1) - (Math.pow(2, i - 1) - 1)).toInt

          if (i == depth) {
            numNodesinRow -= nodesFromFull
            bottomRow = true
          }
          else if (i == (depth - 1)) {
            nodesToSkip = nodesFromFull / 2
            secBotRow = true
          }

          //println("Row " + i + " needs " + numNodesinRow + " Nodes")
          //println("nodes to skip: " + nodesToSkip)

          var rowTemp: mutable.ArrayBuffer[Node[A]] = mutable.ArrayBuffer()

          for (k <- 1 to numNodesinRow) {
            //println("Node " + k + ": " + heapTemp(0))
            //println("   " + (2 * (k - nodesToSkip) - 2))
            //println("   " + (2 * (k - nodesToSkip) - 1))
            var rowNumBefore: Int = depth - i - 1
            //println("   Row from Bot " + rowNumBefore)

            if (bottomRow) {
              val newNode: Node[A] = new Node[A](heapTemp(0), Empty, Empty)
              rowTemp.append(newNode)
            }
            else if (secBotRow && k <= nodesToSkip) {
              val newNode: Node[A] = new Node[A](heapTemp(0), Empty, Empty)
              rowTemp.append(newNode)
            }
            else if (secBotRow && k > nodesToSkip) {
              //println("   " + heapArrayTemp(rowNumBefore)((2 * (k - nodesToSkip) - 1)))
              val newNode: Node[A] = new Node[A](heapTemp(0), heapArrayTemp(rowNumBefore)((2 * (k - nodesToSkip) - 1)), heapArrayTemp(rowNumBefore)((2 * (k - nodesToSkip) - 2)))
              rowTemp.append(newNode)
            }
            else {
              val newNode: Node[A] = new Node[A](heapTemp(0), heapArrayTemp(rowNumBefore)((2 * (k) - 1)), heapArrayTemp(rowNumBefore)((2 * (k) - 2)))
              rowTemp.append(newNode)
            }
            heapTemp.remove(0)
          }
          heapArrayTemp.append(rowTemp)
          //println(rowTemp)
        }
        //println(heapArrayTemp.length)
        OutputNode = new Node[A](root, heapArrayTemp(heapArrayTemp.length - 1)(1), heapArrayTemp(heapArrayTemp.length - 1)(0))
        //println(OutputNode)
        return OutputNode
      }
      else if(depth == 2) {
        //println(heapArray.length)
        for (i <- depth to 1 by -1) {
          var bottomRow: Boolean = false
          var secBotRow: Boolean = false
          var nodesToSkip: Int = 0
          var numNodesinRow: Int = ((Math.pow(2, i) - 1) - (Math.pow(2, i - 1) - 1)).toInt

          if (i == depth) {
            numNodesinRow -= nodesFromFull
            bottomRow = true
          }
          else if (i == (depth - 1)) {
            nodesToSkip = nodesFromFull / 2
            secBotRow = true
          }

          //println("Row " + i + " needs " + numNodesinRow + " Nodes")
          //println("nodes to skip: " + nodesToSkip)

          var rowTemp: mutable.ArrayBuffer[Node[A]] = mutable.ArrayBuffer()

          for (k <- 1 to numNodesinRow) {
            //println("Node " + k + ": " + heapTemp(0))
            //println("   " + (2 * (k - nodesToSkip) - 2))
            //println("   " + (2 * (k - nodesToSkip) - 1))
            var rowNumBefore: Int = depth - i - 1
            //println("   Row from Bot " + rowNumBefore)

            if (bottomRow) {
              val newNode: Node[A] = new Node[A](heapTemp(0), Empty, Empty)
              rowTemp.append(newNode)
            }
            else if ((secBotRow && k <= nodesToSkip) || (secBotRow && heapArrayTemp(rowNumBefore).isEmpty)) {
              val newNode: Node[A] = new Node[A](heapTemp(0), Empty, Empty)
              rowTemp.append(newNode)
            }
            else if (secBotRow && k > nodesToSkip) {
              if (!heapArrayTemp(rowNumBefore).isEmpty) {
                println(heapArrayTemp(rowNumBefore)((2 * (k - nodesToSkip) - 1)))
                val newNode: Node[A] = new Node[A](heapTemp(0), heapArrayTemp(rowNumBefore)((2 * (k - nodesToSkip) - 1)), heapArrayTemp(rowNumBefore)((2 * (k - nodesToSkip) - 2)))
                rowTemp.append(newNode)
              }
              else {
                val newNode: Node[A] = new Node[A](heapTemp(0), Empty, Empty)
                rowTemp.append(newNode)
              }
            }
            else {
              val newNode: Node[A] = new Node[A](heapTemp(0), heapArrayTemp(rowNumBefore)((2 * (k) - 1)), heapArrayTemp(rowNumBefore)((2 * (k) - 2)))
              rowTemp.append(newNode)
            }
            heapTemp.remove(0)

          }
          heapArrayTemp.append(rowTemp)
          //println(rowTemp)
        }
        //println(heapArrayTemp.length)
        OutputNode = new Node[A](root, heapArrayTemp(heapArrayTemp.length - 1)(1), heapArrayTemp(heapArrayTemp.length - 1)(0))
        //println(OutputNode)
        return OutputNode
      }
      else {
        if(heapArray.length == 2){
          OutputNode = new Node[A](root, Node[A](heapArray(1), Empty, Empty), Empty)
          //println(OutputNode)
          return OutputNode
        }
        else {
          OutputNode = new Node[A](root, Node[A](heapArray(1), Empty, Empty), Node[A](heapArray(2), Empty, Empty))
          //println(OutputNode)
          return OutputNode
        }
      }
    }
    else if(heapArray.length == 1) {
      var root: A = heapArray(0)
      OutputNode = new Node[A](root, Empty, Empty)
      OutputNode
    }
    else {
      Empty
    }
  }

  def flattenHeapTreeToHeapArray[A: ClassTag](root: Tree[A]): Array[A] = {
    //println("")
    //println("Start flatten")
    var outputArray: mutable.ArrayBuffer[A] = mutable.ArrayBuffer()
    var nextRow: mutable.ArrayBuffer[Tree[A]] = mutable.ArrayBuffer()
    var end: Boolean = false
    if(root.value.get != Empty) {

      outputArray.append(root.value.get)
      if (root.left.get.value != Empty || root.right.get.value != Empty) {
        if (root.left.get.value != Empty && root.left.get.value != None) {
          nextRow.append(root.left.get)
        }
        if (root.right.get.value != Empty && root.right.get.value != None) {
          nextRow.append(root.right.get)
        }

        if (nextRow.isEmpty) { end = true}
        //println("Output Array " + outputArray)\
        //println(root.right.get.value)
        //println("Second Row is " + nextRow)

        while (!end) {
          var nextRowTemp: mutable.ArrayBuffer[Tree[A]] = mutable.ArrayBuffer()
          var lastRow: Boolean = true

          for (i <- nextRow) {
            //println("    Current Node " + i)
            if (i.left.get != Empty) {
              lastRow = false
            }
            if (i.right.get != Empty) {
              lastRow = false
            }
            //println("  " + lastRow)

            if (!lastRow) {
              if (i.left.get != Empty) {
                nextRowTemp.append(i.left.get)
              }
              if (i.right.get != Empty) {
                nextRowTemp.append(i.right.get)
              }
            }
            if (lastRow) {
              end = true
            }

            outputArray.append(i.value.get)
            //println("Next Row is " + nextRowTemp)
          }
          nextRow = nextRowTemp
          //println("Output Array " + outputArray)
        }
      }
      outputArray.toArray
    }
    else {
      Array()
    }
  }

  def isValidBinaryHeap[A](root: Tree[A])(implicit comp: Ordering[A]): Boolean = {
    var nextRow: mutable.ArrayBuffer[Tree[A]] = mutable.ArrayBuffer()

    if (root.left.get.value != Empty && root.left.get.value != None) {
      nextRow.append(root.left.get)
    }
    if (root.right.get.value != Empty && root.right.get.value != None) {
      nextRow.append(root.right.get)
    }
    //println(nextRow)
    var end: Boolean = false
    var valid: Boolean = true
    if (nextRow.isEmpty) { end = true}

    while(!end) {
      var nextRowTemp: mutable.ArrayBuffer[Tree[A]] = mutable.ArrayBuffer()
      var lastRow: Boolean = true

      for(i <- nextRow) {
        //println("    Current Node " + i)
        var validtoCompare: Boolean = true
        if(i.left.get == Empty) {
          validtoCompare = false
        }
        if(i.right.get == Empty) {
          validtoCompare = false
        }
        if(i.left.get != Empty) {
          lastRow = false
        }
        if(i.right.get != Empty) {
          lastRow = false
        }

        if(validtoCompare) {
          val ret: Int = 0
          val ret2: Int = 0

          if(i.left.get != Empty) {
            val ret: Int = comp.compare(i.left.get.value.get, i.value.get)
            if(ret > 0) {
              valid = false
            }
          }
          if(i.right.get != Empty) {
            val ret2: Int = comp.compare(i.right.get.value.get, i.value.get)
            if(ret2 > 0) {
              valid = false
            }
          }
        }

        //println("  " + lastRow)

        if(!lastRow) {
          if(i.left.get != Empty) { nextRowTemp.append(i.left.get) }
          if(i.right.get != Empty) { nextRowTemp.append(i.right.get) }
        }
        if(lastRow) { end = true}
      }
      //println("Next Row is " + nextRowTemp)
      nextRow = nextRowTemp
      //println("Output Array " + outputArray)
    }

    valid
  }

  def applyTree[A](root: Tree[A], index: Int): Option[A] = {
    var output: Option[A] = None
    var nextRow: mutable.ArrayBuffer[Tree[A]] = mutable.ArrayBuffer()
    var end: Boolean = false
    var valid: Boolean = true
    var indexCount: Int = 0;
    //println("looking for index " + index)
    if(index == 0 ) { output = root.value}

    if (root.left.get.value != Empty || root.right.get.value != Empty) {
      if (root.left.get.value != Empty) {nextRow.append(root.left.get)}
      if (root.right.get.value != Empty) {nextRow.append(root.right.get)}

      while (!end) {
        var nextRowTemp: mutable.ArrayBuffer[Tree[A]] = mutable.ArrayBuffer()
        var lastRow: Boolean = true

        for (i <- nextRow) {
          indexCount += 1;
          //println("    Current Index " + indexCount)
          if (indexCount == index) {
            output = i.value
            end == true
          }
          if (i.left.get != Empty) {
            lastRow = false
          }
          if (i.right.get != Empty) {
            lastRow = false
          }

          if (!lastRow) {
            if (i.left.get != Empty) {
              nextRowTemp.append(i.left.get)
            }
            if (i.right.get != Empty) {
              nextRowTemp.append(i.right.get)
            }
          }
          if (lastRow) {
            end = true
          }
          //println("Next Row is " + nextRowTemp)
        }
        nextRow = nextRowTemp
        //println("Output Array " + outputArray)
      }
    }
    output
  }

  def updateHeap[A](root: Tree[A], index: Int, elem: A)(implicit comp: Ordering[A]): Tree[A] = {
    root
  }
}
