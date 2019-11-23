/**
 * Main.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa4

import cse250.objects.Tree

object Main {
  def main(args: Array[String]): Unit = {
    val heapArray = Array(10,5,4,3,1,2,0,-2,-4)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    println(heapTree)

    println(s"Heap as array: ${heapArray.toSeq}")
    println(s"Heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(heapTree).toSeq}")

    for (i <- heapArray.indices) {
      val valueAtIndex = TreeUtilities.applyTree(heapTree,i)
      println(s"Expected: ${Some(heapArray(i))}, Got: ${valueAtIndex}")
    }
    println(s"Index -1: ${TreeUtilities.applyTree(heapTree,-1)}")
    println(s"Index ${heapArray.length}: ${TreeUtilities.applyTree(heapTree,heapArray.length)}")

    // Change 5 to -1 in heap.
    var updatedHeapTree = TreeUtilities.updateHeap(heapTree,1,-1)
    println(s"Heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(heapTree).toSeq}")
    println(s"Updated heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(updatedHeapTree).toSeq}")
    // Change 10 to -1 in heap.
    updatedHeapTree = TreeUtilities.updateHeap(heapTree,0,-1)
    println(s"Heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(heapTree).toSeq}")
    println(s"Updated heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(updatedHeapTree).toSeq}")
    // Change 4 to -1 in heap.
    updatedHeapTree = TreeUtilities.updateHeap(heapTree,2,-1)
    println(s"Heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(heapTree).toSeq}")
    println(s"Updated heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(updatedHeapTree).toSeq}")
    // Change 0 to 20 in heap.
    updatedHeapTree = TreeUtilities.updateHeap(heapTree,6,20)
    println(s"Heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(heapTree).toSeq}")
    println(s"Updated heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(updatedHeapTree).toSeq}")
    // Change 1 to 20 in heap.
    updatedHeapTree = TreeUtilities.updateHeap(heapTree,4,20)
    println(s"Heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(heapTree).toSeq}")
    println(s"Updated heap as tree:  ${TreeUtilities.flattenHeapTreeToHeapArray(updatedHeapTree).toSeq}")

    println(s"Tree is a valid heap: ${TreeUtilities.isValidBinaryHeap(heapTree)}")
  }
}

