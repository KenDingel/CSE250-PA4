/**
 * cse250.pa4.TreeUtilitiesTest.scala
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

import cse250.pa4.TreeUtilities
import org.scalatest.{BeforeAndAfter, FlatSpec}


class TreeUtilitiesTest extends FlatSpec with BeforeAndAfter {
  behavior of "buildHeapTreeFromHeapArray:"
  // Tests for buildHeapTreeFromHeapArray
  it should "check root" in {
    val heapArray = Array(10,5,4,3,1,2,0,-2,-4)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree.value.get == heapArray(0))
  }
  it should "check left of root" in {
    val heapArray = Array(10,5,4,3,1,2,0,-2,-4)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree.left.get.value.get == heapArray(1))
  }
  it should "check right of root" in {
    val heapArray = Array(10,5,4,3,1,2,0,-2,-4)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree.right.get.value.get == heapArray(2))
  }
  it should "Empty Array" in {
    val heapArray = Array()
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree == cse250.objects.Empty)
  }
  it should "Single Elem Array" in {
    val heapArray = Array(1)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree.value.get == heapArray(0))
    assert(heapTree.left.get == cse250.objects.Empty)
    assert(heapTree.right.get == cse250.objects.Empty)
  }
  it should "Filled Levels Array" in {
    val heapArray = Array(10,5,4)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree.value.get == heapArray(0))
    assert(heapTree.left.get.value.get == 5)
    assert(heapTree.right.get.value.get == 4)
  }
  it should "Partial Filled Levels Array" in {
    val heapArray = Array(10, 5, 4, 6, 7)
    val heapTree = TreeUtilities.buildHeapTreeFromHeapArray(heapArray)
    assert(heapTree.value.get == heapArray(0))
    assert(heapTree.left.get.left.get.value.get == 6)
    assert(heapTree.left.get.right.get.value.get == 7)
  }


  // ----
  behavior of "flattenHeapTreeToHeapArray:"
  // Tests for flattenHeapTreeToHeapArray
  it should "check root" in {
    val heapTree = cse250.objects.Node(10,cse250.objects.Node(5,cse250.objects.Node(3,cse250.objects.Node(-2,cse250.objects.Empty,cse250.objects.Empty),cse250.objects.Node(-4,cse250.objects.Empty,cse250.objects.Empty)),cse250.objects.Node(1,cse250.objects.Empty,cse250.objects.Empty)),cse250.objects.Node(4,cse250.objects.Node(2,cse250.objects.Empty,cse250.objects.Empty),cse250.objects.Node(0,cse250.objects.Empty,cse250.objects.Empty)))
    val heapArray = TreeUtilities.flattenHeapTreeToHeapArray(heapTree)
    assert(heapTree.value.get == heapArray(0))
  }
  it should "check left of root" in {
    val heapTree = cse250.objects.Node(10,cse250.objects.Node(5,cse250.objects.Node(3,cse250.objects.Node(-2,cse250.objects.Empty,cse250.objects.Empty),cse250.objects.Node(-4,cse250.objects.Empty,cse250.objects.Empty)),cse250.objects.Node(1,cse250.objects.Empty,cse250.objects.Empty)),cse250.objects.Node(4,cse250.objects.Node(2,cse250.objects.Empty,cse250.objects.Empty),cse250.objects.Node(0,cse250.objects.Empty,cse250.objects.Empty)))
    val heapArray = TreeUtilities.flattenHeapTreeToHeapArray(heapTree)
    assert(heapTree.left.get.value.get == heapArray(1))
  }
  it should "check right of root" in {
    val heapTree = cse250.objects.Node(10,cse250.objects.Node(5,cse250.objects.Node(3,cse250.objects.Node(-2,cse250.objects.Empty,cse250.objects.Empty),cse250.objects.Node(-4,cse250.objects.Empty,cse250.objects.Empty)),cse250.objects.Node(1,cse250.objects.Empty,cse250.objects.Empty)),cse250.objects.Node(4,cse250.objects.Node(2,cse250.objects.Empty,cse250.objects.Empty),cse250.objects.Node(0,cse250.objects.Empty,cse250.objects.Empty)))
    val heapArray = TreeUtilities.flattenHeapTreeToHeapArray(heapTree)
    assert(heapTree.right.get.value.get == heapArray(2))
  }
  it should "Empty Array" in {
    val heapTree = cse250.objects.Node(cse250.objects.Empty, cse250.objects.Empty, cse250.objects.Empty)
    val heapArray = TreeUtilities.flattenHeapTreeToHeapArray(heapTree)
    assert(heapArray.isEmpty)
  }
  it should "Single Elem Array" in {
    val heapTree = cse250.objects.Node(10, cse250.objects.Empty, cse250.objects.Empty)
    val heapArray = TreeUtilities.flattenHeapTreeToHeapArray(heapTree)
    assert(heapArray(0) == 10)
    assert(heapArray.length == 1)
  }



  // ----
  behavior of "isValidBinaryHeap:"
  // Tests for flattenHeapTreeToHeapArray
  it should "Single Elem Heap" in {
    val heapTree = cse250.objects.Node(10, cse250.objects.Empty, cse250.objects.Empty)
    val validHeap = TreeUtilities.isValidBinaryHeap(heapTree)
    assert(validHeap == true)
  }
  it should "Non valid" in {
    val heapTree = cse250.objects.Node(10, cse250.objects.Node(15, cse250.objects.Empty, cse250.objects.Empty), cse250.objects.Node(25, cse250.objects.Empty, cse250.objects.Empty))
    val validHeap = TreeUtilities.isValidBinaryHeap(heapTree)
    assert(validHeap == false)
  }







  // ----
  behavior of "applyTree:"
  // Tests for flattenHeapTreeToHeapArray
  it should "work" in {

  }
  // ----
  behavior of "updateHeap:"
  // Tests for flattenHeapTreeToHeapArray
  it should "work" in {

  }
  // ----

}

