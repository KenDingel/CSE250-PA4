/**
 * cse250.objects.Tree.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

import cse250.pa4.TreeUtilities

// Based on: https://gist.github.com/dholbrook/2967371
trait Tree[+A] {
  def value: Option[A] = this match {
    case n: Node[A] => Some(n.v)
    case Empty      => None
  }

  def left: Option[Tree[A]] = this match {
    case n: Node[A] => Some(n.l)
    case Empty      => None
  }

  def right: Option[Tree[A]] = this match {
    case n: Node[A] => Some(n.r)
    case Empty      => None
  }
}

case class Node[A](v: A, l: Tree[A], r: Tree[A]) extends Tree[A]
case object Empty extends Tree[Nothing]
