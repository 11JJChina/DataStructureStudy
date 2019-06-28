package com.zhang

import scala.util.control.Breaks._

class Node(var data: Int, var next: Option[Node])

class SinglyLinkedList(var headOpt: Option[Node]) {

  def this() = this(None)

  def findByValue(value: Int): Option[Node] = {
    headOpt.flatMap(head => {
      var node = head
      while (!node.data.equals(value) && node.next.isDefined) {
        node = node.next.get
      }

      if (node.data.equals(value)) {
        return Some(node)
      } else {

        return None
      }
    })
  }

  def insertToHead(value: Int): Unit = {
    val newNode = new Node(value, None)
    insertToHead(newNode)
  }

  def insertToHead(newNode: Node): Unit = {
    headOpt match {
      case None =>
        headOpt = Some(newNode)
      case Some(head) =>
        newNode.next = Some(head)
        headOpt = Some(newNode)
    }
  }

  def insertToTail(value: Int): Unit = {
    val newNode = new Node(value, None)
    insertToTail(newNode)
  }

  def insertToTail(newNode: Node): Unit = {
    headOpt match {
      case None =>
        headOpt = Some(newNode)
      case Some(head) =>
        var node = head
        while (node.next.isDefined) {
          node = node.next.get
        }
        node.next = Some(newNode)
    }
  }

  def insertAfter(exitNode: Node, value: Int): Unit = {
    val newNode = new Node(value, None)
    insertAfter(exitNode, newNode)
  }

  def insertAfter(exitNode: Node, newNode: Node): Unit = {
    exitNode.next match {
      case None =>
        newNode.next = None
        exitNode.next = Some(newNode)
      case Some(next) =>
        newNode.next = Some(next)
        exitNode.next = Some(newNode)
    }
  }

  def insertBefore(existNode: Node, value: Int): Unit = {
    val newNode = new Node(value, None)
    insertBefore(existNode, newNode)
  }

  def insertBefore(existNode: Node, newNode: Node): Unit = {
    headOpt match {
      case None =>
        throw new IllegalStateException("head node should not be None")
      case Some(head) =>
        if (existNode.equals(head)) {
          insertToHead(newNode)
        }
        var node = head
        while (node.next.isDefined && !node.next.get.equals(existNode)) {
          node = node.next.get
        }

        if (node.next.isEmpty) {
          throw new IllegalArgumentException("existNode " + existNode + " does not exist in this chain")
        }

        newNode.next = node.next
        node.next = Some(newNode)
    }
  }

  def deleteByNode(node: Node): Unit = {
    headOpt.map(head => {
      if (head.equals(node)) {
        //deleting head
        headOpt = node.next
      } else {
        var p: Node = head
        while (p.next.isDefined && !p.next.get.equals(node)) {
          p = p.next.get
        }

        if (p.next.isEmpty) {
          throw new IllegalArgumentException("could not find given node")
        }
        p.next = node.next
      }

    })
  }

  def inverseLink(node: Node): Node = {
    if (headOpt.isEmpty) {
      throw new IllegalArgumentException("list is empty")
    }

    var pre: Option[Node] = None
    var next: Option[Node] = None
    var current: Option[Node] = headOpt

    while (current.isDefined && !current.get.equals(node)) {
      next = current.get.next
      current.get.next = pre

      pre = current
      current = next
    }

    current.get.next = pre

    current.get

  }

  def isPalindrome(): Boolean = {
    headOpt match {
      case None => false
      case Some(head) =>
        var p: Node = head
        var q: Node = head

        if (p.next.isEmpty) {
          return true
        }

        //start to find middle of the chain
        while (q.next.isDefined && q.next.get.next.isDefined) {
          p = p.next.get
          q = q.next.get.next.get
        }
        var leftLink: Option[Node] = None
        var rightLink: Option[Node] = None
        q.next match {
          case None =>
            //p is in the middle of an odd numbers of chain
            rightLink = p.next
            leftLink = inverseLink(p).next
          case Some(_) =>
            //p and p.next is in the middle of the even numbers of chain
            rightLink = p.next
            leftLink = Some(inverseLink(p))
        }

        compareLinkedNodes(leftLink, rightLink)
    }
  }

  def compareLinkedNodes(leftLink: Option[Node], rightLink: Option[Node]): Boolean = {
    var left = leftLink
    var right = rightLink

    breakable {
      while (left.isDefined && right.isDefined) {
        if (!left.get.data.equals(right.get.data)) {
          break
        }
        left = left.get.next
        right = right.get.next
      }
    }
    //make sure we have loop until the end of the chain
    left.isEmpty && right.isEmpty
  }

  def mkString(): String = {
    headOpt.map(head => {
      var node = head
      val result = new StringBuilder

      while (node.next.isDefined) {
        result.append(node.data)
        node = node.next.get
      }
      result.append(node.data)

      result.mkString
    }).getOrElse("")
  }

}
