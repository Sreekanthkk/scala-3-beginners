package com.rockthejvm.practice

import java.util.NoSuchElementException
import scala.annotation.tailrec

abstract class LListGeneric[A] {
  def head:A
  def tail:LListGeneric[A]
  def isEmpty:Boolean
  def add(num:A):LListGeneric[A]=new NonEmptyGeneric[A](num,this)
}

class EmptyGeneric[A] extends LListGeneric[A]{
  def head=throw new NoSuchElementException
  def tail=throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = s"[]"
}

class NonEmptyGeneric[A](override val head:A,override val tail:LListGeneric[A]) extends LListGeneric[A] {
  def isEmpty=false
  override def toString: String = {
    @tailrec
    def toDisplayContent(theList:LListGeneric[A],acc:String):String=
      if(theList.isEmpty) s"$acc]"
      else toDisplayContent(theList.tail,s"$acc,${theList.head}")
    toDisplayContent(this.tail,s"[${this.head}")
  }
}


object LListTestGeneric{
  def main(args: Array[String]): Unit = {

    //val myIntLinkedList=new Empty[Int]
      //val myLinkedList=new NonEmpty(1,new NonEmpty(2,new NonEmpty(3,new Empty)))
      //val myLinkedList_v2=myLinkedList.add(4).add(5).add(6)
      //val myLinkedList_v2=myIntLinkedList.add(4).add(5).add(6)
      //println(myLinkedList_v2.toString)
      val myIntegerLinkedList=new EmptyGeneric[Int]
      println(myIntegerLinkedList.add(1).add(2).add(3).add(4))
      val myStringLinkedList=new EmptyGeneric[String]
      println(myStringLinkedList.add("One").add("Two").add("Three"))
      val myStringLinkedList_v2=new NonEmptyGeneric[String]("Six",new NonEmptyGeneric("Seven",new NonEmptyGeneric("Eight",new EmptyGeneric[String])))
      println(myStringLinkedList_v2)
  }
}