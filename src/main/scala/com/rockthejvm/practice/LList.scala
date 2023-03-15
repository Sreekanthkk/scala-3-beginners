package com.rockthejvm.practice

import java.util.NoSuchElementException
import scala.annotation.tailrec

abstract class LList {
  def head:Int
  def tail:LList
  def isEmpty:Boolean
  def add(num:Int):LList=new NonEmpty(num,this)
}

class Empty extends LList{
  def head=throw new NoSuchElementException
  def tail=throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = s"[]"
}

class NonEmpty(override val head:Int,override val tail:LList) extends LList {
  def isEmpty=false
  override def toString: String = {
    @tailrec
    def toDisplayContent(theList:LList,acc:String):String=
      if(theList.isEmpty) s"$acc]"
      else toDisplayContent(theList.tail,s"$acc,${theList.head}")
    toDisplayContent(this.tail,s"[${this.head}")
  }
}


object LListTest{
  def main(args: Array[String]): Unit = {
    //val myLinkedList=new Empty
      val myLinkedList=new NonEmpty(1,new NonEmpty(2,new NonEmpty(3,new Empty)))
      val myLinkedList_v2=myLinkedList.add(4).add(5).add(6)
      println(myLinkedList_v2.toString)
  }
}