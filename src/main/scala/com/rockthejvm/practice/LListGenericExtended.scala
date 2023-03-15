package com.rockthejvm.practice

import java.util.NoSuchElementException
import scala.annotation.tailrec

//trait Predicate[A]{
//  def test(value:A):Boolean
//}
//trait Transformer[A,B]{
//  def transform(value:A):B
//}
//
//class Doubler extends Transformer[Int,Int]{
//  def transform(value:Int):Int=value*2
//}
//
//class DoublerList extends Transformer[Int,LListGenericExtended[Int]]{
//  override def transform(value: Int): LListGenericExtended[Int] = new NonEmptyGenericExtended[Int](value,new NonEmptyGenericExtended[Int](value+1,new EmptyGenericExtended[Int]))
//}
//
//class EvenPredicate extends Predicate[Int]{
//  override def test(value: Int): Boolean = value % 2 == 0
//}
//
//class SamplePredicate extends Predicate[Int]{
//  override def test(value: Int): Boolean = value % 16 == 0
//}

class MyException extends RuntimeException{
  override def getMessage="No element found!"
}

abstract class LListGenericExtended[A] {
  def head:A
  def tail:LListGenericExtended[A]
  def isEmpty:Boolean
  def add(num:A):LListGenericExtended[A]=new NonEmptyGenericExtended[A](num,this)
  def map[B](transformer:A=>B):LListGenericExtended[B]
  def flatMap[B](transformer:A=>LListGenericExtended[B]):LListGenericExtended[B]
  def filter(predicate: A=>Boolean):LListGenericExtended[A]
  def find(aList:LListGenericExtended[A],predicate: A=>Boolean):A
  infix def ++(anotherList:LListGenericExtended[A]):LListGenericExtended[A]
}


class EmptyGenericExtended[A] extends LListGenericExtended[A]{
  def head=throw new NoSuchElementException
  def tail=throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def map[B](transformer: A=>B):LListGenericExtended[B]=new EmptyGenericExtended[B]

  override def filter(predicate: A=>Boolean): LListGenericExtended[A] = new EmptyGenericExtended[A]
  override def toString: String = s"[]"
  infix def ++(anotherList:LListGenericExtended[A]):LListGenericExtended[A]=anotherList
  def flatMap[B](transformer: A=>LListGenericExtended[B]):LListGenericExtended[B]=new EmptyGenericExtended[B]

  override def find(aList: LListGenericExtended[A], predicate: A=>Boolean): A =
    throw new MyException


}

class NonEmptyGenericExtended[A](override val head:A,override val tail:LListGenericExtended[A]) extends LListGenericExtended[A] {
  def isEmpty=false
  override def map[B](transformer: A=>B):LListGenericExtended[B]=new NonEmptyGenericExtended[B](transformer(head),tail.map(transformer))
  override def filter(predicate: A=>Boolean): LListGenericExtended[A] = {
    if(predicate(head)) new NonEmptyGenericExtended[A](head,tail.filter(predicate))
    else tail.filter(predicate)
  }

  infix def ++(anotherList:LListGenericExtended[A]):LListGenericExtended[A]= new NonEmptyGenericExtended[A](head,tail++anotherList)

  override def flatMap[B](transformer: A=> LListGenericExtended[B]): LListGenericExtended[B] = transformer(head)++tail.flatMap(transformer)

  override def find(aList: LListGenericExtended[A], predicate:A=>Boolean): A = {
    def findHelper(remainder:LListGenericExtended[A]):A={
        if predicate(remainder.head) then head
        else if remainder.tail.isEmpty then throw new MyException
        else findHelper(remainder.tail)
    }
    findHelper(aList)

  }
  override def toString: String = {
    @tailrec
    def toDisplayContent(theList:LListGenericExtended[A],acc:String):String=
      if(theList.isEmpty) s"$acc]"
      else toDisplayContent(theList.tail,s"$acc,${theList.head}")
    toDisplayContent(this.tail,s"[${this.head}")
  }

}


object LListTestGenericExtended{
  def main(args: Array[String]): Unit = {

    //val myIntLinkedList=new Empty[Int]
      //val myLinkedList=new NonEmpty(1,new NonEmpty(2,new NonEmpty(3,new Empty)))
      //val myLinkedList_v2=myLinkedList.add(4).add(5).add(6)
      //val myLinkedList_v2=myIntLinkedList.add(4).add(5).add(6)
      //println(myLinkedList_v2.toString)

      //val myStringLinkedList=new EmptyGenericExtended[String]
      //println(myStringLinkedList.add("One").add("Two").add("Three"))
      //val myStringLinkedList_v2=new NonEmptyGenericExtended[String]("Six",new NonEmptyGenericExtended("Seven",new NonEmptyGenericExtended("Eight",new EmptyGenericExtended[String])))
      //println(myStringLinkedList_v2)

//    val doubler=new Transformer[Int,Int] {
//      override def tranform(value: Int): Int = value *2
//    }

    val myIntegerLinkedList=new EmptyGenericExtended[Int]
    val myIntegerLinkedList_v2=myIntegerLinkedList.add(1).add(2).add(3).add(4)
    val doubler:(Int=>Int)=_*2
    val doubleList:(Int=>LListGenericExtended[Int])=a=>new NonEmptyGenericExtended[Int](a,new NonEmptyGenericExtended[Int](a+1,new EmptyGenericExtended[Int]))
    val predicateEven:(Int=>Boolean)=_%2==0
    val predicateSample:(Int=>Boolean)=_%16==0
    val myIntegerLinkedList_v3=myIntegerLinkedList_v2.map(doubler)
    val myIntegerLinkedList_v4=myIntegerLinkedList_v2.flatMap(doubleList)
    println("Concatenated: "+(myIntegerLinkedList_v2 ++ myIntegerLinkedList_v3))
    println("v2: "+myIntegerLinkedList_v2)
    println("v2.map: "+myIntegerLinkedList_v2.map(doubler))

    println("v2.map(doubleList): "+myIntegerLinkedList_v2.map(doubleList))
    println("v2.flatMap(doubleList): "+myIntegerLinkedList_v2.flatMap(doubleList))

    println("v2.filter: "+myIntegerLinkedList_v2.filter(predicateEven))
    println("Find: "+myIntegerLinkedList_v4.find(myIntegerLinkedList_v4,predicateSample))
  }
}