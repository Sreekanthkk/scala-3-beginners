package com.rockthejvm.part2oop

object OOBasics {

  class Person(val name:String,age:Int,var aFlag:String){
    val allUpper=name.toUpperCase
    def greet(name:String)={
      s"${this.name} says, hi ${name}"
    }
    def greet()={
      s"Hi everyone,my name is ${this.name}"
    }
    def this(name:String)={
      this(name,0,"dummy")
    }
    def this()={
      this("test")
    }
  }

  def main(args: Array[String]): Unit = {

    val aPerson=Person("John",20,"dummy")
    println(aPerson.greet())
    println(aPerson.greet("Marc"))
    println(s"${aPerson.allUpper}")
    val anOtherPerson=Person()
    val oneMorePerson=Person("Jack")
    println(anOtherPerson.greet())
    println(anOtherPerson.greet("Linda"))
    println(oneMorePerson.greet("Kevin"))
    println(oneMorePerson.greet())

  }

}

class Writer(val firstName:String,val lastName:String,val year:Int){
  def fullName()={
    s"$firstName $lastName"
  }
}

class Novel(val name:String,val year:Int,val author:Writer){
  val authorAge=year - author.year
  def isWritterBy(author:Writer)={
    author==this.author
  }
  def copy(newYearOfRelease:Int)={
    new Novel(name,newYearOfRelease,author)
  }

}

class Counter(val count:Int){
  def increment:Counter={
    new Counter(count+1)
  }
  def decrement:Counter={
    if count==0 then this
    else
    new Counter(count-1)
  }
  def increment(n:Int):Counter={
    if(n==0) then this
    else
      increment.increment(n-1)
  }
  def decrement(n:Int):Counter={
    if(n==0) then this
    else
    decrement.decrement(n-1)
  }
  def print()=println(s"current count is: $count")
}
