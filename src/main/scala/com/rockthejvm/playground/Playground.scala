package com.rockthejvm.playground

object Playground {

  def greetFunc(name:String,age:Int)=println(s"Hi my name is $name and I am $age years of old.")
  def factFunc(n:Int):Int={
    if(n<=0) 0
    else if (n==1) 1
    else n* factFunc(n-1)
  }
  def fibFunc(n:Int):Int={
    if(n<2) 1
    else fibFunc(n-1)+fibFunc(n-2)
  }
  def isPrime(n:Int):Boolean={
    def isPrime(n:Int,d:Int):Boolean={
      if(n==0||n==1||n==2) true
      else if(d>n/2) true
      else if(n%d == 0) false
      else isPrime(n,d+1)
    }
    isPrime(n,2)
  }
  def main(args: Array[String]): Unit = {
    println("Running Scala 3! I can't wait to learn Scala in this course...")
    greetFunc("Sreekanth",38)
    println(s"factorial: ${factFunc(5)}")
    println(s"Fibnocci: ${fibFunc(5)}")
    println(s"isPrime(5): ${isPrime(5)}")
    println(s"isPrime(0): ${isPrime(0)}")
    println(s"isPrime(1): ${isPrime(1)}")
    println(s"isPrime(2): ${isPrime(2)}")
    println(s"isPrime(18): ${isPrime(18)}")
    println(s"isPrime(19): ${isPrime(19)}")

  }
}
