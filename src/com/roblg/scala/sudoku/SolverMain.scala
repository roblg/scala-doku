package com.roblg.scala.sudoku


object SolverMain {
  
  def main(args : Array[String]) : Unit = {
    
    // TODO test data. Read data from somewhere else here
    
    val data = Seq(
        Seq(9,0,0, 0,0,0, 0,6,0),
        Seq(3,0,5, 0,0,8, 0,0,0),
        Seq(0,0,0, 6,0,3, 7,0,0),
        
        Seq(0,8,4, 0,3,0, 0,0,0),
        Seq(1,0,0, 0,0,0, 0,0,9),
        Seq(0,0,0, 0,6,0, 3,1,0),
        
        Seq(0,0,2, 8,0,5, 0,0,0),
        Seq(0,0,0, 4,0,0, 2,0,1),
        Seq(0,5,0, 0,0,0, 0,0,6)
    )
    
    val data2 = Seq(
        Seq(9,1,2, 6,3,7, 4,5,8),
        Seq(6,4,3, 8,5,9, 7,2,1),
        Seq(8,7,5, 4,1,2, 6,3,9),
        
        Seq(1,8,7, 3,4,5, 9,6,2),
        Seq(3,6,9, 2,7,1, 5,8,4),
        Seq(5,2,4, 9,6,8, 3,1,7),
        
        Seq(4,9,8, 5,2,6, 1,7,3),
        Seq(2,5,1, 7,9,3, 8,4,6),
        Seq(7,3,6, 1,8,4, 2,9,5)
    )
    
    val b = Board(data)
    val b2 = Board(data2)
    println(b)
    println(b.solvedCopy)
  }
}
