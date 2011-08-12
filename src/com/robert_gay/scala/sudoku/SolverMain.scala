package com.robert_gay.scala.sudoku
 

class Board(val initialData:Seq[Seq[Int]]) {
  
  if (initialData.size != 9 || initialData(0).size != 9) {
    throw new RuntimeException
  }
  
  // mutable boardData variable holding the current state
  var boardData = initialData
  
  def isSolved = {
    cols.forall(c => c.forall(v => v > 0)) && 
    		rows.forall(r => r.forall(v => v > 0))
    // TODO include section here
  }
  
  private def unsolvedIndexes(colOrRow:Seq[Int]) = {
    colOrRow.zip(0 to 8).		// pair up the values with their indexes
    	filter(p => p._1 > 0).	// remove the ones with the values == 0
    	map(p => p._2)			// just return the indexes
  }
  
  private def possibleVals(rowIdx:Int, colIdx:Int) = {
    (unusedVals(getRow(rowIdx)) ++ unusedVals(getCol(colIdx)))
  }
  
  private def unusedVals(colOrRow:Seq[Int]) = {
    // TODO 
    (0 to 8).toSet -- colOrRow
  }
  
  /*
  // My implementation, before I found Seq[A].updated(...)
  def boardWithVal2(r:Int, c:Int, v:Int) = {
   	val (r_lh, r_rh) = boardData.splitAt(r)
    val (c_lh, c_rh) = r_rh.head.splitAt(c)
    r_lh ++ ((c_lh ++ (v +: c_rh.tail)) +: r_rh.tail)
  }
  */
  
  def setVal(r:Int, c:Int, v:Int) {
    boardData = boardData.updated(r, boardData(r).updated(c, v))
  }
  
  def rows = {
    boardData
  }
  
  def cols = {
    (0 to 8).foldLeft(Seq[Seq[Int]]()) {
      (acc, idx) => (acc :+ getCol(idx))
    }
  }
  
  def getRow(i:Int) = {
    boardData(i)
  }
  
  def getCol(i:Int) = {
    // TODO this could be expensive?
    boardData.foldLeft(Seq[Int]()) {
      (acc, row) => (acc :+ row(i))
    }
  }
  
  def getSectForPos(r:Int,j:Int) = {
    
  }
  
  def getSect(i:Int) = {
    
  }
  
}

object Board {
  def apply(d:Seq[Seq[Int]]) = {
    new Board(d)
  }
  
  def solve(b:Board) {
    for (r <- 0 to 8) {
      for (c <- 0 to 8
          if b.rows(r)(c) == 0) {
        for (v <- b.possibleVals(r, c)) {
          b.setVal(r,c,v)
          solve(b)
          if (b.isSolved) {
            return
          }
          b.setVal(r,c,0)
        }
      }
    }
  }
  
}


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
    
//    println(b.rows)
//    println(b.cols)
//    println("Solved b1: " + b.isSolved)
//    println("Solved b2: " + b2.isSolved)
//    
    // println(b.boardData)
    // println(b.boardWithVal2(0,0,37))
    	
    println(b.rows)
    Board.solve(b)
    println(b.isSolved)
    println(b.rows)
    // println(b.boardWithVal2(3,4,37))
    
    
  }
}
