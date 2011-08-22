package com.roblg.scala.sudoku

import scala.collection.mutable.SetBuilder
import scala.collection.mutable.Builder
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer


class Board(val initialData:List[List[Int]]) {
  
  if (initialData.size != 9 || initialData(0).size != 9) {
    throw new RuntimeException
  }
  
  // immutable boardData variable holding the state of this board
  val boardData = initialData
  
  def isSolved = {
    cols.forall(c => c.forall(v => v > 0)) && 
    		rows.forall(r => r.forall(v => v > 0)) &&
    		sects.forall(s => s.forall(v => v > 0))
  }
  
  def possibleVals(rowIdx:Int, colIdx:Int) = {
    if (rows(rowIdx)(colIdx) != 0) {
      Set()
    } else {
      (1 to 9).toSet -- getRow(rowIdx) -- getCol(colIdx) -- getSect(getSectForPos(rowIdx, colIdx))
    }
  }

  def withVal(r:Int, c:Int, v:Int) = {
    Board(boardData.updated(r, boardData(r).updated(c, v)))
  }
  
  def rows = {
    boardData
  }
  
  def cols = {
    (0 to 8).foldLeft(List[List[Int]]()) {
      (acc, idx) => (acc :+ getCol(idx))
    }
  }
  
  def sects = {
    (0 to 8).foldLeft(List[List[Int]]()) {
      (acc, idx) => (acc :+ getSect(idx))
    }
  }
  
  def getRow(i:Int) = {
    boardData(i)
  }
  
  def getCol(i:Int) = {
    // TODO this could be expensive?
    boardData.foldLeft(List[Int]()) {
      (acc, row) => (acc :+ row(i))
    }
  }
  
  def getSectForPos(r:Int,c:Int) = {
    // TODO: ugly 
    r match {
      case ridx if (ridx >= 0 && ridx <= 2) => c match {
        case cidx if (cidx >= 0 && cidx <= 2) => 0
        case cidx if (cidx >= 3 && cidx <= 5) => 1
        case cidx if (cidx >= 6 && cidx <= 8) => 2
      }
      case ridx if (ridx >= 3 && ridx <= 5) => c match {
        case cidx if (cidx >= 0 && cidx <= 2) => 3
        case cidx if (cidx >= 3 && cidx <= 5) => 4
        case cidx if (cidx >= 6 && cidx <= 8) => 5
      }
      case ridx if (ridx >= 6 && ridx <= 8) => c match {
        case cidx if (cidx >= 0 && cidx <= 2) => 6
        case cidx if (cidx >= 3 && cidx <= 5) => 7
        case cidx if (cidx >= 6 && cidx <= 8) => 8
      }
    }
  }
  
  def getSect(i:Int) = {
    val startRow = (i / 3) * 3
    val startCol = (i % 3) * 3
    
    getRow(startRow).slice(startCol, startCol + 3) ++ 
    	getRow(startRow+1).slice(startCol, startCol + 3) ++ 
    	getRow(startRow+2).slice(startCol, startCol + 3)
    	
  }
  
  override def toString = "[Board:" + rows.toString + "]"
  
}

object Board {
  def apply(d:List[List[Int]]) = {
    new Board(d)
  }
  
  def apply(d:Seq[Seq[Int]]) = {
    new Board(d.foldLeft(List[List[Int]]()) {
      (acc, r) => acc :+ r.toList
    })
  }
  
  def solve(b:Board) : Board = {
    if (b.isSolved) {
      return b
    } else {
	    for (r <- 0 to 8) {
	      for (c <- 0 to 8
	        if b.rows(r)(c) == 0) {
	        val possibles = b.possibleVals(r,c)
	        for (v <- possibles) {
	          val newB = solve(b.withVal(r,c,v))
	          if (null != newB && newB.isSolved) {
	            return newB
	          }
	        }
	        return null
	      }
	    }
	    return null;
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
    //println(b.withVal(4,7,37))
    	
    //println(b2.getSect(5))
    
//    println(b.rows)
    println(Board.solve(b))
    //println(b.possibleVals(8,8))
    //println(Board.solve(b).rows)
//    
    // println(b.boardWithVal2(3,4,37))
    
    
  }
}
