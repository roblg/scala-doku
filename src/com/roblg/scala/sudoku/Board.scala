package com.roblg.scala.sudoku

/** A class representing an immutable Sudoku board.
 *  
 *  
 */
class Board(val initialData:List[List[Int]]) {
  
  if (initialData.size != 9 || initialData(0).size != 9) {
    throw new RuntimeException("Sudoku board data must be 9x9!")
  }
  
  // immutable boardData variable holding the state of this board
  val boardData = initialData
  
  /** Determines whether or not the given Board is solved.
   *  @return true if the given Board configuration is solved,
   *  		  false otherwise
   */
  def isSolved = 
    	cols.forall(c => c.forall(v => v > 0)) && 
    	rows.forall(r => r.forall(v => v > 0)) &&
    	sects.forall(s => s.forall(v => v > 0))
  
  /** Finds the set of the values that the cell at (rowIdx,colIdx) could take.
   *  @param r The index of the row of the cell.
   *  @param c The index of the column of the cell.
   *  @return  the possible (valid) values that the cell at (rowIdx,colIdx)
   *  		   could take. 
   */
  def possibleVals(r:Int, c:Int) = 
    if (rows(r)(c) != 0)
      Set()
    else
      (1 to 9).toSet -- rows(r) -- cols(c) -- sectData(sectForPos(r, c))

  /** Updates the value of the cell (r,c) to be `v` and returns an 
   *  updated copy of the board.
   *  @param r The row index of the cell to update.
   *  @param c The column index of the cell to update.
   *  @param v The value to set in the cell
   *  @return  a copy of the board with the value at cell (`r`,`c`) replaced with `v`
   */
  def withVal(r:Int, c:Int, v:Int) = Board(boardData.updated(r, boardData(r).updated(c, v)))
  
  /** The rows on the Board.
   *  @return an List of the rows in the Board. 
   */
  def rows = boardData
  
  private def cols = (0 to 8).foldLeft(List[List[Int]]()) ((acc, idx) => (acc :+ colData(idx)))
 
  private def colData(i:Int) = boardData.foldLeft(List[Int]()) ((acc, row) => (acc :+ row(i)))
  
  private def sects = (0 to 8).foldLeft(List[List[Int]]()) ((acc, idx) => (acc :+ sectData(idx)))  

  private def sectData(i:Int) = {
    val startRow = (i / 3) * 3
    val startCol = (i % 3) * 3
    
    rows(startRow).slice(startCol, startCol + 3) ++ 
    	rows(startRow+1).slice(startCol, startCol + 3) ++ 
    	rows(startRow+2).slice(startCol, startCol + 3)	
  }
  
  // TODO: ugly
  private def sectForPos(r:Int,c:Int) = r match {
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

  /** Starting from this Board, look for a solution, and
   *  return it if it exists.
   *  @return a Board that represents the solution to this Board, or
   *          null if no solution exists.
   */
  def solvedCopy = Board.solve(this)
  
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
  
  private def solve(b:Board) : Board = {
    if (b.isSolved) {
      return b
    } else {
	    for (r <- 0 to 8) {
	      for (c <- 0 to 8
	        if b.rows(r)(c) == 0) {
	        val possibles = b.possibleVals(r,c)
	        for (v <- possibles) {
	          // attempt to solve the board by placing `v` in 
	          // cell (r,c)
	          val newB = solve(b.withVal(r,c,v))
	          if (null != newB && newB.isSolved) {
	            return newB
	          }
	        }
	        // if we've made it here, we've exhausted all the possibilities
	        // for this (row,col) pair, and we haven't found a solution. That means
	        // that the configuration of the board somewhere before us is wrong, which
	        // means the board can't be solved. 
	        return null
	      }
	    }
	    // We've tried every empty cell in the board, and haven't found a solution.
	    // Something is up... return.
	    return null;
    }
  }
}