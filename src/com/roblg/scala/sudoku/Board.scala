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