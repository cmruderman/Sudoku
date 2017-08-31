import hw.sudoku._
object Solution extends SudokuLike {
	type T = Board
	val eB = 0.until(9).flatMap{row => 0.until(9).map { col => (row, col)}}.map{ //coords x and y range from 0-8
		      case (x: Int, y: Int) => ((x, y), 1.until(10).toList)}.toMap //initialize board with each coord pointing to List(123456789)

	def parse (str: String): Board = {
		    val mapper = 0.until(9).flatMap{row => 0.until(9).map { col => ((row, col), str.charAt(row*9+col))} //map the coordinate to corresponding character
		      }.filterNot{case (someCoord, charAtCurrLocation) => charAtCurrLocation == '.'}. //if char is a period filter it out and dont do anything to it
			map{case (coord, charAt) => ((coord._1, coord._2), charAt.asDigit)}.toMap //if its a number, point the coord to that number
		    val mapThePeers = mapper.toList.flatMap{
		      case (coord, listOfValues) => peers(coord._1, coord._2).map { z => (z, listOfValues)}} //map peers of xy to the same list
		    val peersMap = 0.until(9).flatMap{ //for all rows 0-8
		      row => 0.until(9).map{ //for all cols 0-8
		        col => ((row, col), mapThePeers.filter{ //filter the peers 
		          case (coord, valList) => (coord._1 == row && coord._2 == col)//if coord matches
		          }.map{case (someCoord, valueList) => valueList}.toList.distinct)}}.toMap    //map that coordinate to the value and make list have no duplicates
		    val newB = eB.map{
		      case (coord, vals) => {
		      	//if (peersMap.contains((x, y)) && vals.filterNot(d => peersMap((x, y)).contains(d)).size==1){
		        if (peersMap.contains((coord._1, coord._2))) ((coord._1, coord._2), vals.filterNot(d => peersMap((coord._1, coord._2)).contains(d))) //filter out value from peers if it contains that value
		        else ((coord._1, coord._2), vals)}}.map{  //if it doesnt contain it...leave it alone
		      case (coord, vals) => {
		        if (mapper.contains((coord._1, coord._2))) ((coord._1, coord._2), List(mapper((coord._1, coord._2)))) 					
		        else ((coord._1, coord._2), vals)		
		      }
		     }
		    new Board(newB) //return parsed board
 }

	def peers (row : Int , col : Int): List [(Int , Int)] = {
		def getRow(c: Int): List[(Int, Int)] = 0.to(8).toList.map(x => (x, c)) //get coords in row
		def getCol(r: Int): List[(Int, Int)] = 0.to(8).toList.map(z => (r, z)) //get coords in col
		val needToGetRidOfRowCol = getRow(col)++getCol(row)++((row/3)*3).to(((row/3)*3)+2).flatMap{r => ((col/3)*3).to(((col/3)*3)+2).map{ c => (r, c)}} //get coords in box
		needToGetRidOfRowCol.filterNot{ //filter out the coord
			case (x,y) => x==row && y ==col
			}.toList.distinct
  }
}

// Top - left corner is (0 ,0). Bottom - right corner is (8 ,8)
class Board (val available : Map[(Int , Int) , List[Int]]) extends BoardLike[Board] {

	def availableValuesAt (row : Int , col : Int): List [Int] = {
		available.getOrElse((row,col), 1.to(9).toList)
	}

	def valueAt (row :Int , col :Int ): Option[Int] = {
		if(availableValuesAt(row, col).length==1) availableValuesAt(row, col).headOption //if the length of the list is one, return the head
		else None //all other cases return None
	}

	def isSolved (): Boolean = available.values.toList.filter(f => f.length == 1).length == 81 //if all coords are mapped to one, its solved
	def isUnsolvable (): Boolean = available.values.toList.filterNot(f => f.length != 0).length > 0 //if there are any coords mapped to a zero list its unsolvable

	def place(row: Int, col: Int, value: Int): Board = 
	{
		require(availableValuesAt(row, col).contains(value)) //require there is something at the value
	    new Board(placeHelper(value, Solution.peers(row, col), available.map{ //map on the Map
	      case ((r, c), lst) => { 
	        if (r == row && c == col) ((r, c), List(value)) //if the coord exist on the map...place it
	        else ((r, c), lst) //if coord doesnt exist, place it in the map
	      }
	    })) //call placeHelper to deal with updating peers
 	}

 	def placeHelper(digit: Int, lstOfPeers: List[(Int, Int)], map: Map[(Int , Int) , List[Int]]): Map[(Int , Int) , List[Int]] = //placehelper is the way to deal with peers and updating their values
 		lstOfPeers match{
 			case h::t if(map(h).contains(digit)) => { //if the map contains the peer
 				val checkIfLengthOne = map + (h -> map(h).filter(x => x!=digit)) //filter out digit from the peer and add back to map
 				if(map(h).length>1 && checkIfLengthOne(h).length==1){ //if the length of the list of the peer is greater than one before removing and the length is now one after removing
					placeHelper(digit, t, placeHelper(checkIfLengthOne(h).head, Solution.peers(h._1, h._2), checkIfLengthOne))} //place digit in the map and get new map -> call placehelper on new map
 				else placeHelper(digit, t, checkIfLengthOne)}//peer's list is >1 so call placeHelper on the digit, the rest of the peers and the digit filtered out from the peer
 			case h::t => placeHelper(digit, t, map) //if the peer doesnt contain the digit, recur on tail
 			case _ => map //if theres nothing left to recur on return the map
 		} 

	def nextStates(): Stream[Board] = {
		if (isUnsolvable()) Stream.Empty
		else {
			val av = available.flatMap{ //for every single coordinate
				case ((row,col), valList) if(valList.length > 1) => valList.map{valu => place(row, col, valu)}.toStream //if there is more than one value in stream -> place something there
				case _ => Stream.Empty} //return empty stream in all other cases
			val avList = av.toStream
			avList.distinct.sortWith{(before, after) => before.available.values.toStream. //sort the available spots where the value could go
				filterNot(x => x.length!=1).length > after.available.values.toStream.filterNot(x => x.length!=1).length} //order by where there are more spots with single value 
		}
	}
	def solve(): Option[Board] = {
		def helpMeSolve(brdLst: Stream[Board]): Option[Board] = brdLst match{
				case h #:: t if(h.isSolved)=> Some(h)
				case h #:: t => helpMeSolve(t ++ h.nextStates()) //recur with tail and next states of head
				case _ => None //Not able to be solved
			}
		helpMeSolve(Stream(this))  //call to solveHelp passes stream of the boards
	} 
}










