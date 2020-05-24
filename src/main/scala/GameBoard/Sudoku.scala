package GameBoard

import scala.io.Source
import scala.swing.Frame

abstract class Sudoku {
  type Matrix = Array[Array[Int]]

  //Set sudoku board playing field
  val board  = Array.ofDim[Int](9,9)

  var currentPosition: (Int, Int) = (0,0)
  var startPosition: (Int, Int) = (0,0)

  /**
   * Getter for current position of player
   *
   * @return
   */
  def getCurrentPosition: (Int, Int) = {
    currentPosition
  }

  /**
   * Setter for current position of player
   *
   * @param newPosition
   */
  def setCurrentPosition(newPosition: (Int, Int)): Unit = {
    currentPosition = newPosition
  }

  /**
   * Getter for starting position of game
   *
   * @return
   */
  def getStartingPosition: (Int, Int) = {
    startPosition
  }

  /**
   * Setting the new start position
   *
   * @param newPosition
   */
  def setStartingPosition(newPosition: (Int, Int)): Unit = {
    startPosition = newPosition
  }

  /**
   *Displays the sudoku board
   */
  def showTable(table: Matrix): String = {
    table.map(col => col mkString(" ")).mkString("\n")
  }

  /**
   * Resets the sudoku board
   *
   */
  def resetBoard

  //------------------------ Start the game -------------------------

  /**
   * Reading all the lines from a .txt file
   *
   * @param path
   */
  def readFromFile(path: String): List[String] = {
    val bufferedSource = Source.fromFile(path)
    val lines = bufferedSource.getLines().toList
    //closing the opened .txt file
    bufferedSource.close

    lines
  }

  /**
   * ABSTRACT
   * Implementation of this method should fills out the sudoku from a file
   *
   * @param path
   */
  def fillSudoku(path: String): Unit

  //--------------------------- Chekers ------------------------------

  /**
   * Checking if the current move is correct
   *
   * @param row
   * @param col
   * @param inputValue
   * @return
   */
  def checkIfMoveCorrect(row: Int, col: Int, inputValue: Int): Boolean ={
    def checkRow: Boolean = {
      //If there exists even one field with the same value in the same row than we return false
      !board(row).exists(x=> x == inputValue)
    }

    def checkCol: Boolean = {
      val boardTransposed = board.transpose
      //If there exists even one field with the same value in the same col than we return false
      !boardTransposed(col).exists(x => x == inputValue)
    }

    def checkSquare: Boolean = {
      val mySquare = getAllFieldsFromSquare(board, row, col)
      !mySquare.exists(x => x == inputValue)
    }

    //checking rows, columns and squares
    checkSquare && checkRow && checkCol
  }

  /**
   * Checking if the player is completed with filling out the sudoku
   *
   * @return
   */
  def checkIfSudokuFinished(table: Matrix): Boolean = {
    def checkIfEmptyFieldExists(myArray: Array[Int]): Boolean = {
      //if there exists even one 0 in the array, the function will return !true (false)
      !myArray.exists(x => x == 0)
    }
    // returns true only if there are no 0 in any of the rows
    table.forall(arr => checkIfEmptyFieldExists(arr))
  }

  /**
   * Checks if the sudoku board is correctly filled
   *
   * @param table
   * @return
   */
  def checkIfSudokuCorrect(table: Matrix): Boolean = {
    /**
     *  Helper method for checking correctness for a row
     *
     * @param row
     * @return
     */
    def checkIfRowCorrect(row: Array[Int]): Boolean = {
      row.distinct.length == 9
    }

    def checkRows: Boolean = {
      table.forall(x => checkIfRowCorrect(x))
    }
    def checkCols: Boolean = {
      table.transpose.forall(x => checkIfRowCorrect(x))
    }
    def checkSquare: Boolean = {
      val allSquaresCheckes =
        for (r <- 0 to 8 by 3; c <- 0 to 8 by 3)
          yield checkIfRowCorrect(getAllFieldsFromSquare(table,r,c).toArray)

      !allSquaresCheckes.exists(x => x == false)
    }

    checkRows && checkCols && checkSquare
  }

  //------------ Movement of currentPosition by 1 step -----------------------------

  def moveCurrentPositionUp: Unit = {
    if (currentPosition._1 != 0) {
      positionChange(currentPosition._1 - 1, currentPosition._2)
    }
  }

  def moveCurrentPositionDown: Unit = {
    if (currentPosition._1 != 8)
      positionChange(currentPosition._1 + 1, currentPosition._2)
  }

  def moveCurrentPositionLeft: Unit = {
    if (currentPosition._2 != 0)
      positionChange(currentPosition._1, currentPosition._2 - 1)
  }

  def moveCurrentPositionRight: Unit = {
    if (currentPosition._2 != 8)
      positionChange(currentPosition._1, currentPosition._2 + 1)
  }

  //-------------------------------- Actions -----------------------------------

  /**
   * Method for currying! Applying the given function on all elements of board.
   *
   * @param f
   */
  def changeBoard(f: (Int, Int, Int) => Unit): Unit =  {
    /**
     * Going through all the rows and applying the given function
     *
     * @param row
     * @param rowIndex
     */
    def changeBoardRows(row: Array[Int], rowIndex: Int) = {
      /**
       * Going through all the elements of a row and applying the given function
       *
       * @param el
       * @param colIndex
       */
      def changeBoardElements(el: Int, colIndex: Int) = {
        f(rowIndex, colIndex, el)
      }

      val elementsWithIndex = row.zipWithIndex
      elementsWithIndex.foreach(x => changeBoardElements(x._1, x._2))
    }

    val rowsWithIndex = board.zipWithIndex
    rowsWithIndex.foreach(x => changeBoardRows(x._1, x._2))
  }

  /**
   * Getting all the fields from the square in witch the row and col point to
   *
   * @param row
   * @param col
   * @return
   */
  def getAllFieldsFromSquare(table: Matrix, row: Int, col: Int): List[Int] = {
    val helper = table.flatten.grouped(3).toArray

    val firstIndex = (row / 3) * 9 + col /3
    val secondIndex = firstIndex + 3
    val thirdIndex = firstIndex + 6

    //All the fields can be found in an increment of 3
    val mySquare = List(helper(firstIndex), helper(secondIndex), helper(thirdIndex)).flatten
    mySquare
  }

  /**
   * ABSTRACT
   * Implementation of this method should solve the current sudoku board
   */
  def solveSudoku: Boolean

  //-------------------------------- GUI Actions -------------------------------

  /**
   * ABSTRACT
   * Implementation of this method changes the sudoku table in real time in response to user movement on the sudoku field
   *
   * @param row
   * @param col
   */
  def positionChange (row: Int, col: Int)

  /**
   * ABSTRACT
   * Implementation of this method inserts a selected number on the sudoku field with all the necessary checks
   *
   * @param input
   */
  def inputNumber(input: Int)

  /**
   * ABSTRACT
   * Implementation of this method should erase a number on the current position on the sudoku board
   */
  def eraseNumber: Unit

  /**
   * ABSTRACT
   * Implementation of this method should close all the currently opened windows
   */
  def closeWindows: Unit
}
