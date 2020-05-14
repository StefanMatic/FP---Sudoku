package GameBoard

import scala.io.Source
import scala.swing.Frame

object SudokuBoard {
  //Set sudoku board playing field
  val board  = Array.ofDim[Int](9,9)
  val fixedPositions = Array.ofDim[Boolean](9,9)

  private var currentPosition: (Int, Int) = (0,0)

  /**
   * Getter for current position of player
   * @return
   */
  def getCurrentPosition: (Int, Int) = {
    currentPosition
  }

  /**
   * Setter for current position of player
   * @param newPosition
   */
  def setCurrentPosition(newPosition: (Int, Int)) = {
    currentPosition = newPosition
  }

  /**
   * Resets the sudoku board
   */
  def resetBoard = {
    board.map(col => col.map(x => 0))
    fixedPositions.map(col => col.map(x => false))
  }

  def readFromFile(path: String): Unit = {
    val bufferedSource = Source.fromFile(path)
    val lines = bufferedSource.getLines().toList

    //closing the opened .txt file
    bufferedSource.close

    //TODO: izbaciti ovaj poziv metode kako bi mogla ova metoda da bude modularna
    fillOutSudoku(lines)
  }

  /**
   * Filling out all the fields of the sudoku board that are given to the method with the parameter
   * allRows
   *
   * @param allRows
   */
  private def fillOutSudoku(allRows: List[String]): Unit = {
    def fillOutSudokuRows(allMyRows: List[String], row: Int): Unit = {
      allMyRows match {
        case x :: xs => {
          val startColIndex: Int = 0
          fillOutSudokuField(x.toList, row, startColIndex)
          //recursive call for the next row
          fillOutSudokuRows(xs, row+1)
        }
        case Nil => println("End of table")
      }
    }

    def fillOutSudokuField(fieldValues: List[Char], myRow: Int, myCol: Int): Unit ={
      fieldValues match {
        case x :: xs if (x == '-') => {
          board(myRow).update(myCol, 0)
          fixedPositions(myRow).update(myCol, false)
          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case x :: xs =>{
          board(myRow).update(myCol, x.asDigit)

          //We are storing witch fields were pre-filled
          fixedPositions(myRow).update(myCol, true)
          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case Nil =>
      }
    }

    //Calling the start first method to start the filling of the sudoku wield with the index of the first row
    fillOutSudokuRows(allRows, 0)
  }

  //------------ Movement of currentPosition by 1 step -----------------------------
  def moveCurrentPositionUp: Unit = {
    if (currentPosition._1 != 0)
      currentPosition = (currentPosition._1 - 1, currentPosition._2)
  }

  def moveCurrentPositionDown: Unit = {
    if (currentPosition._1 != 8)
      currentPosition = (currentPosition._1 + 1, currentPosition._2)
  }

  def moveCurrentPositionLeft: Unit = {
    if (currentPosition._2 != 0)
      currentPosition = (currentPosition._1, currentPosition._2 - 1)
  }

  def moveCurrentPositionRight: Unit = {
    if (currentPosition._2 != 8)
      currentPosition = (currentPosition._1, currentPosition._2 + 1)
  }

  //----------------------------------------------------------------------------

  /**
   * Inserting the given input into sudoku table
   *
   * @param input
   * @return
   */
  def insertNumber(input: Int): Boolean = {
    if (fixedPositions(currentPosition._1)(currentPosition._2))
      false
    else {
      if (checkIfMoveCorrect(currentPosition._1, currentPosition._2, input)){
        board(currentPosition._1).update(currentPosition._2, input)
        true
      }
      else {
        //There already exist the same number in the same row, column or square
        false
      }
    }
  }

  /**
   * After checking to see if the input value can be inserted and knowing that is not allowed
   * this function is called to get the message why the operation wac declined
   *
   * @param input
   * @return
   */
  def getRefusalText(input: Int): String = {
    if (fixedPositions(currentPosition._1)(currentPosition._2))
      "Odabrano polje pripada pocetnoj tabeli."
    else {
      "Vec postoji cifra - " + input + " u istom redu, koloni ili kvadratu."
    }
  }

  /**
   * Checking if the current move is correct
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
      val helper = board.flatten.grouped(3).toArray

      val firstIndex = (row / 3) * 9 + col /3
      val secondIndex = firstIndex + 3
      val thirdIndex = firstIndex + 6

      //All the fields can be found in an increment of 3
      val mySquare = List(helper(firstIndex), helper(secondIndex), helper(thirdIndex)).flatten
      !mySquare.exists(x => x == inputValue)
    }

    //checking rows, columns and squares
    checkSquare && checkRow && checkCol
  }

  /**
   * Checking if the player is completed with filling out the sudoku
   * @return
   */
  def checkIfSudokuFinished: Boolean = {
    def goThroughRows(row: Int): Boolean = {
      if (row == 9) true
      else {
        if (board(row).exists(x=> x==0)) false
        else
          goThroughRows(row + 1)
      }
    }

    goThroughRows(0)
  }

  /**
   *Displays the sudoku board
   */
  def showTable: String = {
    board.map(col => col mkString(" ")).mkString("\n")
  }
}
