import GUI.GameLookConstants

import scala.io.Source

package object GameBoard {
  type Matrix = Array[Array[Int]]
  type SudokuMatrix = Array[Array[(Int, Boolean)]]
  type FunctionWrapper = ((Int, Int)) => (Int, Int)
  type FunctionListType = List[(String, List[FunctionWrapper])]

  // ----------------------- IO Actions --------------------------------

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

  // ----------------------- Board Actions -----------------------------

  /**
   * Files out the sudoku from a file
   *
   * @param path
   */
  def fillSudoku(path: String): SudokuMatrix = {
    fillOutSudoku(readFromFile(path))
  }

  /**
   * Filling out all the fields of the sudoku board that are given to the method with the parameter
   * allRows
   *
   * @param allRows
   */
  def fillOutSudoku(allRows: List[String]): SudokuMatrix = {
    val mySudokuMatrix: SudokuMatrix = Array.ofDim[(Int, Boolean)](9,9)

    def fillOutSudokuRows(allMyRows: List[String], row: Int): Unit = {
      allMyRows match {
        case x :: xs => {
          val startColIndex: Int = 0
          fillOutSudokuField(x.toList, row, startColIndex)
          //recursive call for the next row
          fillOutSudokuRows(xs, row+1)
        }
        case Nil =>
      }
    }

    def fillOutSudokuField(fieldValues: List[Char], myRow: Int, myCol: Int): Unit ={
      //TODO: Porisi ove linije koda cim proradi
      fieldValues match {
        case x :: xs if (x == '-') => {
          //Inserting the value 0 in board and inserting 0 to indicate that this field is not part of the original sudoku board
          mySudokuMatrix(myRow).update(myCol, (0, false))
          //board(myRow).update(myCol, 0)
          //fixedPositions(myRow).update(myCol, false)

          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case x :: xs if (x == 'P') => {
          //Inserting the value -1 in board and inserting false to indicate that this field is not part of the original sudoku board
          mySudokuMatrix(myRow).update(myCol, (-1, false))

          //board(myRow).update(myCol, 0)
          //fixedPositions(myRow).update(myCol, false)

          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case x :: xs =>{
          //Inserting the value 0 in board and inserting true to indicate that this field IS part of the original sudoku board
          mySudokuMatrix(myRow).update(myCol, (x.asDigit, true))

          //board(myRow).update(myCol, x.asDigit)

          //We are storing witch fields were pre-filled
          //fixedPositions(myRow).update(myCol, true)

          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case Nil =>
      }
    }

    //Calling the start first method to start the filling of the sudoku wield with the index of the first row
    fillOutSudokuRows(allRows, 0)

    mySudokuMatrix
  }

  /**
   * Method for currying! Applying the given function on all elements of board.
   *
   * @param f
   */
  def changeBoard(f: (Int, Int, Int) => Unit)(table: SudokuMatrix): Unit =  {
    /**
     * Going through all the rows and applying the given function
     *
     * @param row
     * @param rowIndex
     */
    def changeBoardRows(row: Array[(Int, Boolean)], rowIndex: Int) = {
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
      elementsWithIndex.foreach(x =>
      {
        val element: (Int, Boolean) = x._1
        changeBoardElements(element._1, x._2)
      })
    }

    val rowsWithIndex = table.zipWithIndex
    rowsWithIndex.foreach(x => changeBoardRows(x._1, x._2))
  }

  /**
   * Getting all the fields from the square in witch the row and col point to
   *
   * @param table
   * @param row
   * @param col
   * @return
   */
  def getAllFieldsFromSquare(row: Int, col: Int, table: Matrix): List[Int] = {
    val helper = table.flatten.grouped(3).toArray

    val firstIndex = (row / 3) * 9 + col /3
    val secondIndex = firstIndex + 3
    val thirdIndex = firstIndex + 6

    //All the fields can be found in an increment of 3
    val mySquare = List(helper(firstIndex), helper(secondIndex), helper(thirdIndex)).flatten
    mySquare
  }

  /**
   * Getting only the value elements from SudokuMatrix (from tuple)
   *
   * @param sudokuMatrix
   * @return
   */
  def getMatrixOfValue(sudokuMatrix: SudokuMatrix): Matrix = {
    sudokuMatrix.map(x => x.map(y => y._1))
  }

  /**
   *Displays the sudoku board
   */
  def showTable(table: Matrix): String = {
    table.map(col => col mkString(" ")).mkString("\n")
  }

  //----------------------------- Checkers --------------------------------

  /**
   * Checking if the current move is correct
   *
   * @param row
   * @param col
   * @param inputValue
   * @return
   */
  def checkIfMoveCorrect(row: Int, col: Int, inputValue: Int, table: Matrix): Boolean ={
    def checkRow: Boolean = {
      //If there exists even one field with the same value in the same row than we return false
      !table(row).exists(x=> x == inputValue)
    }

    def checkCol: Boolean = {
      val boardTransposed = table.transpose
      //If there exists even one field with the same value in the same col than we return false
      !boardTransposed(col).exists(x => x == inputValue)
    }

    def checkSquare: Boolean = {
      val mySquare = getAllFieldsFromSquare(row, col, table)
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
      val allSquaresChecks =
        for (r <- 0 to 8 by 3; c <- 0 to 8 by 3)
          yield checkIfRowCorrect(getAllFieldsFromSquare(r, c, table).toArray)

      !allSquaresChecks.exists(x => x == false)
    }

    checkRows && checkCols && checkSquare
  }

  // ------------------------------- Actions ---------------------------------

  def findStartPosition(table: SudokuMatrix): Positions = {
    val myRow: Int = table.indexWhere(row => row.exists(el => el._1 == -1))
    val myCol: Int = table(myRow).indexWhere(x => x._1 == -1)

    //updating the table
    table(myRow).update(myCol, (0, false))

    new Positions((myRow, myCol), (myRow, myCol))
  }

  def changePositions(curr: (Int, Int), start: (Int, Int), positions: Positions): Positions = {
    if (curr._1 == -1){
      positions.changeStartingPosition(start._1, start._2)
    } else if (start._1 == -1) {
      positions.changeCurrentPosition(curr._1, curr._2)
    } else {
      positions.changeCurrentAndStartingPosition(curr._1, curr._2, start._1, start._2)
    }
  }

  /**
   * Inserting the given input into sudoku table
   *
   * @return
   */
  def insertNumberOnBoard(row: Int, col: Int, value: Int, table: SudokuMatrix): Int = {
    //Checking to see if the user inputs the same number as the one already on the sudoku board
    if (table(row)(col)._1 == value){
      GameLookConstants.CODE_OK
    }
    else {
      if (table(row)(col)._2) {
        //Nothing happens because it is not allowed to change an original board numbers
        GameLookConstants.CODE_ERROR
      } else {
        //Updating the sudoku board and checking if the operation was valid
        //this order of calls must be held
        val checkForMoveValidation = checkIfMoveCorrect(row, col, value, getMatrixOfValue(table))

        val previousValue: (Int, Boolean) = table(row)(col)
        table(row).update(col, (value, previousValue._2))

        if (checkForMoveValidation) {
          GameLookConstants.CODE_OK
        } else
          GameLookConstants.CODE_WARNING
      }
    }
  }

  /**
   * Emptying out a selected field if it is not from the original sudoku board
   *
   * @return
   */
  def eraseNumberFromBoard(row: Int, col: Int, table: SudokuMatrix): Int = {
    if (table(row)(col)._2)
      GameLookConstants.CODE_ERROR
    else{
      val previousValue: (Int, Boolean) = table(row)(col)
      table(row).update(col, (0, previousValue._2))
      GameLookConstants.CODE_OK
    }
  }
}
