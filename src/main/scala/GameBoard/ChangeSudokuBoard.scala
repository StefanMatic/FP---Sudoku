package GameBoard

import GUI.{GameLookConstants, NewSudokuBoardFrame}

import scala.swing.{Action, Color}
import scala.language.postfixOps

object ChangeSudokuBoard extends Sudoku {
  var newSudokuBoard: NewSudokuBoardFrame = null

  /**
   * Setter for NewSudokuBoard
   *
   * @param newBoard
   */
  def setGameFrameTable(newBoard: NewSudokuBoardFrame): Unit = {
    newSudokuBoard = newBoard
    //setting the first position for the beginning of the game
    positionChange(currentPosition._1,currentPosition._2)
  }

  /**
   * Resets the sudoku board
   *
   */
  override def resetBoard = {
    board.map(col => col.map(x => 0))
  }

  //------------------------ Start the game -------------------------

  /**
   * Files out the sudoku from a file
   *
   * @param path
   */
  override def fillSudoku(path: String): Unit = {
    fillOutSudoku(readFromFile(path))
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

          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case x :: xs if (x == 'P') => {
          board(myRow).update(myCol, 0)
          setCurrentPosition(myRow, myCol)


          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case x :: xs =>{
          board(myRow).update(myCol, x.asDigit)

          //We are storing witch fields were pre-filled
          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case Nil =>
      }
    }

    //Calling the start first method to start the filling of the sudoku wield with the index of the first row
    fillOutSudokuRows(allRows, 0)
  }

  //-------------------------------- Actions -----------------------------------

  //TODO: Ispravi ovo samo da vraca da li je moguce da se uradi sudoku bez ispisivanja
  //TODO: takode napravi kopiju borda pre nego sto krenes da ne bbi poremetio pravi board
  /**
   * Solves the current sudoku board
   */
  override def solveSudoku: Boolean = {
    /**
     * Looks at the square of the current field and checks if there are any duplicates of numbers
     *
     * @param row
     * @param col
     * @param number
     * @return
     */
    def checkSquare(row: Int, col: Int, number: Int): Boolean = {
      !getAllFieldsFromSquare(row, col).exists(x => x == number)
    }
    /**
     * Getting all the rows in witch the requested number doesn't appear
     *
     * @param table
     * @param number
     * @return
     */
    def getAllRowsWithoutNumber(table: Array[Array[Int]], number: Int): List[Int] = {
      val allUnusedRows =
        for (row <- 0 to 8 if (!table(row).exists(x => x == number)))
          yield row

      allUnusedRows.toList
    }
    /**
     * Getting all the possible fields on the board for a requested number. This function checks if all the fields
     * don't already have the same number in the same row, column or sub-square
     *
     * @param number
     * @return
     */
    def getAllPossibleFieldsForNumber(number: Int): List[(Int, Int)] = {
      val allRows = getAllRowsWithoutNumber(board, number)
      val allCols = getAllRowsWithoutNumber(board.transpose, number)

      val allPossibleFields =
        for (rows <- allRows; cols <- allCols if (checkSquare(rows, cols, number) &&  board(rows)(cols) == 0))
          yield (rows, cols)

      allPossibleFields.toList
    }
    /**
     *  Try to solve the sudoku board
     */
    def findResults: Boolean = {
      val boardCopy = board.map(_.clone)
      //TODO: Skloni ovo kada proveris da li radi
      println(showTable(boardCopy))

      /**
       * Looks at all the possibilities and tries to find a certain field to fill. If a certain move can be made
       * than this function returns true, otherwise returns false
       *
       * @param possibilities
       * @param currentNumber
       * @return
       */
      def checkIfNumberCanBeUsed (possibilities: List[(Int, Int)], currentNumber: Int): Boolean = {
        def goThroughAllEntries(possibilities: List[(Int, Int)]): Boolean = {
          if (possibilities.length == 1) {
            //After we found a certain move, me make the adjustments to the board
            //TODO: Proveri ovo kada budes dosao do ovog dela
            //positionChange(possibilities.head._1, possibilities.head._2)
            //inputNumber(currentNumber + 1)

            //Writing in the new number
            board(possibilities.head._1).update(possibilities.head._2, currentNumber + 1)

            //return true because we made adjustments on the board
            true
          } else
            false
        }

        //HashMap for all the possible fields for the current number
        val hashMapPossibilitiesRow = possibilities groupBy (x => x._1)
        val hashMapPossibilitiesCol = possibilities groupBy (x => x._2)

        //check all the rows and columns for possible moves
        hashMapPossibilitiesRow.exists(x => goThroughAllEntries(x._2)) || hashMapPossibilitiesCol.exists(x => goThroughAllEntries(x._2))
      }

      //Getting all the possibilities for all the numbers and storing them in an array
      val allPossibilitiesForAllNumbers =
        (for (number <- 1 to 9)
          yield getAllPossibleFieldsForNumber(number)) zipWithIndex

      //try to find even one new move
      val validationForTurn: Boolean = allPossibilitiesForAllNumbers.exists(x => checkIfNumberCanBeUsed(x._1, x._2))

      //if a new move exists, the algorithm continuous
      if (checkIfSudokuFinished || !validationForTurn){
        //If it's finished, that means the sudoku game is solvable
        if (checkIfSudokuFinished)
          true
        else
          false
      } else {
        //the algorithm isn't over yet - it's called recursively
        findResults
      }
    }

    findResults
  }

  //-------------------------------- GUI Actions -------------------------------

  /**
   * Changes the sudoku table in real time in response to user movement on the sudoku field
   *
   * @param row
   * @param col
   */
  override def positionChange(row: Int, col: Int): Unit = {
    def setBoardColors = {
      def changeColor(r: Int, c: Int, color: Color) = {
        for (iter <- 0 to 8) {
          //returning the previous selection to normal color
          newSudokuBoard.allSudokuFields(r)(iter).background = color
          newSudokuBoard.allSudokuFields(iter)(c).background = color
        }
      }
      /**
       * Receives the number of box and the color the background should be changed to
       *
       * @param numOfBox
       * @param color
       */
      def changeColorOfBox(numOfBox: Int, color: Color): Unit = {
        val row = numOfBox / 3 * 3
        val col = (numOfBox % 3) * 3

        for (r <- row until row + 3;
             c <- col until col + 3) {
          //returning the previous selection to normal color
          newSudokuBoard.allSudokuFields(r)(c).background = color
        }
      }

      changeColor(currentPosition._1, currentPosition._2, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColorOfBox((currentPosition._1 / 3) * 3 + currentPosition._2 / 3, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)

      changeColor(row, col, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)
      changeColorOfBox((row / 3) * 3 + col / 3, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)

      newSudokuBoard.allSudokuFields(row)(col).background = GameLookConstants.SELECTED_BUTTON_BACKGROUND_COLOR
    }

    setBoardColors
    setCurrentPosition((row, col))
  }

  /**
   * Inserts a selected number on the sudoku field with all the necessary checks
   *
   * @param input
   */
  override def inputNumber(input: Int): Unit = {
    /**
     * Inserting the given input into sudoku table
     *
     * @return
     */
    def insertNumberOnBoard: Int = {
      //Checking to see if the user inputs the same number as it already is on the sudoku board
      if (board(currentPosition._1)(currentPosition._2) == input) {
        GameLookConstants.CODE_OK
      }
      else {
        //Updating the sudoku board and checking if the operation was valid
        //this order of calls must be held
        val checkForMoveValidation = checkIfMoveCorrect(currentPosition._1, currentPosition._2, input)
        board(currentPosition._1).update(currentPosition._2, input)
        if (checkForMoveValidation) {
          GameLookConstants.CODE_OK
        } else
          GameLookConstants.CODE_WARNING
      }
    }

    //Change the board only if the current position is valid (the sudoku board isn't finished)
    val insertNumOnBoardValidation = insertNumberOnBoard
    val curPosition: (Int, Int) = getCurrentPosition

    //Changing the new input field
    newSudokuBoard.allSudokuFields(currentPosition._1)(currentPosition._2).action = new Action(input.toString) {
      override def apply(): Unit = {
        positionChange(curPosition._1, curPosition._2)
      }
    }

    //Changing the color of the foreground, so the user knows which numbers have manually been set
    newSudokuBoard.allSudokuFields(currentPosition._1)(currentPosition._2).foreground = GameLookConstants.USER_INPUT_BOARD_NUMBER
    newSudokuBoard.listenTo(newSudokuBoard.allSudokuFields(currentPosition._1)(currentPosition._2))

    //Check to see if there were any errors and notify the user
    if (insertNumOnBoardValidation == GameLookConstants.CODE_WARNING) {
      newSudokuBoard.messageOutput.append("WARNING: The number " + input + " already exists in the same row, column or sub square" + '\n')
    }
  }

  /**
   * Erase a number on the current position on the sudoku board
   */
  override def eraseNumber: Unit = {
    board(currentPosition._1).update(currentPosition._2, 0)
    val curPosition: (Int, Int) = getCurrentPosition
    println(curPosition)
    //Changing the new input field
    newSudokuBoard.allSudokuFields(curPosition._1)(curPosition._2).action = new Action(" ") {
      override def apply(): Unit = SudokuBoard.positionChange(curPosition._1, curPosition._2)
    }
  }

  /**
   * Closing all the currently opened windows
   */
  override def closeWindows: Unit = {
    newSudokuBoard.visible = false
    newSudokuBoard.mainOwner.visible = true
    newSudokuBoard.dispose()
  }
}
