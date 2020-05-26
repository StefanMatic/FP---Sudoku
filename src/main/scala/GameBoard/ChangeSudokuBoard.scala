package GameBoard

import java.io.{BufferedWriter, File, FileWriter}

import GUI.{GameLookConstants, NewSudokuBoardFrame}

import scala.swing.{Action, Color}
import scala.language.postfixOps
import scala.swing._


object ChangeSudokuBoard extends Sudoku {
  type FunctionListType = List[(String, List[(Int, Int) => Unit])]

  val functionList: FunctionListType = makeInitFunctionButtons
  var newSudokuBoard: NewSudokuBoardFrame = null

  /**
   * Setter for NewSudokuBoard
   *
   * @param newBoard
   */
  def setGameFrameTable(newBoard: NewSudokuBoardFrame): Unit = {
    newSudokuBoard = newBoard
    //setting the start position for the beginning of the game

    positionChange(currentPosition._1,currentPosition._2)
    checkSaveButton
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
          setStartingPosition(myRow, myCol)
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

  /**
   * Solves the current sudoku board
   */
  override def solveSudoku: Boolean = {
    //making a copy of board to solve
    val boardCopy: Matrix = board.map(_.clone)

    /**
     * Looks at the square of the current field and checks if there are any duplicates of numbers
     *
     * @param row
     * @param col
     * @param number
     * @return
     */
    def checkSquare(row: Int, col: Int, number: Int): Boolean = {
      !getAllFieldsFromSquare(boardCopy, row, col).exists(x => x == number)
    }
    /**
     * Getting all the rows in witch the requested number doesn't appear
     *
     * @param table
     * @param number
     * @return
     */
    def getAllRowsWithoutNumber(table: Matrix, number: Int): List[Int] = {
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
      val allRows = getAllRowsWithoutNumber(boardCopy, number)
      val allCols = getAllRowsWithoutNumber(boardCopy.transpose, number)

      val allPossibleFields =
        for (rows <- allRows; cols <- allCols if (checkSquare(rows, cols, number) &&  boardCopy(rows)(cols) == 0))
          yield (rows, cols)

      allPossibleFields.toList
    }
    /**
     *  Try to solve the sudoku board
     */
    def findResults: Boolean = {
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
            //Writing in the new number
            boardCopy(possibilities.head._1).update(possibilities.head._2, currentNumber + 1)

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
      if (checkIfSudokuFinished(boardCopy) || !validationForTurn){
        //If it's finished, that means the sudoku game is solvable
        if (checkIfSudokuFinished(boardCopy) && checkIfSudokuCorrect(boardCopy)) {
          true
        } else {
          false
        }
      } else {
        //the algorithm isn't over yet - it's called recursively
        findResults
      }
    }

    findResults
  }

  //----------------------------- Table functions ------------------------------

  def makeInitFunctionButtons: FunctionListType = {
    val list1 = ("Unesi broj", List[(Int, Int) => Unit](inputNumberWrapper))
    val list2 = ("Izbrisi broj", List[(Int, Int) => Unit](eraseNumberWrapper))
    val list3 = ("Promeni pocetnu poziciju", List[(Int, Int) => Unit](changeStartPositionWrapper))
    val list4 = ("Filtritej po redu i koloni", List[(Int, Int) => Unit](filterRowAndColWrapper))
    val list5 = ("Filtritaj kocku", List[(Int, Int) => Unit](filterSquareWrapper))
    val list6 = ("Transponovanje", List[(Int, Int) => Unit](transposeWrapper))
    val list7 = ("Zamena", List[(Int, Int) => Unit](changeUpWrapper))

    list1 :: list2 :: list3 :: list4 :: list5 :: list6 :: list7 :: Nil
  }

  def addFunctionToList(functionName: String, funcList: List[(Int, Int) => Unit]) = {
    functionList ::: List((functionName, funcList))
  }

  def executeFunctionList(myFunctions: List[(Int, Int) => Unit]): Unit = {
    if (myFunctions.length == 1) {
      myFunctions.head(currentPosition._1, currentPosition._2)
    }
    else {
      for (func <- myFunctions) {
        //Signal the method to ask for row and col
        func(-1, -1)
      }
    }
  }

  /**
   * Checking to see if the save new sudoku board is enabled or disabled
   *
   */
  def checkSaveButton: Unit = {
    val sudokuName: String = newSudokuBoard.sudokuName.text

    //Checking if the sudoku board is solvable and if the user gave a name for the new table
    if (solveSudoku && !sudokuName.equals("")){
      newSudokuBoard.saveSudoku.enabled = true
    } else {
      newSudokuBoard.saveSudoku.enabled = false
    }
  }

  /**
   * Transposition the sudoku board
   */
  def transposition: Unit = {
    val manipulatedBoard = board.transpose
    manipulatedBoard.copyToArray(board)

    def changeBoardGUI = changeBoard(changeBoardFieldsGUI)
    changeBoardGUI

    //Setting the board colors in the previous order
    positionChange(currentPosition._1, currentPosition._2)
    checkSaveButton
  }

  /**
   * Making a new table in which every element is 9-current value
   */
  def changeUp: Unit = {
    val manipulatedBoard = board.map(col => col.map(el => 9 - el))
    manipulatedBoard.copyToArray(board)

    def changeBoardGUI = changeBoard(changeBoardFieldsGUI)

    changeBoardGUI
    //Setting the board colors in the previous order
    positionChange(currentPosition._1, currentPosition._2)
    checkSaveButton
  }

  /**
   * Changing the current starting position
   *
   * @param row
   * @param col
   */
  def changeStartingPosition(row: Int, col: Int): Unit = {
    restartButtonColor(startPosition._1, startPosition._2)
    setStartingPosition(row, col)
    positionChange(currentPosition._1, currentPosition._2)
  }

  /**
   * Updating the board and GUI
   *
   * @param row
   * @param column
   * @param input
   */
  def updateBoardAndFields(row: Int, column: Int, input: Int): Unit = {
    board(row).update(column, input)
    changeBoardFieldsGUI(row, column, input)
  }

  /**
   * Filtering out all the appearances of selected number in the same row and column
   *
   * @param row
   * @param col
   */
  def filterRowAndColumn(row: Int, col: Int): Unit = {
    val numberToFilterOut: Int = board(row)(col)

    def filterRow: Unit = {
      //Finding the column value of the number to filter
      val colOfNumber = board(row).indexOf(numberToFilterOut)
      //Checking to see if the number exists in row
      if (colOfNumber != -1) {
        board(row).update(colOfNumber, 0)
        changeBoardFieldsGUI(row, colOfNumber, 0)

        //Checking to see if there are more numbers in the same row
        if (board(row).exists(x => x == numberToFilterOut))
          filterRow
      }
    }

    def filterCol: Unit = {
      val boardTransposed = board.transpose
      val rowOfNumber = boardTransposed(col).indexOf(numberToFilterOut)
      //Checking to see if the number exists in column
      if (rowOfNumber != -1) {
        board(rowOfNumber).update(col, 0)
        changeBoardFieldsGUI(rowOfNumber, col, 0)

        //Checking to see if there are more numbers in the same row
        if (boardTransposed(col).exists(x => x == numberToFilterOut))
          filterCol
      }
    }

    //If the number to filter is different from 0 - filter row and column
    if (numberToFilterOut != 0){
      filterRow
      filterCol
      updateBoardAndFields(row, col, numberToFilterOut)
    }
    checkSaveButton
  }

  /**
   * Filtering out all the appearances of selected number in the same sub square
   *
   * @param row
   * @param col
   */
  def filterSubSquare(row: Int, col: Int): Unit = {
    val numberToFilterOut: Int = board(row)(col)
    val rowOfSquare: Int = (row / 3) * 3
    val colOfSquare: Int = (col / 3) * 3

    def filterSquare: Unit = {
      val squareFields: List[Int] = getAllFieldsFromSquare(board, row, col)
      if (squareFields.exists(x => x == numberToFilterOut)) {
        val indexOfNum = squareFields.indexOf(numberToFilterOut)

        val rowInc = indexOfNum / 3
        val colInc = indexOfNum % 3

        //Updating the fields
        updateBoardAndFields(rowOfSquare + rowInc, colOfSquare + colInc, 0)

        //Checking to see if there are more of the same numbers in the square
        if (squareFields.indexOf(numberToFilterOut, indexOfNum) != -1)
          filterSquare
      }
    }

    if (numberToFilterOut != 0) {
      filterSquare

      //Putting back the original number
      updateBoardAndFields(row, col, numberToFilterOut)
    }
    checkSaveButton
  }

  /**
   * Saving the current board to a file with user given name
   */
  def saveNewBoard: Unit = {
    // Getting the name of the sudoku
    val newName = (newSudokuBoard.sudokuName.text).trim
    val fileName = "src/SudokuBoardExamples/" + newName + ".txt"

    // FileWriter
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))

    def writeToFileFunction(row: Int, col: Int, el: Int) = {
      def writeElOfBoard(row: Int, col: Int) = {
        if (board(row)(col) == 0){
          bw.write("-")
        } else {
          bw.write(board(row)(col).toString)
        }
      }

      val start = getStartingPosition

      (row, col) match  {
        case (r, c) if (r == start._1 && c == start._2) => {
          bw.write("P")
        }
        case (r, c) if (c == 8) => {
          writeElOfBoard(r, c)
          bw.write('\n')
        }
        case (r, c) => {
          writeElOfBoard(r, c)
        }
      }
    }
    def writeToFile = changeBoard(writeToFileFunction)

    writeToFile
    bw.close()
  }

  //---------------------------- Wrapper version -----------------------------

  /**
   * Checks to see if all the characters are numeric
   *
   * @param x
   * @return
   */
  def isAllDigits(x: String) = x forall Character.isDigit

  /**
   * Getting the location of the next operation from user
   *
   * @param reasonForAsking
   * @return
   */
  def getRowAndCol(reasonForAsking: String): (Int, Int) = {
    val rowAndColString = Dialog.showInput(newSudokuBoard.contents.head, "Radi " + reasonForAsking + " dati broj reda i kolone u formi red,kolona (redovi i kolone se indeksiraju 0-8)" , initial="")
    rowAndColString match {
      case Some(s) => {
        val rowAndCol: String = s.trim
        println(rowAndCol)
        if (rowAndCol.length == 3 && rowAndCol.contains(',')){
          val rowAndColValues: Array[String] = rowAndCol.split(',')
          if (rowAndColValues.forall(x => isAllDigits(x))) {
            if (rowAndColValues(0).toInt != 9 && rowAndColValues(1).toInt !=9)
              (rowAndColValues(0).toInt, rowAndColValues(1).toInt)
            else
              getRowAndCol("tacnog unosa")
          } else {
            // Not digits
            getRowAndCol("tacnog unosa")
          }
        }
        else {
          getRowAndCol("tacnog unosa")
        }
      }
      case None => {
        //If the user doesn't give a value, the function will take the currentPosition of game
        (currentPosition._1, currentPosition._2)
      }
    }
  }

  /**
   * Changing the start position wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def changeStartPositionWrapper(row: Int, col: Int): Unit = {
    if (row == -1) {
      val pos: (Int, Int) = getRowAndCol("PROMENE POCETNE POZICIJE")
      //If wrong format than repeat process
      if (pos._1 == -1)
        changeStartPositionWrapper(-1, -1)
      changeStartingPosition(pos._1, pos._2)
    } else {
      changeStartingPosition(row, col)
    }
  }

  /**
   * Input number wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def inputNumberWrapper(row: Int, col: Int): Unit = {
    if (row == -1) {
      val pos: (Int, Int) = getRowAndCol("UNOSA BROJA")
      //If wrong format than repeat process
      if (pos._1 == -1)
        inputNumberWrapper(-1, -1)

      val retrieve = Dialog.showInput(newSudokuBoard.contents.head, "Koji broj zelite da unesete", initial = "")

      retrieve match {
        case Some(s) => {
          val inputString: String = s.trim

          if (isAllDigits(inputString)){
            val inputValue = inputString.toInt
            if (inputValue >= 0 && inputValue <= 8)
              updateBoardAndFields(pos._1, pos._2, inputString.toInt)
            else {
              //Again because of bad formatting
              inputNumberWrapper(-1,-1)
            }
          }
        }
        case None => {
          //Do nothig, ignore the function
        }
      }
    } else{
      //Giving the user a chance to use the number picker
      newSudokuBoard.numPicker.visible = true
    }

  }

  /**
   * Erase number wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def eraseNumberWrapper(row: Int, col: Int): Unit = {
    newSudokuBoard.numPicker.visible = false
    if (row == -1) {
      val pos: (Int, Int) = getRowAndCol("BRISANJE BROJA")
      //If wrong format than repeat process
      if (pos._1 == -1) {
        eraseNumberWrapper(-1, -1)
      }

      // Applying the function to the new pos._1 and pos._2
      updateBoardAndFields(pos._1, pos._2, 0)
    } else {
      updateBoardAndFields(row, col, 0)
    }
  }

  /**
   * Filter row and column wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def filterRowAndColWrapper(row: Int, col: Int): Unit = {
    newSudokuBoard.numPicker.visible = false
    if (row == -1) {
      val pos: (Int, Int) = getRowAndCol("FILTRIRANJA REDOVA I KOLONA")
      //If wrong format than repeat process
      if (pos._1 == -1) {
        filterRowAndColWrapper(-1, -1)
      }

      // Applying the function to the new pos._1 and pos._2
      filterRowAndColumn(pos._1, pos._2)
    } else {
      filterRowAndColumn(row, col)
    }
  }

  /**
   * Filter square wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def filterSquareWrapper(row: Int, col: Int): Unit = {
    newSudokuBoard.numPicker.visible = false
    if (row == -1) {
      val pos: (Int, Int) = getRowAndCol("FILTRIRANJA KOCKE")
      //If wrong format than repeat process
      if (pos._1 == -1) {
        filterSquareWrapper(-1, -1)
      }

      // Applying the function to the new pos._1 and pos._2
      filterSubSquare(pos._1, pos._2)
    } else {
      filterSubSquare(row, col)
    }

  }

  /**
   * Change-up wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def changeUpWrapper(row: Int, col: Int): Unit = {
    newSudokuBoard.numPicker.visible = false
    changeUp
  }

  /**
   * Translation wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param row
   * @param col
   */
  def transposeWrapper(row: Int, col: Int): Unit = {
    newSudokuBoard.numPicker.visible = false
    transposition
  }

  //-------------------------------- GUI Actions -------------------------------

  /**
   * Changing the numbers on the board
   *
   * @param row
   * @param col
   * @param input
   */
  def changeBoardFieldsGUI(row: Int, col:Int, input: Int): Unit ={
    //Changing the new input field
    newSudokuBoard.allSudokuFields(row)(col).action =
      if (input != 0) {
        new Action(input.toString) {
          override def apply(): Unit = {
            positionChange(row, col)
          }
        }
      } else {
        new Action(" ") {
          override def apply(): Unit = {
            positionChange(row, col)
          }
        }
      }

    //Changing the color of the foreground, so the user knows which numbers have manually been set
    newSudokuBoard.allSudokuFields(row)(col).foreground = GameLookConstants.USER_INPUT_BOARD_NUMBER
    newSudokuBoard.listenTo(newSudokuBoard.allSudokuFields(row)(col))
  }

  /**
   * Changing the background color of the start position
   */
  def showStartPosition: Unit = {
    newSudokuBoard.allSudokuFields(startPosition._1)(startPosition._2).background = GameLookConstants.lightGrayishLimeGreen
  }

  /**
   * Restarting the button background color to white
   *
   * @param row
   * @param col
   */
  def restartButtonColor(row: Int, col: Int): Unit = {
    newSudokuBoard.allSudokuFields(row)(col).background = GameLookConstants.white

  }

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

    //While the sudoku is in edit mode, the user can always see the start position
    showStartPosition
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
    changeBoardFieldsGUI(curPosition._1, curPosition._2, input)

    //Check to see if there were any errors and notify the user
    if (insertNumOnBoardValidation == GameLookConstants.CODE_WARNING) {
      newSudokuBoard.messageOutput.append("WARNING: The number " + input + " already exists in the same row, column or sub square" + '\n')
    }

    checkSaveButton
  }

  /**
   * Erase a number on the current position on the sudoku board
   */
  override def eraseNumber: Unit = {
    board(currentPosition._1).update(currentPosition._2, 0)
    val curPosition: (Int, Int) = getCurrentPosition

    //Changing the new input field
    changeBoardFieldsGUI(curPosition._1, curPosition._2, 0)

    checkSaveButton
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
