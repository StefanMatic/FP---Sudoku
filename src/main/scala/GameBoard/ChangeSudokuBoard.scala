package GameBoard

import java.io.{BufferedWriter, File, FileWriter}

import GUI._

import scala.swing.{Action, Color}
import scala.language.postfixOps
import scala.swing._


class ChangeSudokuBoard(path: String, mainOwner: Frame) {
  type FunctionWrapper = ((Int, Int)) => (Int, Int)
  type FunctionListType = List[(String, List[FunctionWrapper])]

  //Set sudoku board playing field
  val sudokuTable: SudokuMatrix = fillSudoku(path)
  var positions: Positions = findStartPosition(sudokuTable)

  var functionList: FunctionListType = makeInitFunctionButtons
  val newSudokuBoard: NewSudokuBoardFrame = setGameFrame(mainOwner)

  /**
   * Setter for NewSudokuBoard
   *
   * @param mainOwner
   */
  def setGameFrame(mainOwner: Frame): NewSudokuBoardFrame = {
    val newBoard = new NewSudokuBoardFrame(mainOwner, this)
    //setting the start position for the beginning of the game

    positions = positionChange(positions.currentPosition._1, positions.currentPosition._2, newBoard)
    checkSaveButton(newBoard)

    newBoard
  }

  //-------------------------------- Actions -----------------------------------

  /**
   * Solves the current sudoku board
   */
   def solveSudoku: Boolean = {
    //making a copy of board to solve
    val boardCopy: SudokuMatrix = sudokuTable.map(_.clone)

    /**
     * Looks at the square of the current field and checks if there are any duplicates of numbers
     *
     * @param row
     * @param col
     * @param number
     * @return
     */
    def checkSquare(row: Int, col: Int, number: Int): Boolean = {
      !getAllFieldsFromSquare( row, col, getMatrixOfValue(boardCopy)).exists(x => x == number)
    }
    /**
     * Getting all the rows in witch the requested number doesn't appear
     *
     * @param table
     * @param number
     * @return
     */
    def getAllRowsWithoutNumber(number: Int, table: Matrix): List[Int] = {
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
      val allRows = getAllRowsWithoutNumber(number, getMatrixOfValue(boardCopy))
      val allCols = getAllRowsWithoutNumber(number, getMatrixOfValue(boardCopy.transpose))

      val allPossibleFields =
        for (rows <- allRows; cols <- allCols if (checkSquare(rows, cols, number) &&  boardCopy(rows)(cols)._1 == 0))
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
            boardCopy(possibilities.head._1).update(possibilities.head._2, (currentNumber + 1, false))

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
      if (checkIfSudokuFinished(getMatrixOfValue(boardCopy)) || !validationForTurn){
        //If it's finished, that means the sudoku game is solvable
        if (checkIfSudokuFinished(getMatrixOfValue(boardCopy)) && checkIfSudokuCorrect(getMatrixOfValue(boardCopy))) {
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

  /**
   * Making initial list of basic board manipulation functions
   *
   * @return
   */
  def makeInitFunctionButtons: FunctionListType = {
    val list1 = ("Input number", List[FunctionWrapper](inputNumberWrapper))
    val list2 = ("Erase number", List[FunctionWrapper](eraseNumberWrapper))
    val list3 = ("Change start position", List[FunctionWrapper](changeStartPositionWrapper))
    val list4 = ("Filter row and column", List[FunctionWrapper](filterRowAndColWrapper))
    val list5 = ("Filter square", List[FunctionWrapper](filterSquareWrapper))
    val list6 = ("Transpose", List[FunctionWrapper](transposeWrapper))
    val list7 = ("Change up", List[FunctionWrapper](changeUpWrapper))

    list1 :: list2 :: list3 :: list4 :: list5 :: list6 :: list7 :: Nil
  }

  /**
   * After making a new sequence or composite function, this method adds the function to the global list
   *
   * @param functionName
   * @param funcList
   */
  def addFunctionToList(functionName: String, funcList: List[FunctionWrapper]) = {
    functionList = functionList ::: List((functionName, funcList))
    newSudokuBoard.addFunction(functionName, funcList)
  }

  /**
   * Going through given list of function and executing them one by one. If there are more function than 1 in the list
   * the called function get a signal through their arguments
   *
   * @param myFunctions
   */
  def executeFunctionList(myFunctions: List[FunctionWrapper]): Unit = {
    if (myFunctions.length == 1) {
      myFunctions.head(positions.currentPosition._1, positions.currentPosition._2)
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
   */
  def checkSaveButton(newSudokuBoardFrame: NewSudokuBoardFrame): Unit = {
    val sudokuName: String = newSudokuBoardFrame.sudokuName.text

    //Checking if the sudoku board is solvable and if the user gave a name for the new table
    if (solveSudoku && !sudokuName.equals("")){
      newSudokuBoardFrame.saveSudoku.enabled = true
    } else {
      newSudokuBoardFrame.saveSudoku.enabled = false
    }
  }

  /**
   * Transposition the sudoku board
   */
  def transposition: Unit = {
    val manipulatedBoard = sudokuTable.transpose
    manipulatedBoard.copyToArray(sudokuTable)

    def changeBoardGUI: SudokuMatrix => Unit = changeBoard(changeBoardFieldsGUI)
    changeBoardGUI(sudokuTable)

    //Setting the board colors in the previous order
    positions = positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
    checkSaveButton(newSudokuBoard)
  }

  /**
   * Making a new table in which every element is 9-current value
   */
  def changeUp: Unit = {
    val manipulatedBoard = sudokuTable.map(col => col.map(el => (9 - el._1, el._2)))
    manipulatedBoard.copyToArray(sudokuTable)

    def changeBoardGUI: SudokuMatrix => Unit = changeBoard(changeBoardFieldsGUI)

    changeBoardGUI(sudokuTable)
    //Setting the board colors in the previous order
    positions = positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
    checkSaveButton(newSudokuBoard)
  }

  /**
   * Changing the current starting position
   *
   * @param row
   * @param col
   */
  def changeStartingPosition(row: Int, col: Int): Unit = {
    restartButtonColor(positions.startingPosition._1, positions.startingPosition._2, newSudokuBoard)
    positions = positions.changeStartingPosition(row, col)
    positions = positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
  }

  /**
   * Updating the board and GUI
   *
   * @param row
   * @param column
   * @param input
   */
  def updateBoardAndFields(row: Int, column: Int, input: Int): Unit = {
    val previousValue: (Int, Boolean) = sudokuTable(row)(column)
    sudokuTable(row).update(column, (input, previousValue._2))

    changeBoardFieldsGUI(row, column, input)
  }

  /**
   * Filtering out all the appearances of selected number in the same row and column
   *
   * @param row
   * @param col
   */
  def filterRowAndColumn(row: Int, col: Int): Unit = {
    val numberToFilterOut: Int = sudokuTable(row)(col)._1

    def filterRow: Unit = {
      //Finding the column value of the number to filter
      val colOfNumber = sudokuTable(row).indexWhere(x => x._1 == numberToFilterOut)
      //Checking to see if the number exists in row
      if (colOfNumber != -1) {
        sudokuTable(row).update(colOfNumber, (0, false))
        changeBoardFieldsGUI(row, colOfNumber, 0)

        //Checking to see if there are more numbers in the same row
        if (sudokuTable(row).exists(x => x._1 == numberToFilterOut))
          filterRow
      }
    }

    def filterCol: Unit = {
      val boardTransposed = sudokuTable.transpose
      val rowOfNumber = boardTransposed(col).indexWhere(x => x._1 == numberToFilterOut)
      //Checking to see if the number exists in column
      if (rowOfNumber != -1) {
        sudokuTable(rowOfNumber).update(col, (0, false))
        changeBoardFieldsGUI(rowOfNumber, col, 0)

        //Checking to see if there are more numbers in the same row
        if (boardTransposed(col).exists(x => x._1 == numberToFilterOut))
          filterCol
      }
    }

    //If the number to filter is different from 0 - filter row and column
    if (numberToFilterOut != 0){
      filterRow
      filterCol
      updateBoardAndFields(row, col, numberToFilterOut)
    }
    checkSaveButton(newSudokuBoard)
  }

  /**
   * Filtering out all the appearances of selected number in the same sub square
   *
   * @param row
   * @param col
   */
  def filterSubSquare(row: Int, col: Int): Unit = {
    val numberToFilterOut: Int = sudokuTable(row)(col)._1
    val rowOfSquare: Int = (row / 3) * 3
    val colOfSquare: Int = (col / 3) * 3

    def filterSquare: Unit = {
      val squareFields: List[Int] = getAllFieldsFromSquare(row, col, getMatrixOfValue(sudokuTable))
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
    checkSaveButton(newSudokuBoard)
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
        if (sudokuTable(row)(col)._1 == 0){
          bw.write("-")
        } else {
          bw.write(sudokuTable(row)(col)._1.toString)
        }
      }

      val start: (Int, Int) = positions.startingPosition

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
    def writeToFile: SudokuMatrix => Unit = changeBoard(writeToFileFunction)

    writeToFile(sudokuTable)
    bw.close()
  }

  //---------------------------- Wrapper version -----------------------------

  /**
   * Checks to see if all the characters are numeric
   *
   * @param x
   * @return
   */
  private def isAllDigits(x: String) = x forall Character.isDigit

  /**
   * Getting the location of the next operation from user through a dialog
   *
   * @param reasonForAsking
   * @return
   */
  private def getRowAndCol(reasonForAsking: String, newSudokuBoardFrame: NewSudokuBoardFrame): (Int, Int) = {
    val rowAndColString = Dialog.showInput(newSudokuBoardFrame.contents.head, "Radi " + reasonForAsking + " dati broj reda i kolone u formi red,kolona (redovi i kolone se indeksiraju 0-8)" , initial="")
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
              getRowAndCol("tacnog unosa", newSudokuBoardFrame)
          } else {
            // Not digits
            getRowAndCol("tacnog unosa", newSudokuBoardFrame)
          }
        }
        else {
          getRowAndCol("tacnog unosa", newSudokuBoardFrame)
        }
      }
      case None => {
        //If the user doesn't give a value, the function will take the currentPosition of game
        (positions.currentPosition._1, positions.currentPosition._2)
      }
    }
  }

  /**
   * Getting the number for input into a certain field
   *
   * @return
   */
  private def getNumberForInput(newSudokuBoardFrame: NewSudokuBoardFrame): Int = {
    val retrieve = Dialog.showInput(newSudokuBoardFrame.contents.head, "Koji broj zelite da unesete", initial = "")
    retrieve match {
      case Some(s) => {
        val inputString: String = s.trim

        if (isAllDigits(inputString)){
          val inputValue = inputString.toInt
          if (inputValue >= 1 && inputValue <= 9) {
            inputValue
          } else {
            //Again because of bad formatting
            getNumberForInput(newSudokuBoardFrame)
          }
        } else {
          // Again because of non digit characters
          getNumberForInput(newSudokuBoardFrame)
        }
      }
      case None => {
        //return a bad number to signal the program to ignore operation
        -1
      }
    }
  }

  /**
   * Changing the start position wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def changeStartPositionWrapper(position: (Int, Int)): (Int, Int) = {
    println("change start position")
    if (position._1 == -1) {
      val pos: (Int, Int) = getRowAndCol("PROMENE POCETNE POZICIJE", newSudokuBoard)
      //If wrong format than repeat process
      if (pos._1 == -1)
        changeStartPositionWrapper((-1, -1))
      changeStartingPosition(pos._1, pos._2)
    } else {
      changeStartingPosition(position._1, position._2)
    }

    //returning the given position
    position
  }

  /**
   * Input number wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def inputNumberWrapper(position: (Int, Int)): (Int, Int) = {
    println("input number")
    if (position._1 == -1) {
      val pos: (Int, Int) = getRowAndCol("UNOSA BROJA", newSudokuBoard)
      //If wrong format than repeat process
      if (pos._1 == -1)
        inputNumberWrapper(-1, -1)

      positions = positionChange(pos._1, pos._2, newSudokuBoard)

      val numberForInput = getNumberForInput(newSudokuBoard)
      //If number valid do operation, else ignore
      if (numberForInput != -1)
        inputNumber(numberForInput)
    } else{

      val numberForInput = getNumberForInput(newSudokuBoard)
      //If number valid do operation, else ignore
      if (numberForInput != -1)
        inputNumber(numberForInput)
    }

    //returning the row and column
    position
  }

  /**
   * Erase number wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def eraseNumberWrapper(position: (Int, Int)): (Int, Int) = {
    println("erase number")
    if (position._1 == -1) {
      val pos: (Int, Int) = getRowAndCol("BRISANJE BROJA", newSudokuBoard)
      //If wrong format than repeat process
      if (pos._1 == -1) {
        eraseNumberWrapper((-1, -1))
      }

      positions = positionChange(pos._1, pos._2, newSudokuBoard)
      // Applying the function to the new pos._1 and pos._2
      eraseNumber
    } else {
      eraseNumber
    }
    //returning the row and column
    position
  }

  /**
   * Filter row and column wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def filterRowAndColWrapper(position: (Int, Int)): (Int, Int) = {
    println("filter row and col")
    if (position._1 == -1) {
      val pos: (Int, Int) = getRowAndCol("FILTRIRANJA REDOVA I KOLONA", newSudokuBoard)
      //If wrong format than repeat process
      if (pos._1 == -1) {
        filterRowAndColWrapper(-1, -1)
      }
      // Applying the function to the new pos._1 and pos._2
      filterRowAndColumn(pos._1, pos._2)
    } else {
      filterRowAndColumn(position._1, position._2)
    }
    //returning the row and column
    position
  }

  /**
   * Filter square wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def filterSquareWrapper(position: (Int, Int)): (Int, Int) = {
    println("filter square")
    if (position._1 == -1) {
      val pos: (Int, Int) = getRowAndCol("FILTRIRANJA KOCKE", newSudokuBoard)
      //If wrong format than repeat process
      if (pos._1 == -1) {
        filterSquareWrapper(-1, -1)
      }

      // Applying the function to the new pos._1 and pos._2
      filterSubSquare(pos._1, pos._2)
    } else {
      filterSubSquare(position._1, position._2)
    }

    position
  }

  /**
   * Change-up wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def changeUpWrapper(position: (Int, Int)): (Int, Int) = {
    println("change up")
    changeUp

    //returning the position
    position
  }

  /**
   * Translation wrapper. The two parameters are row and col that are either given through current position of user on the board
   * or by the dialog input from the user
   *
   * @param position
   */
  def transposeWrapper(position: (Int, Int)): (Int, Int) = {
    println("transposition")
    transposition

    //returning the position
    position
  }

  //-------------------------------- GUI Actions -------------------------------

  def moveSingleStepUp: Positions = {
    positions = positions.moveCurrentPositionUp
    positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
  }

  def moveSingleStepDown: Positions = {
    positions = positions.moveCurrentPositionDown
    positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
  }

  def moveSingleStepRight: Positions = {
    positions = positions.moveCurrentPositionRight
    positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
  }

  def moveSingleStepLeft: Positions = {
    positions = positions.moveCurrentPositionLeft
    positionChange(positions.currentPosition._1, positions.currentPosition._2, newSudokuBoard)
  }

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
            positions = positionChange(row, col, newSudokuBoard)
          }
        }
      } else {
        new Action(" ") {
          override def apply(): Unit = {
            positions = positionChange(row, col, newSudokuBoard)
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
  def showStartPosition(row: Int, col: Int, gameFrame: NewSudokuBoardFrame): Unit = {
    gameFrame.allSudokuFields(row)(col).background = GameLookConstants.lightGrayishLimeGreen
  }

  /**
   * Restarting the button background color to white
   *
   * @param row
   * @param col
   */
  def restartButtonColor(row: Int, col: Int, newSudokuBoardFrame: NewSudokuBoardFrame): Unit = {
    newSudokuBoardFrame.allSudokuFields(row)(col).background = GameLookConstants.white
  }

  /**
   * Changes the sudoku table in real time in response to user movement on the sudoku field
   *
   * @param row
   * @param col
   */
  def positionChange(row: Int, col: Int, gameFrame: NewSudokuBoardFrame): Positions = {

    def setBoardColors = {
      def changeColor(r: Int, c: Int, color: Color) = {
        for (iter <- 0 to 8) {
          //returning the previous selection to normal color
          gameFrame.allSudokuFields(r)(iter).background = color
          gameFrame.allSudokuFields(iter)(c).background = color
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
          gameFrame.allSudokuFields(r)(c).background = color
        }
      }

      changeColor(positions.currentPosition._1, positions.currentPosition._2, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColorOfBox((positions.currentPosition._1 / 3) * 3 + positions.currentPosition._2 / 3, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)

      changeColor(row, col, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)
      changeColorOfBox((row / 3) * 3 + col / 3, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)

      gameFrame.allSudokuFields(row)(col).background = GameLookConstants.SELECTED_BUTTON_BACKGROUND_COLOR
    }

    setBoardColors

    //While the sudoku is in edit mode, the user can always see the start position
    showStartPosition(positions.startingPosition._1, positions.startingPosition._2, gameFrame)

    positions.changeCurrentPosition(row, col)
  }

  /**
   * Inserts a selected number on the sudoku field with all the necessary checks
   *
   * @param input
   */
  def inputNumber(input: Int): Unit = {
    /**
     * Inserting the given input into sudoku table
     *
     * @return
     */
    def insertNumberOnBoard: Int = {
      //Checking to see if the user inputs the same number as it already is on the sudoku board
      if (sudokuTable(positions.currentPosition._1)(positions.currentPosition._2)._1 == input) {
        GameLookConstants.CODE_OK
      }
      else {
        //Updating the sudoku board and checking if the operation was valid
        //this order of calls must be held
        val checkForMoveValidation = checkIfMoveCorrect(positions.currentPosition._1, positions.currentPosition._2, input, getMatrixOfValue(sudokuTable))
        sudokuTable(positions.currentPosition._1).update(positions.currentPosition._2, (input, false))
        if (checkForMoveValidation) {
          GameLookConstants.CODE_OK
        } else
          GameLookConstants.CODE_WARNING
      }
    }

    //Change the board only if the current position is valid (the sudoku board isn't finished)
    val insertNumOnBoardValidation = insertNumberOnBoard
    val curPosition: (Int, Int) = positions.currentPosition

    //Changing the new input field
    changeBoardFieldsGUI(curPosition._1, curPosition._2, input)

    //Check to see if there were any errors and notify the user
    if (insertNumOnBoardValidation == GameLookConstants.CODE_WARNING) {
      newSudokuBoard.messageOutput.append("WARNING: (" + positions.currentPosition._1 + ", " + positions.currentPosition._2 + ") The number " + input + " already exists" +
        "in the same row, column or square" + '\n')
    }

    checkSaveButton(newSudokuBoard)
  }

  /**
   * Erase a number on the current position on the sudoku board
   */
  def eraseNumber: Unit = {
    sudokuTable(positions.currentPosition._1).update(positions.currentPosition._2, (0, false))
    val curPosition: (Int, Int) = positions.currentPosition

    //Changing the new input field
    changeBoardFieldsGUI(curPosition._1, curPosition._2, 0)

    checkSaveButton(newSudokuBoard)
  }

  /**
   * Closing all the currently opened windows
   */
  def closeWindows: Unit = {
    newSudokuBoard.visible = false
    newSudokuBoard.mainOwner.visible = true
    newSudokuBoard.dispose()
  }

  def addedFunctionMessage:Unit = {
    Dialog.showMessage(newSudokuBoard.contents.head, "Function added!", title="New function")
    changeUp
    changeUp
  }
}
