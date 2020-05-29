package GameBoard

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.io.{Codec, Source}
import GUI.{FinishedGameFrame, GameFrame, GameLookConstants}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.swing.{Action, Color, Frame}

object SudokuBoard extends Sudoku {
  //Set sudoku board playing field
  val fixedPositions = Array.ofDim[Boolean](9,9)

  private var gameFrame: GameFrame = null
  private var finishedGame: FinishedGameFrame = null

  /**
   * Setter for gameFrame
   *
   * @param mainOwner
   */
  def setGameFrame(mainOwner: Frame): Unit = {
    gameFrame = new GameFrame(mainOwner)
    //setting the first position for the beginning of the game
    positionChange(currentPosition._1,currentPosition._2)
  }

  /**
   * Resets the sudoku board
   *
   */
  def resetBoard = {
    board.map(col => col.map(x => 0))
    fixedPositions.map(col => col.map(x => false))
  }


  /**
   * Files out the sudoku from a file
   *
   * @param path
   */
  def fillSudoku(path: String): Unit = {
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
          fixedPositions(myRow).update(myCol, false)

          fillOutSudokuField(xs,myRow, myCol + 1)
        }
        case x :: xs if (x == 'P') => {
          board(myRow).update(myCol, 0)
          fixedPositions(myRow).update(myCol, false)
          setCurrentPosition(myRow, myCol)
          setStartingPosition(myRow, myCol)

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

  //-------------------------------- Actions -----------------------------------

  /**
   * Reads instructions that are implemented to the sudoku board. The instructions are taken from a defined
   * file path
   *
   * @param path
   */
  def readInstructionsFromFile(path: String) = {
    def goThroughRows(allMyRows: List[String]): Unit = {
      allMyRows match {
        case x :: xs => {
          goThroughChars(x.toList)

          //recursive call for the next row
          goThroughRows(xs)
        }
        case x :: Nil => {
          goThroughChars(x.toList)
          //No recursive call
        }
        case Nil => println("End of table")
      }
    }
    def goThroughChars(fieldValues: List[Char]): Unit ={
      fieldValues match {
        case x :: xs if (x == 'd') => {
          moveCurrentPositionDown
          goThroughChars(xs)
        }
        case x :: xs if (x == 'u')=>{
          moveCurrentPositionUp
          goThroughChars(xs)
        }
        case x :: xs if (x == 'l') => {
          moveCurrentPositionLeft
          goThroughChars(xs)
        }
        case x :: xs if (x == 'r')=>{
          moveCurrentPositionRight
          goThroughChars(xs)
        }
        case x :: xs if (x >= '1' && x <= '9')=>{
          inputNumber(x.asDigit)
          goThroughChars(xs)
        }
        case Nil => println("End of Row")
      }
    }

    //Calling the method goThroughRows to start the process of reading the lines
    goThroughRows(readFromFile(path))
  }

  /**
   * Solves the current sudoku board
   */
  override def solveSudoku: Boolean = {
    /*
    ----Backtracking works, but it's not good were there are a lot of different options for one field------

        def checkIfNumberPossibleAtPosition(row: Int, col: Int, number: Int): Boolean = {
      /**
       * Check to see if there is another field with the same number in the same row
       *
       * @return
       */
      def checkRow: Boolean = {
        !board(row).exists(x => x == number)
      }
      /**
       * Check to see if there is another field with the same number in the same column
       *
       * @return
       */
      def checkCol: Boolean = {
        val transBoard = board.transpose
        !transBoard(col).exists(x => x == number)
      }
      def checkSquare: Boolean = {
        !getAllFieldsFromSquare(row, col).exists(x => x == number)
      }

      checkRow && checkCol && checkSquare
    }


    def doSomething(row: Int, col: Int): Unit = {
      for (possible <- 1 to 9) {
        if (checkIfNumberPossibleAtPosition(row, col, possible)) {
          //println("(" + row + ", " + col + ") => " + possible)
          board(row).update(col, possible)
          solve
        }
      }
    }

    def solve: Unit = {
      for (r <- 0 to 8; c <- 0 to 8 if (board(r)(c) == 0)) {
        doSomething(r, c)
        if (!checkIfSudokuFinished) {
          //the previous try wasn't good so we return the field to 0
          board(r).update(c, 0)
        }
      }
    }

    solve
    println(showTable)

    */
    //Saving all the turns necessary to finish the sudoku
    var allTurns = new ListBuffer[String]()

    /**
     * Looks at the square of the current field and checks if there are any duplicates of numbers
     *
     * @param row
     * @param col
     * @param number
     * @return
     */
    def checkSquare(row: Int, col: Int, number: Int): Boolean = {
      !getAllFieldsFromSquare(board, row, col).exists(x => x == number)
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
            positionChange(possibilities.head._1, possibilities.head._2)
            inputNumber(currentNumber + 1)

            //Writing in the new number
            board(possibilities.head._1).update(possibilities.head._2, currentNumber + 1)
            //Writing the result into the file
            allTurns += ((currentNumber + 1) + " - (" + possibilities.head._1 +", " + possibilities.head._2 + ")")

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
      if (checkIfSudokuFinished(board) || !validationForTurn){
        //opening a file to write thw results in
        val file = new File("src/SudokuSolved/Solved.txt")
        val bw = new BufferedWriter(new FileWriter(file))

        for (line <- allTurns.toList)
          bw.write(line + '\n')

        if (checkIfSudokuFinished(board))
          bw.write("FINISHED!")
        else
          bw.write("CANNOT BE SOLVED")

        //close the file writer and return
        bw.close()

        if (checkIfSudokuFinished(board)){
          true
        }
        else {
          //Write message to user
          gameFrame.messageOutput.append("THERE IS A MISTAKE! THE SUDOKU CANNOT BE SALVED!" + '\n')
          false
        }
      } else
        findResults
      }

    findResults
  }

  //-------------------------------- GUI Actions -------------------------------

  /**
   * Changing the sudoku table in real time in response to user movement on the sudoku field
   *
   * @param row
   * @param col
   */
  def positionChange (row: Int, col: Int): Unit = {

    def setBoardColors: Unit = {
      def changeColor(r: Int, c: Int, color: Color) = {
        for (iter <- 0 to 8) {
          //returning the previous selection to normal color
          gameFrame.allSudokuFields(r)(iter).background = color
          gameFrame.allSudokuFields(iter)(c).background = color
        }
      }

      /**
       * Recives the number of box and the color the backgroun should be changed to
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

      changeColor(currentPosition._1, currentPosition._2, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColorOfBox((currentPosition._1 / 3) * 3 + currentPosition._2 / 3, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)

      changeColor(row, col, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)
      changeColorOfBox((row / 3) * 3 + col / 3, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)

      gameFrame.allSudokuFields(row)(col).background = GameLookConstants.SELECTED_BUTTON_BACKGROUND_COLOR
    }

    setBoardColors
    setCurrentPosition((row, col))
  }

  /**
   * Inserting a selected number on the sudoku field with all the necessary checks
   *
   * @param input
   */
  def inputNumber(input: Int) = {
    /**
     * Inserting the given input into sudoku table
     *
     * @return
     */
    def insertNumberOnBoard: Int = {
      //Checking to see if the user inputs the same number as it already is on the sudoku board
      if (board(currentPosition._1)(currentPosition._2) == input){
        GameLookConstants.CODE_OK
      }
      else {
        if (fixedPositions(currentPosition._1)(currentPosition._2))
        //Nothing happens because it is not allowed to change an original board numbers
        GameLookConstants.CODE_ERROR
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
    }
    /**
     * After checking to see if the input value can be inserted and knowing that is not allowed
     * this function is called to get the message why the operation wac declined
     *
     * @param code
     * @return
     */
    def getRefusalText(code: Int): String = {
      code match {
        case GameLookConstants.CODE_ERROR => "ERROR: Chosen field is part of the original sudoku board"
        case GameLookConstants.CODE_WARNING => "WARNING: (" + currentPosition._1 + ", " + currentPosition._2 + ") The number " + input + " already exists" +
          "in the same row, column or square"
      }
    }

    //Change the board only if the current position is valid (the sudoku board isn't finished)
    if (!(checkIfSudokuFinished(board) && checkIfSudokuCorrect(board))) {
      val insertNumOnBoardValidation = insertNumberOnBoard

      if (insertNumOnBoardValidation != GameLookConstants.CODE_ERROR) {
        val currentPosition: (Int, Int) = getCurrentPosition

        //Changing the new input field
        gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).action = new Action(input.toString) {
          override def apply(): Unit = {
            positionChange(currentPosition._1, currentPosition._2)
          }
        }

        //Changing the color of the foreground, so the user knows which numbers have manually been set
        gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).foreground = GameLookConstants.USER_INPUT_BOARD_NUMBER
        gameFrame.listenTo(gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2))

        //Checking to see if the user finished the game
        if (checkIfSudokuFinished(board))
          if (checkIfSudokuCorrect(board)) {
            finishSudoku
          } else {
            gameFrame.messageOutput.append("THE BOARD IS FILLED, BUT THERE IS A MISTAKE" + '\n')
          }
      }
      //If the game is not over, check to see if there were any errors and notify the user
      if (insertNumOnBoardValidation != GameLookConstants.CODE_OK) {
        gameFrame.messageOutput.append(getRefusalText(insertNumOnBoardValidation) + '\n')
      }
    }
  }

  /**
   * Erasing a a number on the current position on the sudoku board
   */
  def eraseNumber: Unit = {
    /**
     * Emptying out a selected field if it is not from the original sudoku board
     *
     * @return
     */
    def eraseNumberFromBoard: Int = {
      if (fixedPositions(currentPosition._1)(currentPosition._2))
        GameLookConstants.CODE_ERROR
      else{
        board(currentPosition._1).update(currentPosition._2, 0)
        GameLookConstants.CODE_OK
      }
    }

    if (!(checkIfSudokuFinished(board) && checkIfSudokuCorrect(board))) {
      val eraseNumberValidation = eraseNumberFromBoard
      if (eraseNumberValidation == GameLookConstants.CODE_OK) {
        val currentPosition: (Int, Int) = SudokuBoard.getCurrentPosition

        //Changing the new input field
        gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).action = new Action(" ") {
          override def apply(): Unit = SudokuBoard.positionChange(currentPosition._1, currentPosition._2)
        }
      }
      else {
        gameFrame.messageOutput.append("ERROR: Chosen field is part of the original sudoku board" + '\n')
      }
    }
  }

  /**
   * Opens the finished game frame
   */
  def finishSudoku: Unit = {
    finishedGame = new GUI.FinishedGameFrame(gameFrame.mainOwner, gameFrame)
  }

  /**
   * Finishing the game and closing the game windows
   */
  def closeWindows: Unit = {
    if (finishedGame != null) {
      finishedGame.visible = false
      finishedGame.dispose()
    }
    gameFrame.visible = false
    gameFrame.mainOwner.visible = true
    gameFrame.dispose()
  }
}
