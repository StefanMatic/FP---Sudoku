package GameBoard

import java.io.{BufferedWriter, File, FileWriter}

import GUI.{FinishedGameFrame, GameFrame, GameLookConstants}

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.swing.{Action, Color, Frame}

class SudokuBoard(path: String, mainOwner: Frame) {
  //Set sudoku board playing field
  val sudokuTable: SudokuMatrix = fillSudoku(path)
  var positions: Positions = findStartPosition(sudokuTable)

  //Frames
  val gameFrame: GameFrame = setGameFrame(mainOwner)
  val finishedGame: FinishedGameFrame = setFinishGame(mainOwner)

  /**
   * Setter for gameFrame
   *
   * @param mainOwner
   */
  def setGameFrame(mainOwner: Frame): GameFrame = {
    //gameFrame = new GameFrame(mainOwner)
    //setting the first position for the beginning of the game
    //positionChange(positions.currentPosition._1, positions.currentPosition._2)

    val myGameFrame = new GameFrame(mainOwner, this)
    //setting the first position for the beginning of the game
    callPositionChange(positions.currentPosition._1, positions.currentPosition._2, myGameFrame)

    myGameFrame
  }

  /**
   * Setter for FinishedGame
   *
   * @param mainOwner
   * @return
   */
  def setFinishGame(mainOwner: Frame): FinishedGameFrame = {
    val finishedGame = new GUI.FinishedGameFrame(gameFrame.mainOwner, gameFrame, this)
    finishedGame.visible = false

    finishedGame
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
        case Nil =>
      }
    }
    def goThroughChars(fieldValues: List[Char]): Unit ={
      fieldValues match {
        case x :: xs if (x == 'd') => {
          moveSingleStepDown
          goThroughChars(xs)
        }
        case x :: xs if (x == 'u')=>{
          moveSingleStepUp
          goThroughChars(xs)
        }
        case x :: xs if (x == 'l') => {
          moveSingleStepLeft
          goThroughChars(xs)
        }
        case x :: xs if (x == 'r')=>{
          moveSingleStepRight
          goThroughChars(xs)
        }
        case x :: xs if (x >= '1' && x <= '9')=>{
          inputNumber(x.asDigit)
          goThroughChars(xs)
        }
        case x :: xs => {
          goThroughChars(xs)
        }
        case Nil =>
      }
    }

    //Calling the method goThroughRows to start the process of reading the lines
    goThroughRows(readFromFile(path))
  }

  /**
   * Solves the current sudoku board
   */
  def solveSudoku(table: SudokuMatrix): Boolean = {
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
      !getAllFieldsFromSquare( row, col, getMatrixOfValue(table)).exists(x => x == number)
    }
    /**
     * Getting all the rows in witch the requested number doesn't appear
     *
     * @param number
     * @return
     */
    def getAllRowsWithoutNumber(number: Int, table: SudokuMatrix): List[Int] = {
      val allUnusedRows =
        for (row <- 0 to 8 if (!table(row).exists(x => x._1 == number)))
          yield row

      allUnusedRows.toList
    }
    /**
     * Getting all the possible fields on the board for a requested number. This function checks if all the fields
     * don't already have the same number in the same row, column or sub-square
     *
     * @param number
     * @param table
     * @return
     */
    def getAllPossibleFieldsForNumber(number: Int, table: SudokuMatrix): List[(Int, Int)] = {
      val allRows = getAllRowsWithoutNumber(number, table)
      val allCols = getAllRowsWithoutNumber(number, table.transpose)

      val allPossibleFields =
        for (rows <- allRows; cols <- allCols if (checkSquare(rows, cols, number) && table(rows)(cols)._1 == 0))
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
            callPositionChange(possibilities.head._1, possibilities.head._2, gameFrame)
            inputNumber(currentNumber + 1)

            //Writing in the new number
            table(possibilities.head._1).update(possibilities.head._2, (currentNumber + 1, false))
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
          yield getAllPossibleFieldsForNumber(number, sudokuTable)) zipWithIndex

      //try to find even one new move
      val validationForTurn: Boolean = allPossibilitiesForAllNumbers.exists(x => checkIfNumberCanBeUsed(x._1, x._2))

      //if a new move exists, the algorithm continuous
      if (checkIfSudokuFinished(getMatrixOfValue(sudokuTable)) || !validationForTurn){
        //opening a file to write thw results in
        val file = new File("src/SudokuSolved/Solved.txt")
        val bw = new BufferedWriter(new FileWriter(file))

        for (line <- allTurns.toList)
          bw.write(line + '\n')

        if (checkIfSudokuFinished(getMatrixOfValue(sudokuTable)))
          bw.write("FINISHED!")
        else
          bw.write("CANNOT BE SOLVED")

        //close the file writer and return
        bw.close()

        if (checkIfSudokuFinished(getMatrixOfValue(sudokuTable))){
          true
        }
        else {
          //Write a message to user
          gameFrame.messageOutput.append("THERE IS A MISTAKE! THE SUDOKU CANNOT BE SOLVED!" + '\n')
          false
        }
      } else
        findResults
      }

    findResults
  }

  //-------------------------------- GUI Actions -------------------------------

  /**
   * Moving single step up on board
   */
  def moveSingleStepUp: Unit = {
    val pos: Positions = positions.moveCurrentPositionUp
    callPositionChange(pos.currentPosition._1, pos.currentPosition._2, gameFrame)
  }

  /**
   * Moving single step down on board
   */
  def moveSingleStepDown: Unit = {
    val pos: Positions = positions.moveCurrentPositionDown
    callPositionChange(pos.currentPosition._1, pos.currentPosition._2, gameFrame)
  }

  /**
   * Moving single step right on board
   */
  def moveSingleStepRight: Unit = {
    val pos: Positions = positions.moveCurrentPositionRight
    callPositionChange(pos.currentPosition._1, pos.currentPosition._2, gameFrame)
  }

  /**
   * Moving single step left on board
   */
  def moveSingleStepLeft: Unit = {
    val pos: Positions = positions.moveCurrentPositionLeft
    callPositionChange(pos.currentPosition._1, pos.currentPosition._2, gameFrame)
  }

  /**
   * Centralising the use of side-effect of position change
   *
   * @param row
   * @param col
   * @param gameFrame
   */
  def callPositionChange(row: Int, col:Int, gameFrame: GameFrame): Unit = {
    positions = positionChange(row, col, gameFrame)
  }

  /**
   * Changing the sudoku table in real time in response to user movement on the sudoku field
   *
   * @param row
   * @param col
   */
  def positionChange (row: Int, col: Int, gameFrame: GameFrame): Positions = {

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

      changeColor(positions.currentPosition._1, positions.currentPosition._2, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColorOfBox((positions.currentPosition._1 / 3) * 3 + positions.currentPosition._2 / 3, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)

      changeColor(row, col, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)
      changeColorOfBox((row / 3) * 3 + col / 3, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)

      gameFrame.allSudokuFields(row)(col).background = GameLookConstants.SELECTED_BUTTON_BACKGROUND_COLOR
    }

    setBoardColors
    positions.changeCurrentPosition(row, col)
  }

  /**
   * Inserting a selected number on the sudoku field with all the necessary checks
   *
   * @param input
   */
  def inputNumber(input: Int) = {

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
        case GameLookConstants.CODE_WARNING => "WARNING: (" + positions.currentPosition._1 + ", " + positions.currentPosition._2 + ") The number " + input + " already exists" +
          "in the same row, column or square"
      }
    }

    //Change the board only if the current position is valid (the sudoku board isn't finished)
    if (!(checkIfSudokuFinished(getMatrixOfValue(sudokuTable)) && checkIfSudokuCorrect(getMatrixOfValue(sudokuTable)))) {
      val insertNumOnBoardValidation = insertNumberOnBoard(positions.currentPosition._1, positions.currentPosition._2, input, sudokuTable)

      if (insertNumOnBoardValidation != GameLookConstants.CODE_ERROR) {
        val currentPosition: (Int, Int) = positions.currentPosition

        //Changing the new input field
        gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).action = new Action(input.toString) {
          override def apply(): Unit = {
            callPositionChange(currentPosition._1, currentPosition._2, gameFrame)
          }
        }
        //Changing the color of the foreground, so the user knows which numbers have manually been set
        gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).foreground = GameLookConstants.USER_INPUT_BOARD_NUMBER
        gameFrame.listenTo(gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2))

        //Checking to see if the user finished the game
        if (checkIfSudokuFinished(getMatrixOfValue(sudokuTable)))
          if (checkIfSudokuCorrect(getMatrixOfValue(sudokuTable))) {
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
    if (!(checkIfSudokuFinished(getMatrixOfValue(sudokuTable)) && checkIfSudokuCorrect(getMatrixOfValue(sudokuTable)))) {
      val eraseNumberValidation = eraseNumberFromBoard(positions.currentPosition._1, positions.currentPosition._2, sudokuTable)
      if (eraseNumberValidation == GameLookConstants.CODE_OK) {
        val currentPosition: (Int, Int) = positions.currentPosition

        //Changing the new input field
        gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).action = new Action(" ") {
          override def apply(): Unit = {
            callPositionChange(currentPosition._1, currentPosition._2, gameFrame)
          }
        }
      }
      else {
        gameFrame.messageOutput.append("ERROR: Chosen field is part of the original sudoku board" + '\n')
      }
    }
  }

  /**
   * Makes the finished game frame
   */
  def finishSudoku: Unit = {
    //finishedGame = new GUI.FinishedGameFrame(gameFrame.mainOwner, gameFrame)
    finishedGame.visible = true
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
