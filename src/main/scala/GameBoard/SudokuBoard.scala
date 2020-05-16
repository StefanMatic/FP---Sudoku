package GameBoard

import scala.io.Source
import GUI.GameLookConstants
import GUI.GameFrame

import scala.swing.{Action, Color, Frame}

object SudokuBoard {
  //Set sudoku board playing field
  val board  = Array.ofDim[Int](9,9)
  val fixedPositions = Array.ofDim[Boolean](9,9)

  private var currentPosition: (Int, Int) = (0,0)
  private var gameFrame: GameFrame = null


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
  def setCurrentPosition(newPosition: (Int, Int)) = {
    currentPosition = newPosition
  }

  /**
   * Setter for gameFrame
   *
   * @param game
   */
  def setGameFrameTable(game: GameFrame): Unit = {
    gameFrame = game
    //setting the first position for the beginning of the game
    positionChange(0,0)
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
   * Reading all the lines from a .txt file
   *
   * @param path
   */
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

  //---------------------------- Chekers -------------------------------------

  /**
   * Getting all the fields from the square in witch the row and col point to
   *
   * @param row
   * @param col
   * @return
   */
  private def getAllFieldsFromSquare(row: Int, col: Int): List[Int] = {
    val helper = board.flatten.grouped(3).toArray

    val firstIndex = (row / 3) * 9 + col /3
    val secondIndex = firstIndex + 3
    val thirdIndex = firstIndex + 6

    //All the fields can be found in an increment of 3
    val mySquare = List(helper(firstIndex), helper(secondIndex), helper(thirdIndex)).flatten
    mySquare
  }

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
      val mySquare = getAllFieldsFromSquare(row, col)
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
  def checkIfSudokuFinished: Boolean = {
    def checkIfEmptyFieldExists(myArray: Array[Int]): Boolean = {
      //if there exists even one 0 in the array, the function will return !true (false)
      !myArray.exists(x=> x==0)
    }
    // returns true only if there are no 0 in any of the rows
    board.forall(arr => checkIfEmptyFieldExists(arr))
  }

  /**
   * Checks if the sudoku board is correctly filled
   *
   * @return
   */
  def checkIfSudokuCorrect: Boolean = {
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
      board.forall(x => checkIfRowCorrect(x))
    }
    def checkCols: Boolean = {
      board.transpose.forall(x => checkIfRowCorrect(x))
    }
    def checkSquare: Boolean = {
      val allSquaresCheckes =
        for (r <- 0 to 8 by 3; c <- 0 to 8 by 3)
          yield checkIfRowCorrect(getAllFieldsFromSquare(r,c).toArray)

      //TODO: Izbrisi ovo kada vidis da li radi posao
      println(allSquaresCheckes)
      !allSquaresCheckes.exists(x => x == false)
    }

    checkRows && checkCols && checkSquare
  }

  /**
   *Displays the sudoku board
   */
  def showTable: String = {
    board.map(col => col mkString(" ")).mkString("\n")
  }

  //-------------------------------- Actions -----------------------------------

  def readInstructionsFromFile(path: String) = {
    //Ovaj deo moze da se zameni sa gornjom funcijom za citanje iz fajla kada ude modularna
    val bufferedSource = Source.fromFile(path)
    val lines = bufferedSource.getLines().toList

    //closing the opened .txt file
    bufferedSource.close
    //------------------ nastavak------------------

    def goThroughRows(allMyRows: List[String]): Unit = {
      allMyRows match {
        case x :: xs => {
          goThroughChars(x.toList)

          //recursive call for the next row
          goThroughRows(xs)
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
    goThroughRows(lines)
  }

  //-------------------------------- GUI Actions -------------------------------

  /**
   * Changing the sudoku table in real time in response to user movement on the sudoku field
   *
   * @param row
   * @param col
   */
  def positionChange (row: Int, col: Int) = {

    def setBoardColors = {
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
      def changeColorOfBox(numOfBox: Int, color: Color):Unit = {
        val row = numOfBox / 3 * 3
        val col = (numOfBox % 3) * 3

        for (r <- row until  row + 3;
             c <- col until  col + 3) {
          //returning the previous selection to normal color
          gameFrame.allSudokuFields(r)(c).background = color
        }
      }

      changeColor(currentPosition._1, currentPosition._2, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColorOfBox((currentPosition._1 / 3) * 3 + currentPosition._2 / 3, GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR)

      changeColor(row, col, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)
      changeColorOfBox((row/3) * 3 + col / 3, GameLookConstants.SELECTED_BUTTON_AREA_BACKGROUND)

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
        case GameLookConstants.CODE_ERROR => "Odabrano polje pripada pocetnoj tabeli"
        case GameLookConstants.CODE_WARNING => "Vec postoji cifra " + input + " u istom redu, koloni ili kvadratu"
      }
    }

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

      //TODO: Dodaj da se prelazi na drugi prozor
      //Checking to see if the user finished the game
      if (checkIfSudokuFinished)
        println("Dobra - " + checkIfSudokuCorrect)
    }

    //If the game is not over, check to see if there were any errors and notify the user
    if (insertNumOnBoardValidation != GameLookConstants.CODE_OK){
      gameFrame.messageOutput.append(getRefusalText(insertNumOnBoardValidation) + '\n')
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

    val eraseNumberValidation = eraseNumberFromBoard
    if (eraseNumberValidation == GameLookConstants.CODE_OK) {
      val currentPosition: (Int, Int) = SudokuBoard.getCurrentPosition

      //Changing the new input field
      gameFrame.allSudokuFields(currentPosition._1)(currentPosition._2).action = new Action(" ") {
        override def apply(): Unit = SudokuBoard.positionChange(currentPosition._1, currentPosition._2)
      }
    }
    else {
      gameFrame.messageOutput.append("Odabrano polje pripada pocetnoj tabeli." + '\n')
    }
  }
}
