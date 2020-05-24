package GUI

import GameBoard.{ChangeSudokuBoard, SudokuBoard}

import scala.swing.event._
import scala.swing._

class NewSudokuBoardFrame(val mainOwner: Frame) extends Frame {
  val allSudokuFields  = Array.ofDim[Button](9,9)
  val messageOutput = new TextArea()

  //method for making uniform buttons with the predeclared NUMBER_FONT
  def makeButtonNumbersFont(name: String): Button = {
    val myButton = new Button(name)

    myButton.xLayoutAlignment = 0.5f
    myButton.margin = new Insets(15, 15, 15, 15)
    myButton.font = GameLookConstants.NUMBERS_FONT
    myButton.background = GameLookConstants.MENU_BUTTON_BACKGROUND
    myButton.foreground = GameLookConstants.MENU_BUTTON_FOREGROUND
    myButton
  }

  /**
   * Creating the sudoku table and filling it with data
   *
   * @return
   */
  def sudokuTable: GridPanel = {
    def makePanelWithBorder(r: Int, c:Int) :GridPanel = {
      val newPanel = new GridPanel(r,c)
      newPanel.border = Swing.LineBorder(new Color(0,0,0), 2)
      newPanel.focusable = true
      newPanel.requestFocus()

      newPanel
    }

    //The variable 'panel' is accessible from every local function
    val panel = makePanelWithBorder(3,3)

    //sub-boxes of sudoku table
    val grid0 = makePanelWithBorder(3,3)
    val grid1 = makePanelWithBorder(3,3)
    val grid2 = makePanelWithBorder(3,3)
    val grid3 = makePanelWithBorder(3,3)
    val grid4 = makePanelWithBorder(3,3)
    val grid5 = makePanelWithBorder(3,3)
    val grid6 = makePanelWithBorder(3,3)
    val grid7 = makePanelWithBorder(3,3)
    val grid8 = makePanelWithBorder(3,3)

    def fillGrid ={
      fillAllRows(0)
    }
    def fillAllRows(row: Int): Unit = {
      if (row != 9) {
        fillAllCol(row, 0)
        fillAllRows(row + 1)
      }
    }
    def fillAllCol(row: Int, col: Int): Unit = {
      if (col != 9){
        val newButton: Button = new Button()
        val newButtonAction: Action =
          if (ChangeSudokuBoard.board(row)(col) != 0)
            new Action(ChangeSudokuBoard.board(row)(col).toString) {
              override def apply(): Unit = ChangeSudokuBoard.positionChange(row,col)
            }
          else {
            new Action(" ") {
              override def apply(): Unit = ChangeSudokuBoard.positionChange(row,col)
            }
          }

        newButton.foreground = GameLookConstants.ORIGINAL_BOARD_NUMBER
        newButton.action = newButtonAction
        newButton.background = GameLookConstants.UNSELECTED_BUTTON_BACKGROUND_COLOR
        newButton.font = GameLookConstants.DEFAULT_FONT

        //storing the new button in our button matrix and putting it on the grid panel
        allSudokuFields(row).update(col, newButton)
        ((row/3) * 3 + col/3) match {
          case 0 => grid0.contents += newButton
          case 1 => grid1.contents += newButton
          case 2 => grid2.contents += newButton
          case 3 => grid3.contents += newButton
          case 4 => grid4.contents += newButton
          case 5 => grid5.contents += newButton
          case 6 => grid6.contents += newButton
          case 7 => grid7.contents += newButton
          case 8 => grid8.contents += newButton
        }
        //Setting the listener for this button
        listenTo(newButton)

        fillAllCol(row, col + 1)
      }
    }

    //calling function for filling the sudoku table and returning the new gridPanel
    fillGrid

    //fill the main panel with smaller ones that represent sub-boxes of the sudoku board
    panel.contents += grid0
    panel.contents += grid1
    panel.contents += grid2
    panel.contents += grid3
    panel.contents += grid4
    panel.contents += grid5
    panel.contents += grid6
    panel.contents += grid7
    panel.contents += grid8

    panel.border = Swing.EmptyBorder(30,30,30,30)
    panel.background = GameLookConstants.GAME_BACKGROUND
    panel.focusable = true
    panel.requestFocus()

    listenTo(panel.keys)
    reactions += {
      case KeyTyped(_, c, _, _) =>
        if ('1' <= c && c <= '9') {
          ChangeSudokuBoard.inputNumber(c.asDigit)
        }
      case KeyPressed(_, Key.Up, _, _) => ChangeSudokuBoard.moveCurrentPositionUp
      case KeyPressed(_, Key.Down, _, _) => ChangeSudokuBoard.moveCurrentPositionDown
      case KeyPressed(_, Key.Left, _, _) => ChangeSudokuBoard.moveCurrentPositionLeft
      case KeyPressed(_, Key.Right, _, _) => ChangeSudokuBoard.moveCurrentPositionRight
    }

    panel
  }
  /**
   * Creating the number picker for the sudoku input
   *
   * @return
   */
  def numberPicker: BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Vertical)

    val numberOne: Button = makeButtonNumbersFont("1")
    val numberTwo: Button = makeButtonNumbersFont("2")
    val numberThree: Button = makeButtonNumbersFont("3")
    val numberFour: Button = makeButtonNumbersFont("4")
    val numberFive: Button = makeButtonNumbersFont("5")
    val numberSix: Button = makeButtonNumbersFont("6")
    val numberSeven: Button = makeButtonNumbersFont("7")
    val numberEight: Button = makeButtonNumbersFont("8")
    val numberNine: Button = makeButtonNumbersFont("9")
    val erase: Button = makeButtonNumbersFont("  ")

    boxPanel.contents += erase
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberOne
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberTwo
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberThree
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberFour
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberFive
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberSix
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberSeven
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberEight
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += numberNine
    boxPanel.contents += Swing.VStrut(10)

    //the method call for EmptyBorder - Swing.EmptyBorder(top, left, bottom, right)
    boxPanel.border = Swing.EmptyBorder(30,15,30,30)
    boxPanel.background = GameLookConstants.GAME_BACKGROUND

    //button handlers
    listenTo(numberOne, numberTwo, numberThree, numberFour, numberFive, numberSix, numberSeven, numberEight, numberNine, erase)
    //keyboard handlers
    listenTo(boxPanel.keys)

    reactions += {
      case ButtonClicked(`numberOne`) => ChangeSudokuBoard.inputNumber(1)
      case ButtonClicked(`numberTwo`) => ChangeSudokuBoard.inputNumber(2)
      case ButtonClicked(`numberThree`) => ChangeSudokuBoard.inputNumber(3)
      case ButtonClicked(`numberFour`) => ChangeSudokuBoard.inputNumber(4)
      case ButtonClicked(`numberFive`) => ChangeSudokuBoard.inputNumber(5)
      case ButtonClicked(`numberSix`) => ChangeSudokuBoard.inputNumber(6)
      case ButtonClicked(`numberSeven`) => ChangeSudokuBoard.inputNumber(7)
      case ButtonClicked(`numberEight`) => ChangeSudokuBoard.inputNumber(8)
      case ButtonClicked(`numberNine`) => ChangeSudokuBoard.inputNumber(9)
      case ButtonClicked(`erase`) => ChangeSudokuBoard.eraseNumber
    }

    boxPanel
  }
  /**
   * Creating the message board and close button for return to the starting window
   *
   * @return
   */
  def messageOutputAndExit : BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Horizontal)

    val closeButton = makeButtonNumbersFont("Close")

    listenTo(closeButton)
    reactions += {
      case ButtonClicked(`closeButton`) => {
        ChangeSudokuBoard.closeWindows
      }
    }

    messageOutput.rows = 5
    messageOutput.columns = 20
    messageOutput.font = GameLookConstants.TEXT_FONT
    messageOutput.editable = false
    messageOutput.lineWrap = true

    boxPanel.contents += new ScrollPane(messageOutput)
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += closeButton

    boxPanel.border = Swing.EmptyBorder(15,30,30,30)
    boxPanel.background = GameLookConstants.GAME_BACKGROUND

    boxPanel
  }
  /**
   * Creating the functions panel on the left of the sudoku board
   *
   * @return
   */
  def functionsPanel: BoxPanel = {

    val boxPanel = new BoxPanel(Orientation.Vertical)

    val changeStartPosition = makeButtonNumbersFont("POCETNA POZICIJA")
    val filterRowAndColumn = makeButtonNumbersFont("FILTRIRAJ REDOVE I KOLONE")
    val filterSquare = makeButtonNumbersFont("FILTRIRAJ KOSCKU")
    val changeUp = makeButtonNumbersFont("ZAMENA")
    val transpose = makeButtonNumbersFont("TRANSPONUJ")

    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += changeStartPosition
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += filterRowAndColumn
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += filterSquare
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += changeUp
    boxPanel.contents += Swing.VStrut(10)
    boxPanel.contents += transpose
    boxPanel.contents += Swing.VStrut(10)


    listenTo(changeStartPosition, filterRowAndColumn, filterSquare, changeUp, transpose)
    reactions += {
      case ButtonClicked(`changeStartPosition`) => {
        ChangeSudokuBoard.changeStratingPositionWrapper
      }
      case ButtonClicked(`filterRowAndColumn`) => {
        ChangeSudokuBoard.filterRowAndColumnWrapper
      }
      case ButtonClicked(`filterSquare`) => {
        ChangeSudokuBoard.filterSubSquareWrapper
      }
      case ButtonClicked(`changeUp`) => {
        ChangeSudokuBoard.changeUp
      }
      case ButtonClicked(`transpose`) => {
        ChangeSudokuBoard.transposition
      }
    }
    boxPanel.border = Swing.EmptyBorder(150,30,30,15)
    boxPanel.background = GameLookConstants.GAME_BACKGROUND

    boxPanel
  }

  title = "Make sudoku board"

  //main panel
  val mainPanel = new BorderPanel(){
    add(sudokuTable, BorderPanel.Position.Center)
    add(numberPicker, BorderPanel.Position.East)
    add(messageOutputAndExit, BorderPanel.Position.South)
    add(functionsPanel, BorderPanel.Position.West)
  }

  listenTo(mainPanel.keys)
  mainPanel.focusable = true
  mainPanel.requestFocus()
  mainPanel.background = GameLookConstants.GAME_BACKGROUND

  contents = mainPanel
  ChangeSudokuBoard.setGameFrameTable(this)

  //last attribute touch-up
  visible = true
  resizable = true
  peer.setLocationRelativeTo(null)
  size = new Dimension(1000, 900)
}
