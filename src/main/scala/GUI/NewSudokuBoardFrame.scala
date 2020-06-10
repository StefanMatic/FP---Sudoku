package GUI

import GameBoard.ChangeSudokuBoard

import scala.swing.event._
import scala.swing._

class NewSudokuBoardFrame(val mainOwner: Frame, changeSudokuBoard: ChangeSudokuBoard) extends Frame {
  val allSudokuFields  = Array.ofDim[Button](9,9)
  val messageOutput = new TextArea()
  val saveSudoku = makeButtonNumbersFont("SAVE")
  val sudokuName = new TextField()

  val numPicker: BoxPanel = numberPicker
  val funcPanel: BoxPanel = functionsPanel

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
   * Adding newly created functions
   *
   * @param name
   * @param func
   */
  def addFunction(name: String, func: List[((Int, Int))=>(Int, Int)]): Unit = {
    val newButton = makeButtonNumbersFont(name.toUpperCase)
    funcPanel.contents += Swing.VStrut(10)
    funcPanel.contents += newButton

    listenTo(newButton)
    reactions += {
      case ButtonClicked(`newButton`) => {
        changeSudokuBoard.executeFunctionList(func)
      }
    }

    funcPanel.repaint()
    westNoxPanel.repaint()

    changeSudokuBoard.addedFunctionMessage
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
          if (changeSudokuBoard.sudokuTable(row)(col)._1 != 0)
            new Action(changeSudokuBoard.sudokuTable(row)(col)._1.toString) {
              override def apply(): Unit =
                changeSudokuBoard.positions = changeSudokuBoard.positionChange(row, col, NewSudokuBoardFrame.this)
            }
          else {
            new Action(" ") {
              override def apply(): Unit =
                changeSudokuBoard.positions = changeSudokuBoard.positionChange(row, col, NewSudokuBoardFrame.this)
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
          changeSudokuBoard.inputNumber(c.asDigit)
        }
      case KeyPressed(_, Key.Up, _, _) => changeSudokuBoard.moveSingleStepUp
      case KeyPressed(_, Key.Down, _, _) => changeSudokuBoard.moveSingleStepDown
      case KeyPressed(_, Key.Left, _, _) => changeSudokuBoard.moveSingleStepLeft
      case KeyPressed(_, Key.Right, _, _) => changeSudokuBoard.moveSingleStepRight
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
    // val erase: Button = makeButtonNumbersFont("  ")

    // boxPanel.contents += erase
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
    listenTo(numberOne, numberTwo, numberThree, numberFour, numberFive, numberSix, numberSeven, numberEight, numberNine)
    //keyboard handlers
    listenTo(boxPanel.keys)

    reactions += {
      case ButtonClicked(`numberOne`) => changeSudokuBoard.inputNumber(1)
      case ButtonClicked(`numberTwo`) => changeSudokuBoard.inputNumber(2)
      case ButtonClicked(`numberThree`) => changeSudokuBoard.inputNumber(3)
      case ButtonClicked(`numberFour`) => changeSudokuBoard.inputNumber(4)
      case ButtonClicked(`numberFive`) => changeSudokuBoard.inputNumber(5)
      case ButtonClicked(`numberSix`) => changeSudokuBoard.inputNumber(6)
      case ButtonClicked(`numberSeven`) => changeSudokuBoard.inputNumber(7)
      case ButtonClicked(`numberEight`) => changeSudokuBoard.inputNumber(8)
      case ButtonClicked(`numberNine`) => changeSudokuBoard.inputNumber(9)
      // case ButtonClicked(`erase`) => ChangeSudokuBoard.eraseNumber
    }

    boxPanel.visible = false
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
        changeSudokuBoard.closeWindows
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

    for (func <- changeSudokuBoard.functionList) {
      val newButton = makeButtonNumbersFont(func._1.toUpperCase)

      boxPanel.contents += Swing.VStrut(10)
      boxPanel.contents += newButton

      listenTo(newButton)
      reactions += {
        case ButtonClicked(`newButton`) => {
          changeSudokuBoard.executeFunctionList(func._2)
        }
      }
    }

    boxPanel.border = Swing.EmptyBorder(30,30,30,15)
    boxPanel.background = GameLookConstants.GAME_BACKGROUND

    boxPanel
  }
  /**
   * Creating the input for sudoku name and button for saving
   *
   * @return
   */
  def sudokuNameAndSave: BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Horizontal)

    val hint = new Label("Name of table: ")
    hint.foreground = GameLookConstants.MENU_BUTTON_BACKGROUND
    hint.font = GameLookConstants.MENU_TITLE_FONT

    sudokuName.editable = true
    sudokuName.yLayoutAlignment = 0.5
    sudokuName.columns = 20
    sudokuName.font = GameLookConstants.MENU_TITLE_FONT
    sudokuName.foreground = GameLookConstants.MENU_TITLE
    sudokuName.listenTo(sudokuName.keys)
    sudokuName.reactions += {
      case e: KeyTyped => {
        changeSudokuBoard.checkSaveButton(NewSudokuBoardFrame.this)
      }
    }

    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += hint
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += sudokuName
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += saveSudoku
    boxPanel.contents += Swing.HStrut(10)

    listenTo(saveSudoku)
    reactions += {
      case ButtonClicked(`saveSudoku`) => {
        changeSudokuBoard.saveNewBoard
        Dialog.showMessage(contents.head, "The new sudoku table is saved", title="SAVED!")
        changeSudokuBoard.closeWindows
      }
    }

    boxPanel.border = Swing.EmptyBorder(30,30,15,30)
    boxPanel.background = GameLookConstants.GAME_BACKGROUND

    boxPanel
  }

  def functionMaker: BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Horizontal)

    val funcChoose = new Label("Make new function:")
    funcChoose.font = GameLookConstants.MENU_TITLE_FONT
    val compositeFunction = makeButtonNumbersFont("Composite")
    val sequenceFunction = makeButtonNumbersFont("Sequence")

    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += funcChoose
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += compositeFunction
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += sequenceFunction
    boxPanel.contents += Swing.HStrut(10)

    listenTo(compositeFunction, sequenceFunction)
    reactions += {
      case ButtonClicked(`compositeFunction`) =>
        new MakeNewFunctionFrame(false, changeSudokuBoard)
      case ButtonClicked(`sequenceFunction`) =>
        new MakeNewFunctionFrame(true, changeSudokuBoard)
    }

    boxPanel.background = GameLookConstants.GAME_BACKGROUND
    boxPanel
  }

  title = "Make new sudoku table"

  val westNoxPanel = new ScrollPane(funcPanel)

  val northBoxPanel = new BoxPanel(Orientation.Vertical)
  northBoxPanel.contents += sudokuNameAndSave
  northBoxPanel.contents += Swing.VGlue
  northBoxPanel.contents += functionMaker
  northBoxPanel.contents += Swing.VStrut(10)

  northBoxPanel.background = GameLookConstants.GAME_BACKGROUND
  //main panel
  val mainPanel = new BorderPanel(){
    add(northBoxPanel, BorderPanel.Position.North)
    add(sudokuTable, BorderPanel.Position.Center)
    add(numPicker, BorderPanel.Position.East)
    add(messageOutputAndExit, BorderPanel.Position.South)
    add(westNoxPanel, BorderPanel.Position.West)
  }

  listenTo(mainPanel.keys)
  mainPanel.focusable = true
  mainPanel.requestFocus()
  mainPanel.background = GameLookConstants.GAME_BACKGROUND

  contents = mainPanel

  //last attribute touch-up
  visible = true
  resizable = true
  peer.setLocationRelativeTo(null)
  size = new Dimension(1000, 1000)
}
