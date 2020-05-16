package GUI

import scala.swing._
import scala.swing.event._
import java.awt.event._

import GameBoard.SudokuBoard
import GUI.GameLookConstants

class GameFrame(private val mainOwner: Frame) extends Frame {
  val allSudokuFields  = Array.ofDim[Button](9,9)

  //Testiranje - izbrisati ovo

  val messageOutput = new TextArea()
  val closeButton = new Button("Close")
  val readInstructions = new Button("Load")

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
          if (SudokuBoard.board(row)(col) != 0)
            new Action(SudokuBoard.board(row)(col).toString) {
              override def apply(): Unit = SudokuBoard.positionChange(row,col)
          }
          else {
            new Action(" ") {
              override def apply(): Unit = SudokuBoard.positionChange(row,col)
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
    panel.focusable = true
    panel.requestFocus()

    listenTo(panel.keys)
    reactions += {
      case KeyTyped(_, c, _, _) =>
        if ('1' <= c && c <= '9') {
          SudokuBoard.inputNumber(c.asDigit)
        }
      case KeyPressed(_, Key.Up, _, _) => SudokuBoard.moveCurrentPositionUp
      case KeyPressed(_, Key.Down, _, _) => SudokuBoard.moveCurrentPositionDown
      case KeyPressed(_, Key.Left, _, _) => SudokuBoard.moveCurrentPositionLeft
      case KeyPressed(_, Key.Right, _, _) => SudokuBoard.moveCurrentPositionRight
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

    def makeNumberPickers(name: String): Button = {
      val myButton = new Button(name)

      myButton.xLayoutAlignment = 0.5f
      myButton.margin = new Insets(15, 15, 15, 15)
      myButton.font = GameLookConstants.NUMBERS_FONT
      myButton
    }

    val numberOne: Button = makeNumberPickers("1")
    val numberTwo: Button = makeNumberPickers("2")
    val numberThree: Button = makeNumberPickers("3")
    val numberFour: Button = makeNumberPickers("4")
    val numberFive: Button = makeNumberPickers("5")
    val numberSix: Button = makeNumberPickers("6")
    val numberSeven: Button = makeNumberPickers("7")
    val numberEight: Button = makeNumberPickers("8")
    val numberNine: Button = makeNumberPickers("9")
    val erase: Button = makeNumberPickers("  ")

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

    //button handlers
    listenTo(numberOne, numberTwo, numberThree, numberFour, numberFive, numberSix, numberSeven, numberEight, numberNine, erase)
    //keyboard handlers
    listenTo(boxPanel.keys)

    reactions += {
      case ButtonClicked(`numberOne`) => SudokuBoard.inputNumber(1)
      case ButtonClicked(`numberTwo`) => SudokuBoard.inputNumber(2)
      case ButtonClicked(`numberThree`) => SudokuBoard.inputNumber(3)
      case ButtonClicked(`numberFour`) => SudokuBoard.inputNumber(4)
      case ButtonClicked(`numberFive`) => SudokuBoard.inputNumber(5)
      case ButtonClicked(`numberSix`) => SudokuBoard.inputNumber(6)
      case ButtonClicked(`numberSeven`) => SudokuBoard.inputNumber(7)
      case ButtonClicked(`numberEight`) => SudokuBoard.inputNumber(8)
      case ButtonClicked(`numberNine`) => SudokuBoard.inputNumber(9)
      case ButtonClicked(`erase`) => SudokuBoard.eraseNumber
    }

    boxPanel
  }
  /**
   * Creating the message board and close button for return to the starting window
   *
   * @return
   */
  def messageOutputAndExit : BoxPanel = {
    def buttonFunctions: BoxPanel = {
      val boxP = new BoxPanel(Orientation.Vertical)

      //setting up the visual of the load button
      readInstructions.margin = new Insets(15, 15, 15, 15)
      readInstructions.font = GameLookConstants.DEFAULT_FONT
      readInstructions.yLayoutAlignment = 0.5f

      //setting up the visual of the exit button
      closeButton.margin = new Insets(15, 15, 15, 15)
      closeButton.font = GameLookConstants.DEFAULT_FONT
      closeButton.yLayoutAlignment = 0.5f

      listenTo(closeButton, readInstructions)
      reactions += {
        case ButtonClicked(`closeButton`) => {
          mainOwner.visible = true
          dispose()
        }
        case ButtonClicked(`readInstructions`) => {
          SudokuBoard.readInstructionsFromFile("src/SudokuInstructons/ins.txt")
        }
      }

      boxP.yLayoutAlignment = 0.5f
      boxP.xLayoutAlignment = 0.5f

      boxP.contents += readInstructions
      boxP.contents += Swing.VStrut(10)
      boxP.contents += closeButton

      boxP
    }

    val boxPanel = new BoxPanel(Orientation.Horizontal)

    messageOutput.rows = 5
    messageOutput.columns = 20
    messageOutput.font = GameLookConstants.TEXT_FONT
    messageOutput.editable = false
    messageOutput.lineWrap = true

    boxPanel.contents += new ScrollPane(messageOutput)
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += buttonFunctions

    boxPanel.border = Swing.EmptyBorder(15,30,30,30)

    boxPanel
  }

  title = "Game"

  //main panel
  val mainPanel = new BorderPanel(){
    add(sudokuTable, BorderPanel.Position.Center)
    add(numberPicker, BorderPanel.Position.East)
    add(messageOutputAndExit, BorderPanel.Position.South)
  }

  listenTo(mainPanel.keys)
  mainPanel.focusable = true
  mainPanel.requestFocus()

  contents = mainPanel
  SudokuBoard.setGameFrameTable(this)

  //last attribute touch-up
  visible = true
  resizable = true
  peer.setLocationRelativeTo(null)
  size = new Dimension(800, 900)
}
