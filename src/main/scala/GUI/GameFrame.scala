package GUI

import scala.swing._
import scala.swing.event._
import java.awt.Font

import GameBoard.SudokuBoard

class GameFrame(private val mainOwner: Frame) extends Frame {
  val UNSELECTED_BUTTON_BACKGROUND_COLOR: Color = new Color(255, 255, 255)
  val SELECTED_BUTTON_BACKGROUND_COLOR = new Color(190, 190, 190)
  val SELECTED_BUTTON_AREA_BACKGROUND = new Color(225, 225, 225)

  val DEFAULT_FONT = new Font("Arial", Font.BOLD, 22)

  val allSudokuFields  = Array.ofDim[Button](9,9)
  val myButton = new Button("Close")

  /**
   * creatig the sudoku table and filling it with data
   * @return
   */
  def sudokuTable: GridPanel = {
    def makePanelWithBorder(r: Int, c:Int) :GridPanel = {
      val newPanel = new GridPanel(r,c)
      newPanel.border = Swing.LineBorder(new Color(0,0,0), 2)

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
        val buttonMapName: String = row + "," + col
        val newButton: Button = new Button()
        val newButtonAction: Action =
          if (SudokuBoard.board(row)(col) != 0)
            new Action(SudokuBoard.board(row)(col).toString) {
              override def apply(): Unit = positionChange(row,col)
          }
          else {
            new Action(" ") {
              override def apply(): Unit = positionChange(row,col)
            }
          }

        newButton.action = newButtonAction
        newButton.background = UNSELECTED_BUTTON_BACKGROUND_COLOR
        newButton.font = DEFAULT_FONT

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
    panel
  }

  def numberPicker: BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Vertical)

    def makeNumberPickers(name: String): Button = {
      val myButton = new Button(name)

      myButton.xLayoutAlignment = 0.5f
      myButton.margin = new Insets(15, 15, 15, 15)
      myButton.font = DEFAULT_FONT
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

    boxPanel.border = Swing.EmptyBorder(30,30,30,30)

    //handlers
    listenTo(numberOne, numberTwo, numberThree, numberFour, numberFive, numberSix, numberSeven, numberEight, numberNine)

    /*
    reactions += {
      case ButtonClicked(`numberOne`) =>
    }
    */

    boxPanel
  }

  title = "Game"

  //main panel
  val mainPanel = new BorderPanel(){
    add(sudokuTable, BorderPanel.Position.Center)
    add(numberPicker, BorderPanel.Position.East)
    add(myButton, BorderPanel.Position.South)
  }

  contents = mainPanel

  //action handlers
  listenTo(myButton)
  reactions += {
    case ButtonClicked(`myButton`) => {
      mainOwner.visible = true
      dispose()
    }
  }

  //last attribute touch-up
  visible = true
  resizable = true
  peer.setLocationRelativeTo(null)
  positionChange(0,0)

  def positionChange (row: Int, col: Int) = {
    val lastPosition: (Int, Int) = SudokuBoard.getCurrentPosition

    def setBoardColors = {
      def changeColor(r: Int, c: Int, color: Color) = {
        for (iter <- 0 to 8) {
          //returning the previous selection to normal color
          allSudokuFields(r)(iter).background = color
          allSudokuFields(iter)(c).background = color
        }
      }

      def changeColorOfBox(numOfBox: Int, color: Color):Unit = {
        val row = numOfBox / 3 * 3
        val col = (numOfBox % 3) * 3

        for (r <- row until  row + 3;
             c <- col until  col + 3) {
          //returning the previous selection to normal color
          allSudokuFields(r)(c).background = color
        }
      }

      changeColor(lastPosition._1, lastPosition._2, UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColorOfBox((lastPosition._1 / 3) * 3 + lastPosition._2 / 3, UNSELECTED_BUTTON_BACKGROUND_COLOR)

      changeColor(row, col, SELECTED_BUTTON_AREA_BACKGROUND)
      changeColorOfBox((row/3) * 3 + col / 3, SELECTED_BUTTON_AREA_BACKGROUND)

      allSudokuFields(row)(col).background = SELECTED_BUTTON_BACKGROUND_COLOR
    }

    setBoardColors
    SudokuBoard.setCurrentPosition((row, col))
  }

}
