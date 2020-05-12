package GUI

import scala.swing._
import scala.swing.event._

import GameBoard.SudokuBoard

class GameFrame(private val mainOwner: Frame) extends Frame {
  val UNSELECTED_BUTTON_BACKGROUND_COLOR: Color = new Color(255, 255, 255)
  val SELECTED_BUTTON_BACKGROUND_COLOR = new Color(190, 190, 190)
  val SELECTED_BUTTON_AREA_BACKGROUND = new Color(225, 225, 225)

  //creatig the sudoku table and filling it with data
  def sudokuTable: GridPanel = {
    //The variable 'panel' is accessible from every local function
    val panel = new GridPanel(9,9)

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

        //storing the new button in our button matrix and putting it on the grid panel
        allSudokuFields(row).update(col, newButton)
        panel.contents += newButton

        //Setting the listener for this button
        listenTo(newButton)

        fillAllCol(row, col + 1)
      }
    }

    //calling function for filling the sudoku table and returning the new gridPanel
    fillGrid

    panel.border = Swing.EmptyBorder(30,30,30,30)
    panel
  }

  title = "Game"

  val allSudokuFields  = Array.ofDim[Button](9,9)
  val myButton = new Button("Close")

  //main panel
  val mainPanel = new BorderPanel(){
    add(sudokuTable, BorderPanel.Position.Center)
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

  def positionChange (row: Int, col: Int) = {
    val lastPosition: (Int, Int) = SudokuBoard.getCurrentPosition

    def setBoardColors = {
      def changeColor(r: Int, c: Int, colour: Color) = {
        for (iter <- 0 to 8) {
          //returning the previous selection to normal color
          allSudokuFields(r)(iter).background = colour
          allSudokuFields(iter)(c).background = colour
        }
      }

      changeColor(lastPosition._1, lastPosition._2, UNSELECTED_BUTTON_BACKGROUND_COLOR)
      changeColor(row, col, SELECTED_BUTTON_AREA_BACKGROUND)

      allSudokuFields(row)(col).background = SELECTED_BUTTON_BACKGROUND_COLOR
    }

    setBoardColors
    SudokuBoard.setCurrentPosition((row, col))
  }

}
