package GUI

import scala.swing._
import scala.swing.event._

import GameBoard.SudokuBoard

class GameFrame(private val mainOwner: Frame) extends Frame {
  title = "Game"

  /*
  //creatig the sudoku table and filling it with data
  def sudokuTable: GridPanel = {
    def fillAllRows(p: GridPanel) = {
      for (col <= Su)
    }

    val panel = new GridPanel(9,9)

    panel
  }
*/
  val myButton = new Button("Close")

  //main panel
  val mainPanel = new BorderPanel(){
    //add(sudokuTable, BorderPanel.Position.Center)
    add(myButton, BorderPanel.Position.Center)
    add(new TextArea(SudokuBoard.showTable), BorderPanel.Position.South)
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

}
