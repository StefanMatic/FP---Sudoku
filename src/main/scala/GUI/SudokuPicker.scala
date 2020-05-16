package GUI

import GameBoard.SudokuBoard

import scala.swing._
import scala.swing.event._

class SudokuPicker(private val mainOwner: Frame) extends Frame {
  val boxPanel = new BoxPanel(Orientation.Vertical)

  private def makeSudokuPickButtons = {
    //getting all the .txt files
    val allSudokuBoards = new java.io.File("src/SudokuBoardExamples").listFiles.filter(_.getName.endsWith(".txt"))
    for (f <- allSudokuBoards){
      val newButton = new Button(f.getName)

      newButton.xLayoutAlignment = 0.5f
      newButton.margin = new Insets(15, 15, 15, 15)
      newButton.font = GameLookConstants.DEFAULT_FONT
      newButton.action = new Action(f.getName.substring(0, f.getName.indexOf("."))) {
        override def apply(): Unit = {
          visible = false
          SudokuBoard.readFromFile("src/SudokuBoardExamples/" + f.getName)
          new GameFrame(mainOwner)
        }
      }

      boxPanel.contents += Swing.VStrut(10)
      boxPanel.contents += newButton
    }
  }

  makeSudokuPickButtons
  boxPanel.border = Swing.EmptyBorder(150, 100, 150, 100)
  boxPanel.xLayoutAlignment = 0.5f

  title = "Pick difficulty:"

  contents = boxPanel

  resizable = true
  peer.setLocationRelativeTo(null)
  visible = true
}
