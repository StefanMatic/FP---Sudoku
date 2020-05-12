import java.awt.BorderLayout

import javax.swing._
import GameBoard.SudokuBoard

import scala.swing._

class MyMainFrame extends MainFrame {
  title = "SUDOKU"
  val header: Label = new Label("S U D O K U")
  val startGame = new Button("New Game")
  val makeNewSudokuBoard = new Button("Make new sudoku board")

  header.preferredSize = new Dimension(200, 50)

  startGame.margin = new Insets(15, 15, 15, 15)
  startGame.preferredSize = new Dimension(200, 50)


  val boxPanel = new BoxPanel(Orientation.Vertical)
  boxPanel.contents += header
  boxPanel.contents += Swing.VStrut(50)
  boxPanel.contents += Swing.Glue
  boxPanel.contents += startGame
  boxPanel.contents += Swing.VStrut(20)
  boxPanel.contents += makeNewSudokuBoard

  boxPanel.background = new Color(16, 59, 27)
  boxPanel.border = Swing.EmptyBorder(100, 50, 100, 50)

  contents = boxPanel

  resizable = true

}

object MySudoku extends SimpleSwingApplication{
  override def top: Frame = new MyMainFrame()
}
