package GUI

import GameBoard.{ChangeSudokuBoard, SudokuBoard}

import scala.swing._
import scala.swing.event._

class SudokuPicker(private val mainOwner: Frame, val gameMode: Boolean) extends Frame {
  val boxPanel = new BoxPanel(Orientation.Vertical)
  //making a scroll pane to make the frame scrollable
  val scrollPane = new ScrollPane(boxPanel)

  //making the subject of the frame for better user experience
  val subjectSudokuPicker = new Label("SELECT TABLE")
  subjectSudokuPicker.font = GameLookConstants.MENU_TITLE_FONT
  subjectSudokuPicker.foreground = GameLookConstants.MENU_TITLE
  subjectSudokuPicker.xLayoutAlignment = 0.5f

  //adding the subject to main boxPanel with a weider gap in between
  boxPanel.contents += subjectSudokuPicker
  boxPanel.contents += Swing.VStrut(50)
  boxPanel.contents += Swing.Glue

  private def makeSudokuPickButtons = {
    //getting all the .txt files
    val allSudokuBoards = new java.io.File("src/SudokuBoardExamples").listFiles.filter(_.getName.endsWith(".txt"))
    for (f <- allSudokuBoards){
      val newButton = new Button(f.getName)

      newButton.xLayoutAlignment = 0.5f
      newButton.margin = new Insets(15, 15, 15, 15)
      newButton.font = GameLookConstants.MENU_BUTTON_FONT
      newButton.background = GameLookConstants.MENU_BUTTON_BACKGROUND
      newButton.foreground = GameLookConstants.MENU_BUTTON_FOREGROUND
      newButton.action = new Action(f.getName.substring(0, f.getName.indexOf("."))) {
        override def apply(): Unit = {
          visible = false
          if (gameMode) {
            SudokuBoard.fillSudoku("src/SudokuBoardExamples/" + f.getName)
            SudokuBoard.setGameFrame(mainOwner)
          }
          else {
            ChangeSudokuBoard.fillSudoku("src/SudokuBoardExamples/" + f.getName)
            ChangeSudokuBoard.setGameFrame(mainOwner)
          }
        }
      }

      boxPanel.contents += Swing.VStrut(20)
      boxPanel.contents += newButton
    }
  }

  makeSudokuPickButtons
  boxPanel.background = GameLookConstants.GAME_BACKGROUND
  boxPanel.border = Swing.EmptyBorder(150, 100, 150, 100)
  boxPanel.xLayoutAlignment = 0.5f

  title = "Pick difficulty"

  contents = scrollPane

  size = new Dimension(500, 700)
  resizable = true
  peer.setLocationRelativeTo(null)
  visible = true
}
