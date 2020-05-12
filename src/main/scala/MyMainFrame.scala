
import scala.swing._
import scala.swing.event.ButtonClicked

class MyMainFrame extends MainFrame {
  def makeButtons(name: String): Button = {
    val myButton = new Button(name)

    myButton.xLayoutAlignment = 0.5f
    myButton.margin = new Insets(15, 15, 15, 15)
    myButton
  }

  title = "SUDOKU"

  //Creating label that is header of application
  val header: Label = new Label("S U D O K U")
  header.xLayoutAlignment = 0.5f

  //Creating the buttons of the start menu
  val startGame = makeButtons("New Game")
  val makeNewSudokuBoard = makeButtons("Make new sudoku board")
  val exitGame = makeButtons("Exit")

  //Creating a boxPanel and inserting all of the previously made components with spaces in between
  val boxPanel = new BoxPanel(Orientation.Vertical)

  //the order is important
  boxPanel.contents += header
  boxPanel.contents += Swing.VStrut(50)
  boxPanel.contents += Swing.Glue
  boxPanel.contents += startGame
  boxPanel.contents += Swing.VStrut(20)
  boxPanel.contents += makeNewSudokuBoard
  boxPanel.contents += Swing.VStrut(20)
  boxPanel.contents += exitGame

  //boxPanel.background = new Color(16, 59, 27)
  boxPanel.border = Swing.EmptyBorder(150, 100, 150, 100)
  contents = boxPanel

  resizable = true
  //centering the window to middle of screen
  peer.setLocationRelativeTo(null)


  //Listeners
  listenTo(startGame, makeNewSudokuBoard, exitGame)
  reactions += {
    case ButtonClicked(`exitGame`) => sys.exit(0)
  }
}

object MySudoku extends SimpleSwingApplication{
  override def top: Frame = new MyMainFrame()
}
