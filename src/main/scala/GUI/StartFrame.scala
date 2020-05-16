package GUI


import scala.swing._
import scala.swing.event.ButtonClicked
import GUI.GameFrame
import GameBoard.SudokuBoard

class StartFrame extends MainFrame {
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

  boxPanel.border = Swing.EmptyBorder(150, 100, 150, 100)
  boxPanel.xLayoutAlignment = 0.5f
  contents = boxPanel

  size = new Dimension(400, 600)
  resizable = true
  //centering the window to middle of screen
  peer.setLocationRelativeTo(null)


  //Listeners
  listenTo(startGame, makeNewSudokuBoard, exitGame)

  reactions += {
    case ButtonClicked(`startGame`) => {
      visible = false
      //TODO: Ovo bi trebalo da se radi u zasebnom frejmu ali neka za sada ostane dok se ne sredi sve
      //SudokuBoard.readFromFile("src/SudokuBoardExamples/Easy.txt")
      //new GameFrame(this)
      new SudokuPicker(this)
    }
    case ButtonClicked(`exitGame`) => sys.exit(0)
  }
}

