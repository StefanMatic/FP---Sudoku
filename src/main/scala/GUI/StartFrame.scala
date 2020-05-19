package GUI

import scala.swing._
import scala.swing.event.ButtonClicked

class StartFrame extends MainFrame {
  def makeButtons(name: String): Button = {
    val myButton = new Button(name)

    myButton.xLayoutAlignment = 0.5f
    myButton.margin = new Insets(15, 15, 15, 15)
    myButton.background = GameLookConstants.MENU_BUTTON_BACKGROUND
    myButton.foreground = GameLookConstants.MENU_BUTTON_FOREGROUND
    myButton.font = GameLookConstants.MENU_BUTTON_FONT
    myButton
  }

  title = "SUDOKU"

  //Creating menu title
  val header: Label = new Label("S U D O K U")
  header.xLayoutAlignment = 0.5f
  header.foreground = GameLookConstants.MENU_TITLE
  header.font = GameLookConstants.MENU_TITLE_FONT

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
  boxPanel.background = GameLookConstants.GAME_BACKGROUND

  contents = boxPanel

  size = new Dimension(500, 700)
  resizable = true
  //centering the window to middle of screen
  peer.setLocationRelativeTo(null)

  //Listeners
  listenTo(startGame, makeNewSudokuBoard, exitGame)

  reactions += {
    case ButtonClicked(`startGame`) => {
      visible = false
      new SudokuPicker(this)
    }
    case ButtonClicked(`exitGame`) => sys.exit(0)
  }
}

