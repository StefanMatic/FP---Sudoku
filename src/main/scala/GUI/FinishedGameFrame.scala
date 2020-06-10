package GUI

import GameBoard.SudokuBoard

import scala.swing._
import scala.swing.event._

/**
 * Class that represents the finished game frame. It's made using the previous color pallet
 *
 * @param myStartFrame
 * @param myGameFrame
 */
class FinishedGameFrame(val myStartFrame: Frame, val myGameFrame: GameFrame, sudokuBoard: SudokuBoard) extends Frame {
  /**
   * Makes menu buttons with all the adjustments
   *
   * @param name
   * @return
   */
  def makeButtons(name: String): Button = {
    val myButton = new Button(name)

    myButton.xLayoutAlignment = 0.5f
    myButton.margin = new Insets(15, 15, 15, 15)
    myButton.background = GameLookConstants.MENU_BUTTON_BACKGROUND
    myButton.foreground = GameLookConstants.MENU_BUTTON_FOREGROUND
    myButton.font = GameLookConstants.MENU_BUTTON_FONT
    myButton
  }

  title = "Congratulation!"

  //Creating menu title
  val header: Label = new Label("F I N I S H E D !  :)")
  header.xLayoutAlignment = 0.5f
  header.foreground = GameLookConstants.MENU_TITLE
  header.font = GameLookConstants.MENU_TITLE_FONT

  val goBack = makeButtons("GO BACK")

  //Creating a boxPanel and inserting all the previously made components with spaces in between
  val boxPanel = new BoxPanel(Orientation.Vertical)

  //the order is important
  boxPanel.contents += header
  boxPanel.contents += Swing.VStrut(50)
  boxPanel.contents += Swing.Glue
  boxPanel.contents += goBack
  boxPanel.contents += Swing.VStrut(20)

  //boxPanel adjustments
  boxPanel.border = Swing.EmptyBorder(150, 100, 150, 100)
  boxPanel.xLayoutAlignment = 0.5f
  boxPanel.background = GameLookConstants.GAME_BACKGROUND

  contents = boxPanel

  //main frame adjustments
  size = new Dimension(500, 700)
  resizable = true
  visible = true
  //centering the window to middle of screen
  peer.setLocationRelativeTo(null)

  //Listeners
  listenTo(goBack)

  reactions += {
    case ButtonClicked(`goBack`) => {
      sudokuBoard.closeWindows
    }
  }

}
