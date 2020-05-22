package GUI

import java.awt.Font
import scala.swing.Color

object GameLookConstants {

  val veryDarkBlue = new Color(29,53,87)
  val darkModerateBlue = new Color(69,123,157)
  val verySoftCyan = new Color(168,218,220)
  val lightGrayishLimeGreen = new Color(181,190,178)
  val darkRed = new Color(230,57,70)

  val black = new Color(0,0,0)
  val veryLightGray = new Color(225,225,225)
  val white = new Color(255, 255, 255)

  //------------- C O L O R S -------------------
  val GAME_BACKGROUND = verySoftCyan

  val MENU_BUTTON_BACKGROUND = veryDarkBlue
  val MENU_BUTTON_FOREGROUND = lightGrayishLimeGreen
  val MENU_TITLE = darkRed

  val UNSELECTED_BUTTON_BACKGROUND_COLOR: Color = white
  val SELECTED_BUTTON_BACKGROUND_COLOR = verySoftCyan
  val SELECTED_BUTTON_AREA_BACKGROUND = veryLightGray

  val ORIGINAL_BOARD_NUMBER: Color = black
  val USER_INPUT_BOARD_NUMBER: Color = darkModerateBlue

  //----------------- F O N T S ---------------------
  val MENU_TITLE_FONT = new Font("Arial", Font.BOLD, 20)
  val MENU_BUTTON_FONT = new Font("Arial", Font.BOLD, 16)

  val DEFAULT_FONT = new Font("Arial", Font.BOLD, 22)
  val TEXT_FONT = new Font("Arial", Font.PLAIN, 18)
  val NUMBERS_FONT = new Font("Arial", Font.PLAIN, 16)

  //---------------- E R R O R  C O D E S-----------
  val CODE_OK = 0
  val CODE_WARNING = 1
  val CODE_ERROR = 2


}
