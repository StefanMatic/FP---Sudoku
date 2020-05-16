package GUI

import java.awt.Font
import scala.swing.Color

object GameLookConstants {
  val UNSELECTED_BUTTON_BACKGROUND_COLOR: Color = new Color(255, 255, 255)
  val SELECTED_BUTTON_BACKGROUND_COLOR = new Color(190, 190, 190)
  val SELECTED_BUTTON_AREA_BACKGROUND = new Color(225, 225, 225)

  val ORIGINAL_BOARD_NUMBER: Color = new Color(0, 0, 0)
  val USER_INPUT_BOARD_NUMBER: Color = new Color(93, 161, 157)

  val DEFAULT_FONT = new Font("Arial", Font.BOLD, 22)
  val TEXT_FONT = new Font("Arial", Font.PLAIN, 18)
  val NUMBERS_FONT = new Font("Arial", Font.PLAIN, 16)

  val CODE_OK = 0
  val CODE_WARNING = 1
  val CODE_ERROR = 2


}
