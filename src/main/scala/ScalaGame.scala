import GameBoard.SudokuBoard

import java.awt._
import javax.swing._
import scala.swing._
import scala.swing.event._


object ScalaGame extends App {
  private def prepareGUI: Unit = {
    val header: JLabel = new JLabel("S U D O K U")
    val myButton = new JButton("Probaa")

    val boxPanel = new JPanel()
    boxPanel.add(header)
    boxPanel.add(myButton)


    val frame = new JFrame("Funkcionalno programiranje - S U D O K U")
    frame.getContentPane.add(boxPanel, BorderLayout.CENTER)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(new Dimension(600, 400))
    frame.setLocationRelativeTo(null)
    frame.setVisible(true)
  }

  prepareGUI
}
