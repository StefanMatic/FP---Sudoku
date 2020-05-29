package GUI

import GameBoard.ChangeSudokuBoard

import scala.swing.event._
import scala.swing._

class MakeNewFunctionFrame(sequence: Boolean) extends Frame {
  type FunctionWrapper = ((Int, Int)) => (Int, Int)

  var listOfFunctions: List[FunctionWrapper] = Nil

  val messageBoard: TextArea = new TextArea()
  val saveFunction: Button = makeButtons("SAVE")
  val closeFrame: Button = makeButtons("CLOSE")
  val functionName: TextField = new TextField()

  //------------------- Functionality ---------------------
  /**
   * Adding newly made function to the global list in ChangeSudokuBoard
   *
   * @param name
   * @param func
   */
  def addToList(name: String, func: List[FunctionWrapper]): Unit = {
    messageBoard.append(name + '\n')
    listOfFunctions = listOfFunctions ::: func
  }

  /**
   * Making a composite or sequence function and returning it
   *
   * @return
   */
  def makeAndSendFunction: List[FunctionWrapper] = {
    if (sequence){
      listOfFunctions
    } else {
      def makeCompose(f: FunctionWrapper, g: FunctionWrapper): FunctionWrapper = {
        f andThen g
      }
      //Making a dummy function as an accumulator
      def dummyFunc: FunctionWrapper = {
        def dummy(pos: (Int, Int)): (Int, Int) = {
          pos
        }
        dummy
      }

      val compositeFunction: FunctionWrapper = listOfFunctions.foldLeft(dummyFunc)(makeCompose)
      List(compositeFunction)
    }
  }

  /**
   * Making of buttons with uniform color, font and positioning
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

  /**
   * Checking if user gave new function name. Without the function name the save button is disabled
   */
  def checkFunctionName: Unit = {
    if (functionName.text.trim.equals(""))
      saveFunction.enabled = false
    else
      saveFunction.enabled = true
  }

  //----------------------- GUI ---------------------------

  /**
   * Making GUI for message report and options for saving the new function or returning
   *
   * @return
   */
  def makeMessageBoardAndSave: BoxPanel = {
    val boxPanel: BoxPanel = new BoxPanel(Orientation.Horizontal)
    val sideBoxPanel: BoxPanel = new BoxPanel(Orientation.Vertical)

    val messageLabel = new Label("Chosen functions:")
    messageLabel.xLayoutAlignment = 0.5f
    messageLabel.font = GameLookConstants.MENU_TITLE_FONT

    messageBoard.columns = 15
    messageBoard.rows = 3
    messageBoard.editable = false
    messageBoard.lineWrap = true
    messageBoard.font = GameLookConstants.TEXT_FONT

    sideBoxPanel.background = GameLookConstants.GAME_BACKGROUND

    //Making the save button initially disabled
    saveFunction.enabled = false

    sideBoxPanel.contents += Swing.VStrut(10)
    sideBoxPanel.contents += saveFunction
    sideBoxPanel.contents += Swing.VStrut(10)
    sideBoxPanel.contents += closeFrame
    sideBoxPanel.contents += Swing.VStrut(10)

    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += messageLabel
    boxPanel.contents += Swing.HStrut(20)
    boxPanel.contents += new ScrollPane(messageBoard)
    boxPanel.contents += Swing.HStrut(30)
    boxPanel.contents += sideBoxPanel
    boxPanel.contents += Swing.HStrut(10)

    listenTo(saveFunction, closeFrame)
    reactions += {
      case ButtonClicked(`saveFunction`) => {
        if (listOfFunctions.length != 0)
          ChangeSudokuBoard.addFunctionToList(functionName.text, makeAndSendFunction)
        dispose()
      }
      case ButtonClicked(`closeFrame`) => {
        dispose()
      }
    }

    boxPanel.background = GameLookConstants.GAME_BACKGROUND
    boxPanel
  }

  /**
   * Making GUI for function selection
   */
  def makeFunctions: BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Vertical)

    //Making all the functions made in this session
    for (func <- ChangeSudokuBoard.functionList) {
      //composite
      if (!sequence) {
        //Skipping over all sequence function when in COMPOSITE MODE
        if (func._2.length == 1){
          val newButton = makeButtons(func._1.toUpperCase)

          boxPanel.contents += Swing.VStrut(10)
          boxPanel.contents += newButton

          listenTo(newButton)
          reactions += {
            case ButtonClicked(`newButton`) => {
              addToList(func._1, func._2)
            }
          }
        }
      } else {
        // When in sequence mode it all functions come into consideration
        val newButton = makeButtons(func._1.toUpperCase)

        boxPanel.contents += Swing.VStrut(10)
        boxPanel.contents += newButton

        listenTo(newButton)
        reactions += {
          case ButtonClicked(`newButton`) => {
            addToList(func._1, func._2)
          }
        }
      }

    }

    boxPanel.background = GameLookConstants.GAME_BACKGROUND

    boxPanel
  }

  /**
   * Making GUI for inserting name of newly made function
   *
   * @return
   */
  def makeNameTitle: BoxPanel = {
    val boxPanel = new BoxPanel(Orientation.Horizontal)

    val nameLabel = new Label("Function name:")
    nameLabel.font = GameLookConstants.MENU_TITLE_FONT
    nameLabel.yLayoutAlignment = 0.5f

    functionName.font = GameLookConstants.MENU_TITLE_FONT
    functionName.foreground = GameLookConstants.MENU_TITLE
    functionName.listenTo(functionName.keys)
    functionName.reactions += {
      case e: KeyTyped => {
        checkFunctionName
      }
    }

    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += nameLabel
    boxPanel.contents += Swing.HStrut(10)
    boxPanel.contents += functionName
    boxPanel.contents += Swing.HStrut(10)

    boxPanel.background = GameLookConstants.GAME_BACKGROUND
    boxPanel
  }

  title = "NEW FUNCTIONS"

  //Creating menu title
  val header: Label = new Label("NOVA FUNKCIJA")
  header.xLayoutAlignment = 0.5f
  header.foreground = GameLookConstants.MENU_TITLE
  header.font = GameLookConstants.MENU_TITLE_FONT

  //Creating a boxPanel and inserting all the previously made components with spaces in between
  val boxPanel = new BoxPanel(Orientation.Vertical)

  //Putting the header first
  boxPanel.contents += header
  boxPanel.contents += Swing.VStrut(30)
  boxPanel.contents += makeNameTitle
  boxPanel.contents += Swing.VStrut(30)
  boxPanel.contents += new ScrollPane(makeFunctions)
  boxPanel.contents += Swing.VStrut(30)
  boxPanel.contents += makeMessageBoardAndSave

  boxPanel.border = Swing.EmptyBorder(50, 50, 50, 50)
  boxPanel.xLayoutAlignment = 0.5f
  boxPanel.background = GameLookConstants.GAME_BACKGROUND

  contents = boxPanel

  size = new Dimension(800, 900)
  visible = true
  resizable = false
  //centering the window to middle of screen
  peer.setLocationRelativeTo(null)
}
