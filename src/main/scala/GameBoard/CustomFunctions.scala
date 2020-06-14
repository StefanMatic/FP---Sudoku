package GameBoard

class CustomFunctions(changeSudokuBoard: ChangeSudokuBoard) {
  var functionList: FunctionListType = makeInitFunctionButtons
  var functions: List[FunctionWrapper] = Nil

  /**
   * Making initial list of basic board manipulation functions
   *
   * @return
   */
  def makeInitFunctionButtons: FunctionListType = {
    val list1 = ("Input number", List[FunctionWrapper](changeSudokuBoard.inputNumberWrapper))
    val list2 = ("Erase number", List[FunctionWrapper](changeSudokuBoard.eraseNumberWrapper))
    val list3 = ("Change start position", List[FunctionWrapper](changeSudokuBoard.changeStartPositionWrapper))
    val list4 = ("Filter row and column", List[FunctionWrapper](changeSudokuBoard.filterRowAndColWrapper))
    val list5 = ("Filter square", List[FunctionWrapper](changeSudokuBoard.filterSquareWrapper))
    val list6 = ("Transpose", List[FunctionWrapper](changeSudokuBoard.transposeWrapper))
    val list7 = ("Change up", List[FunctionWrapper](changeSudokuBoard.changeUpWrapper))

    list1 :: list2 :: list3 :: list4 :: list5 :: list6 :: list7 :: Nil
  }

  /**
   * Adding a new custom made function
   *
   * @param name
   * @param newFunctions
   * @return
   */
  def addFunctionToList(name: String, newFunctions: List[FunctionWrapper]): Unit = {
    functionList = functionList ::: List((name, newFunctions))
    functions = Nil
  }

  /**
   * Adding a function to the list of selected functions for further custom functions
   *
   * @param func
   */
  def addFunctions(func: List[FunctionWrapper]): Unit = {
    functions = functions::: func
  }

  /**
   * User canceled the process of making a new custom function, so the list of currently selected functions is erased
   */
  def noFunction: Unit = {
    functions = Nil
  }

  /**
   * Making a composite function and returning it
   *
   * @return
   */
  def makeCompositeFunction: List[FunctionWrapper] = {
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

    val compositeFunction: FunctionWrapper = functions.foldLeft(dummyFunc)(makeCompose)
    List(compositeFunction)
  }

  /**
   * Returning the sequence of chosen functions
   *
   * @return
   */
  def makeSequenceFunction: List[FunctionWrapper] = {
    functions
  }


}
