package Sudoku

import GameBoard.ChangeSudokuBoard
import org.junit._
import org.junit.Assert._

class ChangeSudokuTests {

  trait ChangeSudokuExamples {
    val sudoku1: ChangeSudokuBoard = new ChangeSudokuBoard("src/test/scala/TestBoards/Easy.txt", null)
    sudoku1.newSudokuBoard.visible = false
    val sudoku2: ChangeSudokuBoard = new ChangeSudokuBoard("src/test/scala/TestBoards/Solved.txt", null)
    sudoku2.newSudokuBoard.visible = false
    val sudoku3: ChangeSudokuBoard = new ChangeSudokuBoard("src/test/scala/TestBoards/Unsolvable.txt", null)
    sudoku3.newSudokuBoard.visible = false
    val sudoku4: ChangeSudokuBoard = new ChangeSudokuBoard("src/test/scala/TestBoards/Bad.txt", null)
    sudoku4.newSudokuBoard.visible = false
  }

  @Test def `erase number from board`: Unit =
    new ChangeSudokuExamples {
      assertEquals(5, sudoku1.sudokuTable(0)(2)._1)

      sudoku1.callPositionChange(0,2,sudoku1.newSudokuBoard)
      sudoku1.eraseNumber
      assertEquals(0, sudoku1.sudokuTable(0)(2)._1)
    }

  @Test def `input number on board`: Unit =
    new ChangeSudokuExamples {
      assertEquals(0, sudoku1.sudokuTable(0)(0)._1)

      sudoku1.callPositionChange(0,0, sudoku1.newSudokuBoard)
      sudoku1.inputNumber(5)
      assertEquals(5, sudoku1.sudokuTable(0)(0)._1)

      //Insert number on an already filled space
      assertEquals(9, sudoku1.sudokuTable(1)(0)._1)
      sudoku1.callPositionChange(1,0, sudoku1.newSudokuBoard)
      sudoku1.inputNumber(5)
      assertEquals(5, sudoku1.sudokuTable(1)(0)._1)
    }

  @Test def `change start position`: Unit =
    new ChangeSudokuExamples {
      assertEquals((8,8), sudoku2.positions.startingPosition)

      sudoku2.changeStartingPosition(1,1)
      assertEquals((1,1), sudoku2.positions.startingPosition)
    }

  @Test def `trasposition`: Unit =
    new ChangeSudokuExamples {
      assertEquals(1, sudoku4.sudokuTable(0)(0)._1)
      assertEquals(2, sudoku4.sudokuTable(0)(1)._1)
      assertEquals(3, sudoku4.sudokuTable(0)(2)._1)
      assertEquals(4, sudoku4.sudokuTable(0)(3)._1)
      assertEquals(5, sudoku4.sudokuTable(0)(4)._1)
      assertEquals(6, sudoku4.sudokuTable(0)(5)._1)
      assertEquals(7, sudoku4.sudokuTable(0)(6)._1)
      assertEquals(8, sudoku4.sudokuTable(0)(7)._1)
      assertEquals(9, sudoku4.sudokuTable(0)(8)._1)

      sudoku4.transposition

      assertEquals(1, sudoku4.sudokuTable(0)(0)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(1)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(2)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(3)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(4)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(5)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(6)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(7)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(8)._1)
    }

  @Test def `changeUp`: Unit =
    new ChangeSudokuExamples {
      assertEquals(1, sudoku4.sudokuTable(0)(0)._1)
      assertEquals(2, sudoku4.sudokuTable(0)(1)._1)
      assertEquals(3, sudoku4.sudokuTable(0)(2)._1)
      assertEquals(4, sudoku4.sudokuTable(0)(3)._1)
      assertEquals(5, sudoku4.sudokuTable(0)(4)._1)
      assertEquals(6, sudoku4.sudokuTable(0)(5)._1)
      assertEquals(7, sudoku4.sudokuTable(0)(6)._1)
      assertEquals(8, sudoku4.sudokuTable(0)(7)._1)
      assertEquals(9, sudoku4.sudokuTable(0)(8)._1)

      sudoku4.changeUp

      assertEquals(8, sudoku4.sudokuTable(0)(0)._1)
      assertEquals(7, sudoku4.sudokuTable(0)(1)._1)
      assertEquals(6, sudoku4.sudokuTable(0)(2)._1)
      assertEquals(5, sudoku4.sudokuTable(0)(3)._1)
      assertEquals(4, sudoku4.sudokuTable(0)(4)._1)
      assertEquals(3, sudoku4.sudokuTable(0)(5)._1)
      assertEquals(2, sudoku4.sudokuTable(0)(6)._1)
      assertEquals(1, sudoku4.sudokuTable(0)(7)._1)
      assertEquals(0, sudoku4.sudokuTable(0)(8)._1)
    }

  @Test def  `filter row and column`: Unit =
    new ChangeSudokuExamples {
      assertEquals(2, sudoku3.sudokuTable(1)(4)._1)
      assertEquals(2, sudoku3.sudokuTable(1)(6)._1)
      assertEquals(2, sudoku3.sudokuTable(5)(3)._1)

      sudoku3.callPositionChange(1,3, sudoku3.newSudokuBoard)
      sudoku3.inputNumber(2)
      sudoku3.filterRowAndColumn(1,3)

      assertEquals(0, sudoku3.sudokuTable(1)(4)._1)
      assertEquals(0, sudoku3.sudokuTable(1)(6)._1)
      assertEquals(0, sudoku3.sudokuTable(5)(3)._1)
    }

  @Test def  `filter square`: Unit =
    new ChangeSudokuExamples {
      assertEquals(2, sudoku3.sudokuTable(0)(4)._1)
      assertEquals(2, sudoku3.sudokuTable(1)(4)._1)

      sudoku3.callPositionChange(1,3, sudoku3.newSudokuBoard)
      sudoku3.inputNumber(2)
      sudoku3.filterSubSquare(1,3)

      assertEquals(0, sudoku3.sudokuTable(0)(4)._1)
      assertEquals(0, sudoku3.sudokuTable(1)(4)._1)
    }

  @Test def `sudoku solvable`: Unit = {
    new ChangeSudokuExamples {
      assertEquals(true, sudoku1.solveSudoku)
      assertEquals(false, sudoku3.solveSudoku)
    }
  }

  /*
  @Test def `composite functions`: Unit =
    new ChangeSudokuExamples {
      assertEquals(2, sudoku3.sudokuTable(0)(4)._1)
      assertEquals(2, sudoku3.sudokuTable(1)(4)._1)
      assertEquals(2, sudoku3.sudokuTable(1)(6)._1)
      assertEquals(2, sudoku3.sudokuTable(5)(3)._1)

      sudoku3.userFunctions.addFunctions(List(sudoku3.inputNumberWrapper))
      sudoku3.userFunctions.addFunctions(List(sudoku3.filterRowAndColWrapper))
      sudoku3.userFunctions.addFunctions(List(sudoku3.filterSquareWrapper))

      sudoku3.callPositionChange(1,3, sudoku3.newSudokuBoard)
      //After the dialog appears, must insert 2 to finish test
      sudoku3.executeFunctionList(sudoku3.userFunctions.makeCompositeFunction)

      assertEquals(0, sudoku3.sudokuTable(0)(4)._1)
      assertEquals(0, sudoku3.sudokuTable(1)(4)._1)
      assertEquals(0, sudoku3.sudokuTable(1)(6)._1)
      assertEquals(0, sudoku3.sudokuTable(5)(3)._1)
    }
   */

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

}
