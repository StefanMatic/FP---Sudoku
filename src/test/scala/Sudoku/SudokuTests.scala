package Sudoku

import org.junit._
import org.junit.Assert.assertEquals

class SudokuTests {
  import GameBoard._
  import GUI.GameLookConstants

  trait SudokuTableExamples {
    val sudoku1: SudokuBoard = new SudokuBoard("src/test/scala/TestBoards/Easy.txt", null)
    sudoku1.gameFrame.visible = false
    val sudoku2: SudokuBoard = new SudokuBoard("src/test/scala/TestBoards/Solved.txt", null)
    sudoku2.gameFrame.visible = false
    val sudoku3: SudokuBoard = new SudokuBoard("src/test/scala/TestBoards/Unsolvable.txt", null)
    sudoku3.gameFrame.visible = false
    val sudoku4:SudokuBoard = new SudokuBoard("src/test/scala/TestBoards/Bad.txt", null)
    sudoku4.gameFrame.visible = false
  }

  /**
   * Reading the input sudoku file and testing if the sudoku was correctly converted
   */
  @Test def `reading sudoku from file`: Unit =
    new SudokuTableExamples {
      assertEquals(0, sudoku1.sudokuTable(0)(0)._1)
      assertEquals(6, sudoku1.sudokuTable(1)(1)._1)
      assertEquals(4, sudoku1.sudokuTable(2)(2)._1)
      assertEquals(5, sudoku2.sudokuTable(3)(3)._1)
      assertEquals(9, sudoku2.sudokuTable(4)(4)._1)
      assertEquals(4, sudoku2.sudokuTable(5)(5)._1)
      assertEquals(0, sudoku3.sudokuTable(6)(6)._1)
      assertEquals(0, sudoku3.sudokuTable(7)(7)._1)
      assertEquals(0, sudoku3.sudokuTable(8)(8)._1)
    }

  /**
   * Testing if the starting position is at the right place
   */
  @Test def `start position check`: Unit =
    new SudokuTableExamples {
      assertEquals((5,4), sudoku1.positions.startingPosition)
      assertEquals((8,8), sudoku2.positions.startingPosition)
      assertEquals((0,6), sudoku3.positions.startingPosition)
    }

  /**
   * Testing functionality of inputting a number on board on a legal spot
   */
  @Test def `correctly input number`: Unit =
    new SudokuTableExamples {
      assertEquals(GameLookConstants.CODE_OK, insertNumberOnBoard(2,6,5, sudoku1.sudokuTable))
      assertEquals(GameLookConstants.CODE_OK, insertNumberOnBoard(8,8,8, sudoku2.sudokuTable))
      assertEquals(GameLookConstants.CODE_OK, insertNumberOnBoard(8,8,4, sudoku3.sudokuTable))
    }

  /**
   * Testing functionality of inputting a number on top of an another user number
   */
  @Test def `override number`: Unit =
    new SudokuTableExamples {
      //Inputting a number twice in a row
      if (GameLookConstants.CODE_OK == insertNumberOnBoard(2,6,5, sudoku1.sudokuTable)) {
        insertNumberOnBoard(2,6,6, sudoku1.sudokuTable)
        assertEquals(6, sudoku1.sudokuTable(2)(6)._1)
      } else {
        assert(true, "Error with inserting the first number")
      }
    }

  /**
   * Testing functionality of inputting a number on board on a semi - legal spot
   */
  @Test def `input number with warning`: Unit =
    new SudokuTableExamples {
      assertEquals(GameLookConstants.CODE_WARNING, insertNumberOnBoard(0,0,5, sudoku1.sudokuTable))
      assertEquals(5, sudoku1.sudokuTable(0)(0)._1)
    }

  /**
   * Testing functionality of inputting a number on board on an illegal spot
   */
  @Test def `input number with error`: Unit =
    new SudokuTableExamples {
      assertEquals(GameLookConstants.CODE_ERROR, insertNumberOnBoard(1,0,3, sudoku1.sudokuTable))
      assertEquals(GameLookConstants.CODE_ERROR, insertNumberOnBoard(1,1,3, sudoku2.sudokuTable))
      assertEquals(GameLookConstants.CODE_ERROR, insertNumberOnBoard(1,1,3, sudoku1.sudokuTable))
    }

  /**
   * Testing functionality of erasing a legal number (user inputted)
   */
  @Test def `correctly erase number`: Unit =
    new SudokuTableExamples {
      //Inserting a number in sudoku1 and erasing it
      insertNumberOnBoard(0,0,5,sudoku1.sudokuTable)
      assertEquals(GameLookConstants.CODE_OK, eraseNumberFromBoard(0,0,sudoku1.sudokuTable))
      assertEquals(0, sudoku1.sudokuTable(0)(0)._1)

      //Inserting a number in sudoku3 and erasing it
      insertNumberOnBoard(0,0,3, sudoku3.sudokuTable)
      assertEquals(GameLookConstants.CODE_OK, eraseNumberFromBoard(0,0,sudoku3.sudokuTable))
      assertEquals(0, sudoku3.sudokuTable(0)(0)._1)
    }

  /**
   * Testing functionality of erasing an original board number
   */
  @Test def `erase number with error`: Unit =
    new SudokuTableExamples {
      assertEquals(GameLookConstants.CODE_ERROR, eraseNumberFromBoard(0,3,sudoku1.sudokuTable))
      assertEquals(3, sudoku1.sudokuTable(0)(3)._1)

      assertEquals(GameLookConstants.CODE_ERROR, eraseNumberFromBoard(0,0,sudoku2.sudokuTable))
      assertEquals(1, sudoku2.sudokuTable(0)(0)._1)
    }

  /**
   * Testing functionality of executing instructions read from a file
   */
  @Test def `instructions from file`: Unit =
    new SudokuTableExamples {
      assertEquals(0, sudoku3.sudokuTable(6)(3)._1)
      assertEquals(0, sudoku3.sudokuTable(7)(3)._1)
      assertEquals(0, sudoku3.sudokuTable(7)(2)._1)
      assertEquals(0, sudoku3.sudokuTable(6)(2)._1)

      sudoku3.callPositionChange(6,3, sudoku3.gameFrame)
      sudoku3.readInstructionsFromFile("src/test/scala/TestInstructions/Instructions.txt")

      assertEquals(5, sudoku3.sudokuTable(6)(3)._1)
      assertEquals(3, sudoku3.sudokuTable(7)(3)._1)
      assertEquals(1, sudoku3.sudokuTable(7)(2)._1)
      assertEquals(7, sudoku3.sudokuTable(6)(2)._1)
    }

  /**
   * Testing functionality of checking to see if user input follows the rules of the game
   */
  @Test def `move correct`: Unit =
    new SudokuTableExamples {
      assertEquals(true, checkIfMoveCorrect(2, 6, 5, getMatrixOfValue(sudoku1.sudokuTable)))

      //Has the same value in square
      assertEquals(false, checkIfMoveCorrect(0, 0, 5, getMatrixOfValue(sudoku1.sudokuTable)))

      //Has the same value in row
      assertEquals(false, checkIfMoveCorrect(0, 5, 5, getMatrixOfValue(sudoku1.sudokuTable)))

      //Has the same value in column
      assertEquals(false, checkIfMoveCorrect(8, 0, 9, getMatrixOfValue(sudoku1.sudokuTable)))

      //Has the same value in square, row and column
      assertEquals(false, checkIfMoveCorrect(1,3, 2, getMatrixOfValue(sudoku3.sudokuTable)))
    }

  /**
   * Testing functionality checking to see if the sudoku board is completely filled
   */
  @Test def `board completed`: Unit =
    new SudokuTableExamples {
      assertEquals(false, checkIfSudokuFinished(getMatrixOfValue(sudoku1.sudokuTable)))

      insertNumberOnBoard(8,8,8, sudoku2.sudokuTable)
      assertEquals(true, checkIfSudokuFinished(getMatrixOfValue(sudoku2.sudokuTable)))
    }

  /**
   * Testing functionality of checking to see if the completely filled sudoku board is correct (follows the rules of the game)
   */
  @Test def `board correctly filled`: Unit =
    new SudokuTableExamples {
      insertNumberOnBoard(8,8,8, sudoku2.sudokuTable)
      insertNumberOnBoard(8,8,9, sudoku4.sudokuTable)

      assertEquals(true, checkIfSudokuFinished(getMatrixOfValue(sudoku2.sudokuTable)))
      assertEquals(true, checkIfSudokuFinished(getMatrixOfValue(sudoku4.sudokuTable)))

      assertEquals(true, checkIfSudokuCorrect(getMatrixOfValue(sudoku2.sudokuTable)))
      assertEquals(false, checkIfSudokuCorrect(getMatrixOfValue(sudoku4.sudokuTable)))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
