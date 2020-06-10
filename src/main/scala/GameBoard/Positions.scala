package GameBoard

/**
 * Storing the current and starting position of game
 *
 * @param currentPosition
 * @param startingPosition
 */
class Positions (val currentPosition: (Int, Int), val startingPosition: (Int, Int)){
  /**
   * Changing the current position of player and returning a new object of Positions
   *
   * @param row
   * @param col
   * @return
   */
  def changeCurrentPosition(row: Int, col: Int): Positions = {
    new Positions((row,col), startingPosition)
  }

  /**
   * Changing the starting position og game and returning a new object of Position
   *
   * @param row
   * @param col
   * @return
   */
  def changeStartingPosition(row: Int, col: Int): Positions = {
    new Positions(currentPosition, (row, col))
  }

  /**
   * Changing the current and starting position of game and returning a new object of Position
   *
   * @param rowCurr
   * @param colCurr
   * @param rowStart
   * @param colStart
   * @return
   */
  def changeCurrentAndStartingPosition(rowCurr: Int, colCurr: Int, rowStart: Int, colStart: Int): Positions = {
    new Positions((rowCurr, colCurr), (rowStart, colStart))
  }

  //------------ Movement of currentPosition by 1 step -----------------------------

  def moveCurrentPositionUp: Positions = {
    if (currentPosition._1 != 0)
      changeCurrentPosition(currentPosition._1 - 1, currentPosition._2)
    else
      this
  }

  def moveCurrentPositionDown: Positions = {
    if (currentPosition._1 != 8)
      changeCurrentPosition(currentPosition._1 + 1, currentPosition._2)
    else
      this
  }

  def moveCurrentPositionLeft: Positions = {
    if (currentPosition._2 != 0)
      changeCurrentPosition(currentPosition._1, currentPosition._2 - 1)
    else
      this
  }

  def moveCurrentPositionRight: Positions = {
    if (currentPosition._2 != 8)
      changeCurrentPosition(currentPosition._1, currentPosition._2 + 1)
    else
      this
  }

}
