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
}
