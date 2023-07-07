import scalafx.animation.AnimationTimer
import scalafx.application.JFXApp3
import scalafx.scene.{Group, Scene}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scala.util.Random
import scalafx.Includes.jfxScene2sfx
import scala.concurrent.Promise

object MazeSimulation extends JFXApp3 {
  val mazeSize = 20
  val cellSize = 20

  var maze: Array[Array[Point]] = Array.ofDim[Point](mazeSize, mazeSize)
  var current = new Point(0, 0)
  var stack = List[Point]()

  class Point(val x: Int, val y: Int) {
    var visited = false
    var walls = Array(true, true, true, true)

    def checkNeighbors(): Option[Point] = {
      val neighbors = List(
        if (x > 0) Some(maze(x - 1)(y)) else None,
        if (y > 0) Some(maze(x)(y - 1)) else None,
        if (x < mazeSize - 1) Some(maze(x + 1)(y)) else None,
        if (y < mazeSize - 1) Some(maze(x)(y + 1)) else None
      ).flatten.filter(!_.visited)

      if (neighbors.nonEmpty) Some(neighbors(Random.nextInt(neighbors.length))) else None
    }
  }

  class SimulationTimer() {
    var reachedExit = false
    private val timerPromise = Promise[AnimationTimer]()
    private lazy val timer: AnimationTimer = AnimationTimer(animationTime => handle(animationTime, timer))

    private def handle(animationTime: Long, timer: AnimationTimer): Unit = {
      current.visited = true

      current.checkNeighbors() match {
        case Some(next) =>
          stack = current :: stack

          if (current.x - next.x == 1) {
            current.walls(3) = false
            next.walls(1) = false
          } else if (current.x - next.x == -1) {
            current.walls(1) = false
            next.walls(3) = false
          }

          if (current.y - next.y == 1) {
            current.walls(0) = false
            next.walls(2) = false
          } else if (current.y - next.y == -1) {
            current.walls(2) = false
            next.walls(0) = false
          }

          current = next

          if (current.x == mazeSize - 1 && current.y == mazeSize - 1) {
            reachedExit = true
            //timer.stop() // Arrêter la simulation lorsque le carré rouge atteint la sortie
          }
        case None if stack.nonEmpty =>
          current = stack.head
          stack = stack.tail
        case _ =>
      }

      drawMaze()
    }

    def start(): Unit = {
      timerPromise.success(timer)
      timer.start()
    }
  }

  override def start(): Unit = {
    for (i <- 0 until mazeSize; j <- 0 until mazeSize) {
      maze(i)(j) = new Point(i, j)
    }

    stage = new JFXApp3.PrimaryStage {
      title.value = "Maze Simulation"
      scene = new Scene(mazeSize * cellSize, mazeSize * cellSize) {
        content = new Group
      }
    }

    val simulationTimer = new SimulationTimer()
    simulationTimer.start()
  }

  def drawMaze(): Unit = {
    val content = for (i <- 0 until mazeSize; j <- 0 until mazeSize) yield {
      val point = maze(i)(j)
      val cell = new Rectangle {
        width = cellSize
        height = cellSize
        x = i * cellSize
        y = j * cellSize
        fill =
          if (i == current.x && j == current.y) Color.Red
          else if (i == mazeSize - 1 && j == mazeSize - 1) Color.Green // exit cell
          else if (point.visited) Color.White
          else Color.Black
      }

      val walls = point.walls.zipWithIndex.flatMap { case (hasWall, idx) =>
        if (hasWall) {
          val wall = idx match {
            case 0 => new Rectangle { // top wall
              width = cellSize
              height = 1
              x = i * cellSize
              y = j * cellSize
              fill = Color.Black
            }
            case 1 => new Rectangle { // right wall
              width = 1
              height = cellSize
              x = (i + 1) * cellSize - 1
              y = j * cellSize
              fill = Color.Black
            }
            case 2 => new Rectangle { // bottom wall
              width = cellSize
              height = 1
              x = i * cellSize
              y = (j + 1) * cellSize - 1
              fill = Color.Black
            }
            case 3 => new Rectangle { // left wall
              width = 1
              height = cellSize
              x = i * cellSize
              y = j * cellSize
              fill = Color.Black
            }
          }
          Some(wall)
        } else {
          None
        }
      }

      cell +: walls
    }

    stage.scene().content = content.flatten
  }
}
