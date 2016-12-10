import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer


// Type Declaration

// Variable Declaration
val lines = Source.fromFile("Instructions.txt").getLines.toList
val grid_size = lines(0).mkString.replaceAll(" ", "").toList
val size = (grid_size(0).toInt - 48, grid_size(1).toInt - 48)
val initial_postion = for{i <- 1 until lines.length if(i%2==1)} yield(lines(i).mkString.replaceAll(" ","").toList)
val moves = for{i <- 1 until lines.length if(i%2 == 0) } yield (lines(i).toList)
val nr_robots = initial_postion.length
val orientation = List('N','E','S','W')
var lost_robots = new ListBuffer[(Int,Int,Int)]()

var answer = ""


// Function used to check the final position of the robot
def check_location(dim:(Int, Int), x: Int, y: Int): Boolean = 
	if(x >= 0 && x <= dim._1 && y >= 0 && y <= dim._2) true else false

// Add Lost Lost check_location
def add_lost(position_x: Int, position_y: Int, index:Int) = {
	val x = (position_x, position_y, index)
	lost_robots += x
}
 
// Function used to write in the Solution File
def appendToFile(p: String, s: String): Unit = {
    val pw = new PrintWriter(new File(p))
    try pw.write(s) finally pw.close()
  }



for(robot <- 0 until nr_robots) {
	/**
	* Initialize position x and y for the new robot
	* And his initial orientation
	*/
	var position_x = initial_postion(robot)(0).toInt - 48
	var position_y = initial_postion(robot)(1).toInt - 48
	var index = orientation.indexOf(initial_postion(robot)(2))
	var lost = false
	for(move <- moves(robot)) {
		if(!lost) {

			// Check what type of intrusctions is
			move match {
				case 'R' =>  if(index == 3) index = 0
							 	else index += 1
				case 'L' =>  if(index == 0) index = 3
								else index -= 1
				case 'F' => {
					// Check for a robot “scent” before moving forward
					if(!lost_robots.contains((position_x, position_y, index))){
						orientation(index) match {
							case 'N' => if(check_location(size,position_x, position_y+1)) 
											position_y += 1
										else {
											 add_lost(position_x, position_y, index)
											 lost = true
										}
							
							case 'S' => if(check_location(size,position_x, position_y-1)) 
											position_y -= 1
										else {
											 add_lost(position_x, position_y, index)
											 lost = true
										}
							
							case 'E' => if(check_location(size,position_x + 1, position_x)) 
											position_x += 1
										else {
											add_lost(position_x, position_y, index)
											lost = true
										}
							
							case 'W' => if(check_location(size,position_x - 1, position_x)) 
											position_x -= 1
										else {
										 	add_lost(position_x, position_y, index)
										 	lost = true
										}
						}
					}
				}
			}
		}
	}

	// Create the output
	answer += "\n" + position_x + " " + position_y + " " + orientation(index)
	
	// Add the lost tag if the robot got lost
	if(lost) answer += " LOST"
}


// Write the answer in the Solution.txt file 
appendToFile("Solution.txt", answer)

// Print the answer to the terminal / IDE 
println(answer)