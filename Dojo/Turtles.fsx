(*
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Creating Turtle graphics in F#

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The goal of this exercise is to show how F# discriminated
unions can be used to design a Domain specific language, 
or DSL.

We will write a simplfied version of the LOGO/Turtle 
language, and use it to create an SVG image of our 
turtle's path.

For a quick example of Turtle/LOGO, here is an online 
version: http://www.transum.org/software/Logo/

Our strategy will be to create the instructions for our 
language, using F# discriminated unions, and transform a 
list of instructions into a list of SVG lines, which we will
put into an html document that can be viewed in the browser.
*)


(*
Creating an image with SVG
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

See the following for information on SVG:
https://www.w3schools.com/graphics/svg_intro.asp
*)


// TODO: run the following block of code.

let svgExample = """
<html>
<body>
  <h1>Turtles & F#!</h1>
    <svg width="100" height="100">
      <line x1="10" y1="20" x2="90" y2="80" stroke="red" stroke-width="2" />
    </svg>
  </body>
</html>
"""

let outputPath = __SOURCE_DIRECTORY__ + "/turtles.html"

System.IO.File.WriteAllText(outputPath, svgExample)


// TODO: this should have created a document "turtle.html"
// This is our output: open it in your browser.



(*
Filling in a template
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The previous example uses a static SVG file, that draws
a single line.
We want to create the content of that file dynamically, 
using code to add as many lines as we want.
*)

// We will use sprintf to format strings.
// This example inserts "some content" in a template, 
// using %s as a placeholder.
// TODO: run the following block of code.

let sprintfDemo = sprintf "<content start>%s</content end>" "some content"

let svgTemplate content =
    sprintf """
<html>
<body>
    <h1>Turtles & F#!</h1>
    <svg width="500" height="500">
%s
    </svg>
</body>
</html>""" content

let svgLine ((x1, y1), (x2, y2)) =
    sprintf 
        """<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="black" />""" 
        x1 y1 x2 y2 


// Now we have: 
// a function svgLine that takes 2 points and creates
// a SVG line between them,
// a function svgTemplate that takes in content and 
// inserts it into an html page.
// We can now create more interesting graphics!


// TODO
// run the following block of code.
// We create first a list of pairs of points, which we
// transform into a list of SVG lines, and insert
// into the SVG template. 

let pointsForSquare = 
    [ 
        (20.0, 20.0), (20.0, 100.0)
        (20.0, 100.0), (100.0, 100.0)
        (100.0, 100.0), (100.0, 20.0)
        (100.0, 20.0), (20.0, 20.0)
    ]

let squareAsSvg =
    pointsForSquare
    // make a line for each point
    |> List.map svgLine
    // contatenate into one string
    |> String.concat "\n"
    // inject into the template
    |> svgTemplate

System.IO.File.WriteAllText(outputPath, squareAsSvg)

// At that point, the turtles.html file should have
// been updated, and show a square.


// TODO:
// create a triangle with sides of length 30, 40 and 50.
// Note: this is a right triangle.
// Or... create whatever shape you want! :)




(*
Defining our language
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Next, we are going to define our version of the Turtle/LOGO
language, using Discriminated Unions.
*)

// Our initial language contains 3 instructions for the turtle:
// (go) forward, (turn) left, repeat
type INSTRUCTION =
    | FORWARD of float // move fwd by x pixels
    | LEFT of float // turn left by x degrees
    | REPEAT of int * INSTRUCTION list // repeat n times instructions

// we can now write a simple program, 
// as a list of INSTRUCTIONs
let simpleProgram = [ FORWARD 50.0; LEFT (90.0); FORWARD 50.0 ]

// or a more complex one, with a 'nested program':
let complexProgram =
    [
        FORWARD 100.0
        LEFT (90.0)
        REPEAT (5, 
            // this is a "nested program"
            [ 
                FORWARD 50.0
                LEFT (90.0) 
            ])
    ]


// TODO
// simply run the code above



(*
Computing the position of the Turtle
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

What we want now is to take the initial position of the 
turtle, apply a list of instructions, and get back the 
list of all the successive states of the turtle.
*)

// The current state of the turtle consists of: 
// a Position = where the turtle is on screen
// an Angle = what direction the turtle is facing.
// We create a type, State, to describe that: 
type State = { X:float; Y:float; Angle:float }

// We will work with 360 degrees, but will need
// to convert to radians for calculation purposes.
// We use the built-in value for Pi.
let PI = System.Math.PI
let toRadians angle = angle * 2.0 * PI / 360.0

// moveForward takes a state of type State, 
// the current position of our turtle, 
// and computes back where the turtle lands 
// if we push it in its current direction
// for a given distance.
let moveForward (state:State) distance =
    { state with
        X = state.X + distance * cos (state.Angle |> toRadians)
        Y = state.Y + distance * sin (state.Angle |> toRadians) 
    }

let turn (state:State) angle =
    { state with 
        Angle = (state.Angle + angle) % 360.0 
    }

let turtleState = {
    X = 0.0
    Y = 0.0
    Angle = 180.0
    }

let afterMove1 = moveForward turtleState 100.0
let afterMove2 = turn afterMove1 90.0


(*
Execute a list of instructions to move the Turtle
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*)


// We want to transform a program - a list of instructions
// for the turtle - into a list of states. We will take
// the instructions one by one, progressively computing the
// new position and appending it to a list of states.
// We will do that, until we have no further instructions.
let rec execute (states: State list) (program: INSTRUCTION list) =
    match states with
    // we need a starting state
    | [] -> failwith "Starting state required"
    // the current state is the head of the list of states
    | currentState :: previousStates ->
        match program with
        // When there is no instruction left, 
        // return the list of states: we are done
        | [] -> states
        // otherwise, pick the head instruction and use it
        // to compute the next state
        | head :: tail ->
            // for each instruction we have (FORWARD, LEFT, REPEAT),
            // apply the matching state transformation
            match head with
            | FORWARD distance ->
                let nextState = moveForward currentState distance
                execute (nextState :: states) tail
            | LEFT (angle) ->
                let nextState = turn currentState angle
                execute (nextState :: states) tail
            | REPEAT (repeat, sub) ->
                let rec runSub iter result =
                    match (iter < repeat) with
                    | false -> result
                    | true  ->
                        let result' = execute result sub
                        runSub (iter+1) result'
                let subResult = runSub 0 states
                execute subResult tail

// We wrap execute inside a run function, which takes
// just the starting state of the turtle, and a list
// of instructions:
let run (startState: State) (program: INSTRUCTION list) =
    let states = execute [ startState ] program
    states |> List.rev


// TODO: run the 2 initial programs, look at the output
// This should produce a list of the states the Turtle
// goes through, as the program executes

let startState = { X = 250.0; Y = 250.0; Angle = 0.0 }
let simpleProgramOutput = run startState simpleProgram


(*
Transforming the program output into an SVG file
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Running a program returns a list of states, the successive
positions of the Turtle. The only thing we need to do at
that point is take all these states, group them in pairs, 
and connect their positions with lines.
*)

let linesBetweenStates (output: State list) =
    output
    |> Seq.pairwise
    |> Seq.map (fun (state1, state2) ->
        (state1.X, state1.Y), (state2.X, state2.Y)) 

let save (content: string) = 
    System.IO.File.WriteAllText(outputPath, content)

let newLine = "\n"

let createSvg (startState: State) (program: INSTRUCTION list) =
    run startState program
    |> linesBetweenStates
    |> Seq.map svgLine
    |> String.concat newLine
    |> svgTemplate
    |> save


// TODO 
// createSvg from one of the sample programs,
// and open the turtle.html file in your browser

createSvg startState simpleProgram


// TODO
// create a program to draw and save a star?




(*
Extending the language: RIGHT
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

We currently have LEFT turns covered; technically, 
that's enough, but it would be nice to have a RIGHT
turn instruction... 
*)


// TODO
// modify the INSTRUCTIONS and add RIGHT
// modify the execute function accordingly




(*
Extending the language: pen size
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Let's make our images a bit fancier, for instance, by 
allowing the user to change the size of our pen. 
We need now to add an instruction, SETPENSIZE of size, 
and we probably also need to add the current pen size 
to the state, so that we can modify the way we create 
and svg line...
*)


// TODO
// modify the INSTRUCTIONS and add SETPENSIZE
// modify the State record, to include a PenSize value
// modify the execute function accordingly
// modify the svgLine function to take in a size




(*
Extensions / going further
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Here we are - we have implemented a simple version of LOGO!

At that point, what you want to do next is entirely up to you.
* You could try to add a couple of instructions, 
like SETCOLOR, or PENUP / PENDOWN. 
* Or, more ambitious, you could support variables, or FOR loops, 
or crazier things.

For reference, here are some more instructions from Logo:

http://derrel.net/ep/logo/logo_com.htm
http://www.snee.com/logo/logo4kids.pdf

In a totally different direction, perhaps you could build an 
interactive logo session, animating the Turtle?

Or - could we use Units of Measure to have a cleaner representation
for angles and length?

Otherwise, you can also check this video with Tomas Petricek,
which demonstrates how similar ideas can be used in different 
contexts:
https://vimeo.com/97315970
*)