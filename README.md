# Turtles dojo

The goal of this dojo is to create a simple version of the [Turtle/Logo language][1], using F# discriminated unions, to produce [SVG graphics][2] like this one:

![Example output](output-example.PNG)

... which is generated from the following code:

``` fsharp
[
REPEAT (
    50, 
    (
    [
        FORWARD 10.0
        LEFT 10.
        REPEAT (5,
            [
                FORWARD 50.
                LEFT 140.
            ]
        )
    ]
))
]
```


## Running the code



[1]: http://www.transum.org/software/Logo/
[2]: https://www.w3schools.com/graphics/svg_intro.asp
