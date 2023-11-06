val xLower = 2
val yLower = 4
val xUpper = 4
val yUpper = 8

var i = xLower
var j = yLower
while 
    j <= yUpper
do
    println((i, j))
    if i % (xUpper - xLower) == 0 && i != xLower then
        i = xLower
        j += 1
    else
        i += 1