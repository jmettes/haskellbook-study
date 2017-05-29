-- learn.hs

module Learn where
-- First, we declare the name of our module so -- it can be imported by name in a project.
-- We won't be doing a project of this size
-- for a while yet.
myResult = x * 5

area x = 3.14 * (x * x)
double x = x * 2
x = 7
y = 10
f = x + y

mult1       = x * y
    where x = 5
          y = 6

mult2 = x * 3 + y
    where x = 3
          y = 1000

mult3 = x * 5
    where y = 10
          x = 10 * 5 + y

mult4 = z / x + y
    where x = 7
          y = negate x
          z = y * 10

waxOn = x * 5
    where z=7
          y=z+8
          x=y^2

triple x = x * 3

waxOff x = triple x