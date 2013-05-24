require './hw7_2.rb'

ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0

a=LineSegment.new(ONE,ONE,FOUR,ONE).intersect(LineSegment.new(THREE,ZERO,THREE,TWO))
if not (a.is_a? NoPoints)
    puts "error case 1"
end
