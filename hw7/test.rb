require "./hw7.rb"

ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0
a=Point.new(FIVE,SEVEN).intersect(LineSegment.new(ONE,ONE,FIVE,SEVEN))
if not (a.x == FIVE and a.y == SEVEN)
    puts "error case 1"
end

a=Point.new(FIVE,FIVE).intersect(LineSegment.new(ONE,ONE,FIVE,SIX))
if not (a.is_a? NoPoints)
    puts "error case 2"
end
a=Line.new(FIVE,ZERO).intersect(LineSegment.new(ONE,FIVE,TWO,TWO))
if not (a.x == ONE and a.y == FIVE)
    puts "error case 3"
end
a=Line.new(FIVE,ZERO).intersect(LineSegment.new(-ONE,-ONE,ONE,FIVE))
if not (a.x == ONE and a.y == FIVE)
    puts "error case 4"
end
a=LineSegment.new(ONE,FIVE,TWO,TWO).intersect(Line.new(FIVE,ZERO))
if not (a.x == ONE and a.y == FIVE)
    puts "error case 5"
end
a=VerticalLine.new(THREE).intersect(LineSegment.new(-ONE,-ONE,THREE,THREE))
if not (a.x == THREE and a.y == THREE)
    puts "error case 6"
end

a=Point.new(FIVE,FIVE).intersect(LineSegment.new(-ONE,FIVE,TEN,FIVE))
#if not (a.is_a? Point)
if not (a.x == FIVE and a.y == FIVE)
    puts "error case 7"
end

