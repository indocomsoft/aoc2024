#!/usr/bin/env ruby
first_list = []
second_list = []
File.open("input") do |f|
  f.each_line do |line|
    a, b = line.strip.split("   ")
    first_list << a.to_i
    second_list << b.to_i
  end
end
first_list.sort!
second_list.sort!
# Part one
puts first_list.zip(second_list).map {(_1 - _2).abs}.sum

# Part two
frequencies = second_list.group_by(&:itself).transform_values(&:size)
puts first_list.map {_1 * (frequencies[_1] || 0)}.sum
