#!/usr/bin/env ruby

content = File.read("input")
puts content.scan(/mul\((\d+),(\d+)\)/).map {|a, b| a.to_i * b.to_i}.sum

puts(
  content
    .scan(/mul\((\d+),(\d+)\)|(do\(\))|(don't\(\))/)
    .reduce([0, true]) do |(sum, enabled), (a, b, maybe_do, maybe_dont)|
      if maybe_do
        [sum, true]
      elsif maybe_dont
        [sum, false]
      elsif enabled
        [sum + a.to_i * b.to_i, true]
      else
        [sum, false]
      end
    end
    .first
)
