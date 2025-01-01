#!/usr/bin/env ruby
result = 0
File.open("input") do |f|
  f.each_line do |line|
    report_diffs = line.strip.split(" ").map(&:to_i).each_cons(2).map{|a,b| b - a}
    p line
    p report_diffs
    if (report_diffs.all?(&:positive?) || report_diffs.all?(&:negative?))
      report_diffs_abs = report_diffs.map(&:abs)
      p report_diffs_abs
      if report_diffs_abs.max <= 3 && report_diffs_abs.min >= 1
        puts "ADD 1"
        result += 1
      end
    end
  end
end
puts result
