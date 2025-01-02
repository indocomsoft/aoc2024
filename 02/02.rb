#!/usr/bin/env ruby

def report_valid?(report)
  report_diffs = report.each_cons(2).map{|a,b| b - a}
  (report_diffs.all?(&:positive?) || report_diffs.all?(&:negative?)) && report_diffs.map(&:abs).max <= 3 && report_diffs.map(&:abs).min >= 1
end

result = 0
result_with_one_bad = 0
File.open("input") do |f|
  f.each_line do |line|
    report = line.strip.split(" ").map(&:to_i)
    if report_valid?(report)
        result += 1
        next
    end
    report.size.times do |i|
      report_with_one_bad = report.clone
      report_with_one_bad.delete_at(i)
      if report_valid?(report_with_one_bad)
        result_with_one_bad += 1
        break
      end
    end
  end
end
puts(result)
puts(result + result_with_one_bad)
