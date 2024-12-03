# Some utility stuff
def r # reloader
  load __FILE__
end

def input_ints(filename)
  File.read(filename)
  .split(/\n/)
  .map { _1.split(/\s+/).map(&:to_i) }
end

# Solutions
## Day 1
def day1_data
  input_ints("day1.txt").transpose
end

def day1
  day1_data.map(&:sort) => a, b
  a.zip(b).map { (_1 - _2).abs }.sum
end

def day1_b
  day1_data => a, b
  t = b.tally
  a.map { _1 * (t[_1] || 0) }.sum
end

## Day 2
def day2_data
  input_ints("day2.txt")
end

def day2_safe?(line)
  # Ok this is very inefficient but the input is very small, so let's stay "declaratative":
  line.sort.then { _1 == line || _1.reverse == line } && line.inject { |acc, x| acc && (x-acc).abs.between?(1,3) && x }
end

def day2
  day2_data.select { |line| day2_safe?(line) }.count
end

def day2_b
  day2_data.select do |line|
    safe = day2_safe?(line) || begin
      # If we are here, report is unsafe, let's try re-checking all combinations without one element
      line.combination(line.size - 1).any? { day2_safe?(_1) }
    end
  end.count
end

def day3
  File.read("day3.txt").scan(/mul\((\d+),(\d+)\)/).map {|a,b| a.to_i*b.to_i}.sum
end
