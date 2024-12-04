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
  # Ok this is very inefficient but the input is very small, so let's stay "declarative":
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

def day3_b
  require 'strscan'
  ss = File.read("day3.txt").then { StringScanner.new _1 }
  result = 0
  start = true
  while !ss.eos? && start || ss.skip_until(/do\(\)/)
    line = ss.scan_until(/don\'t\(\)/)
    result += line.scan(/mul\((\d+),(\d+)\)/).map {|a,b| a.to_i*b.to_i}.sum
    start = false
  end
  result
end

def day4
  lines = File.read("day4.txt").split(/\n/)
  w = lines.first.length
  h = lines.count
  letters = lines.join.split('')
  sum = 0
  for x in 0..(w-1)
    for y in 0..(h-1)
      day4_all_directions(x, y, w, h).map do |ps|
        sum +=1 if ps.map { |xx, yy| letters[yy * w + xx] }.join == "XMAS"
      end
    end
  end
end

def day4_all_directions(x, y, w, h)
  x0 = [x, x+1, x+2, x+3]
  x1 = [x, x-1, x-2, x-3]
  y0 = [y, y+1, y+2, y+3]
  y1 = [y, y-1, y-2, y-3]
  candidates = []
  candidates << x0.map { [_1, y] } unless x > w - 4 # e
  candidates << x0.zip(y0) unless y > h - 4 || x > w - 4 # se
  candidates << y0.map { [x, _1] } unless y > h - 4  # s
  candidates << x1.zip(y0) unless x < 3 || y > h - 4 # sw
  candidates << x1.map { [_1, y] } unless x < 3 # w
  candidates << x1.zip(y1) unless x < 3 || y < 3 # nw
  candidates << y1.map { [x, _1] } unless y < 3 # n
  candidates << x0.zip(y1) unless x > w - 4 || y < 3 # ne
  candidates
end

def day4b
  lines = File.read("day4.txt").split(/\n/)
  w = lines.first.length
  h = lines.count
  letters = lines.join.split('')
  sum = 0
  hits = {}
  for x in 0..(w-1)
    for y in 0..(h-1)
      candidates = day4_all_directions_b(x, y, w, h)
      sum +=1 if !candidates.empty? && candidates.all? do |ps|
        word = ps.map { |xx, yy| letters[yy * w + xx] }.join
        word == "MAS" || word == "SAM"
      end
    end
  end
  sum
end

def day4_all_directions_b(x, y, w, h)
  if x < 1 || y < 1 || x > w - 2 || y > h - 2
    []
  else
    [
      [[x-1, y-1], [x,y], [x+1, y+1]],
      [[x-1, y+1], [x,y], [x+1, y-1]]
    ]
  end
end
