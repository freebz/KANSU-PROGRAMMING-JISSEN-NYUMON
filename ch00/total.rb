# Ruby total.rb
# numbers 내부 값을 전부 더한다.
# total([1,2,3]) => 6
def total(numbers)
  numbers.inject(:+)
end
