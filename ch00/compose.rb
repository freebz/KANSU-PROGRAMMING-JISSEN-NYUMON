# compose.rb
def f(x)
  if x < 1
    0
  else
    "foo"
  end
end

def g(n)
  n+1
end

p g(f(0))  # OK
p g(f(1))  # NG
