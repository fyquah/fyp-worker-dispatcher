import math

import matplotlib.pyplot as plt


t_max = 0.100
t_min = 0.003
steps = 1000
expected_change = 0.10

def calc_p(step, change):
  t_factor = -math.log(t_max / t_min)
  t = t_max * math.exp(t_factor * float(step) / float(steps))
  p = math.exp(-change / t)
  return p

def main():
    arr = []
    for step in range(steps):
        arr.append(calc_p(step, expected_change))
    plt.plot(arr)
    plt.show()

if __name__ == "__main__":
    main()
