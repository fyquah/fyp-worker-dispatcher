import math

import matplotlib.pyplot as plt


t_max = 0.04
t_min = 0.0005
steps = 300
expected_change = 0.01

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
