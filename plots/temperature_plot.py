import math

import matplotlib.pyplot as plt


t_max = 0.100
t_min = 0.003
steps = 1000
expected_change = 0.01

def main():
    arr = []
    for step in range(steps):
        t_factor = -math.log(t_max / t_min)
        t = t_max * math.exp(t_factor * float(step) / float(steps))
        p = math.exp(-0.02 / t)
        arr.append(p)
    plt.plot(arr)
    plt.show()

main()
