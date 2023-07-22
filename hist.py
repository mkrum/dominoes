
import sys
import matplotlib.pyplot as plt

data = list(map(int, sys.stdin.readlines()))
plt.hist(data, bins=100)
plt.show()
