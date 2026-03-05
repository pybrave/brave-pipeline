#!/ssd1/wy/conda/bin/python
import matplotlib.pyplot as plt
import sys

out_file = sys.argv[1]

plt.figure(figsize=(6, 4))
plt.plot([1, 2, 3], [4, 5, 6], label='line')
plt.title("Simple Plot")
plt.xlabel("X")
plt.ylabel("Y")
plt.legend()
plt.tight_layout()
plt.savefig(out_file)
