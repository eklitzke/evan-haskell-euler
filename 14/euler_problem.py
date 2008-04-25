import sys
sys.setrecursionlimit(100000)

def collatz(n):
	if n % 2 == 0:
		return n / 2
	else:
		return 3 * n + 1

collatz_len = {1: 1}
def cl(n):
	if n in collatz_len:
		return collatz_len[n]
	answer = 1 + cl(collatz(n))
	collatz_len[n] = answer
	return answer

d = dict((k, cl(k)) for k in range(1, 1000000))
max_v = max(d.values())
for k, v in d.iteritems():
	if v == max_v:
		print k
