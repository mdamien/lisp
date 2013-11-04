from collections import namedtuple

class State(object):
	def __init__(self,m,s,b,parent=None):
		self.m,self.s,self.b,self.childs,self.parent = m,s,b,[],parent
	def __str__(self):
		return "(%d %d %d)" % (self.m,self.s,self.b)
	def p(self):
		return ''.join(["M"]*self.m+["C"]*self.s+["B"]*self.b+["|"]+["M"]*(3-self.m)+["C"]*(3-self.s)+["B"]*(1-self.b))
	def __eq__(s,b):
		return s.m == b.m and s.s == b.s and s.b == b.b
	def get_ops(self):
		if self.b == 0:
			return ((0,1),(1,0),(1,1),(2,0),(0,2))
		return ((0,-1),(-1,0),(-1,-1),(-2,0),(0,-2))
	def apply_op(self,op):
		return State(self.m+op[0],self.s+op[1],(self.b+1)%2,self)
	def parents(self):
		parent = self.parent
		while parent != None:
			yield parent
			parent = parent.parent
	def valid(s):
		if not (4 > s.m > -1 and 4 > s.s > -1): return False
		if not s.m == 3 and not s.m == 0 and not s.m == s.s: return False
		return True
		#return 4 > s.m > -1 and 4 > s.s > -1 and (s.m >= s.s if s.b == 0 else s.m <= s.s) #moi
		#return 4 > s.m > -1 and 4 > s.s > -1 and s.s <= s.m and (3-s.s <= 3-s.m or s.m == 0 or s.m == 3) #prof

def generate(start=State(3,3,1)):
	to_explore = [start]
	level = 0
	while len(to_explore) > 0:
		to_explore2 = []
		for state in to_explore:
			if not state == State(0,0,0):
				for op in state.get_ops():
					new_state = state.apply_op(op)
					if new_state.valid() and new_state not in state.parents():
						state.childs.append(new_state)
						to_explore2.append(new_state)
		to_explore = to_explore2
		level += 1
	return start

def print_tree(state,level=0):
	print ' '.join([' ']*level)+state.p()
	for state in state.childs:
		print_tree(state,level+1)

print State(0,0,0,1) == State(0,0,0)
print_tree(generate())