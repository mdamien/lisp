from graph import *
from pygraphviz import *

def add_it(G,node):
	G.add_node(id(node),label=str(node.p())) #enléve .p() si tu veux la notation (x y b))
	if node.parent:G.add_edge(id(node),id(node.parent))
	for child in node.childs:
		add_it(G,child)

G = AGraph()
tree = generate()
add_it(G,tree)
G.write("graph.dot")
G.layout(prog='dot')
G.draw('graph.png')