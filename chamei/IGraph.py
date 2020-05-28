class IGraph:
    def IGraph(self):
        nodes = list()

    def add(self,node):
        literal = {node: []}
        self.nodes.append(literal)
    
    
    

class Node: 
    def Node(self, value, dl, antecedent):
        self.value = value
        self.dl = dl
        self.antecedent = antecedent