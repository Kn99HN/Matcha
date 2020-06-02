class IGraph:
    def IGraph(self):
        graph_dict = {}
        self.graph_dict = graph_dict
    
    def add_vertex(self, node):
        if node not in self.graph_dict:
            graph_dict[node] = []
    
    def add_edges(self, nil_literal, false_literals, edges):
        bool_val = False
        '''
        It would be best if the node in formula has the same data structure as the one in the graph.
        It would eliminate lots of complexity dealing with dictionary. 
        '''
        for literal in false_literals:
            for vertex in self.graph_dict:
                if literal[0] == vertex.name and (not literal[1]) == vertex.value:
                    bool_val = True
                else:
                    bool_val = False
        if bool_val:
            for literal in false_literals:
                if nil_literal not in self.graph_dict:
                    self.graph_dict[nil_literal] = []
                else:
                    self.graph_dict.get(literal).append(nil_literal)
    

class Node: 
    def Node(self, value, dl, antecedent):
        self.name = name
        self.value = value
        self.dl = dl
        self.antecedent = antecedent