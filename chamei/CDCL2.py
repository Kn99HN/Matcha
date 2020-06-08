class IGraph:
    def __init__(self):
        graph_dict = {}
        history = set()
        self.graph_dict = graph_dict
        self.history = history

    def exist(self, node):
        for curr_node in self.graph_dict:
            if curr_node.name == node.name or "-" + curr_node.name == node.name or curr_node.name == "-" + node.name:
                return True
        return False

    def add_node(self, node):
        if not self.exist(node):
            self.graph_dict[node] = []
    
    def add_vertices(self, node, clause):
        if not self.exist(node):
            self.add_node(node)
        for lit in clause:
            if lit.name != node.name:
                for val in self.graph_dict:
                    if val.name == lit.name:
                        self.graph_dict.get(val).append(node)
    
    def __str__(self):
        output = ""
        for vertex in self.graph_dict:
            output += vertex.__str__() + " :{ "
            edges = self.graph_dict.get(vertex)
            for edge in edges:
                output += edge.__str__() + ", "
            
            output += "} \n"
        return output
        

class Node: 
    def __init__(self, name, value, dl, antecedent, negate):
        self.name = name
        self.value = value
        self.dl = dl
        self.antecedent = antecedent
        self.negate = negate
    
    def __str__(self):
        return "(" + self.name + "," + str(self.value) + "," + str(self.dl) + "," + str(self.antecedent) + "," + str(self.negate) + ")"

# Pretty printer for formula
def print_formula(formula):
    for (idx, clause) in enumerate(formula):
        output = "clause " + str(idx) + ": "
        for lit in clause:
            output += lit.__str__() + " "
        print(output)

file = open("test.txt", "r")
with file as f: 
    content = [line.rstrip() for line in f]
    formula = []
    form = ''
    counter = 1
    for command in content:
        values = command.split(' ')
        if 'p' in values:
            form = values[1]
        if 'c' not in values and 'p' not in values:
            clause = set()
            for val in values:
                if val is not '0':
                    if "-" in val:
                        node = Node(val, None, None, None, True)
                    else:
                        node = Node(val, None, None, None, False)
                    clause.add(node)
            formula.append(clause)

# Assigning a value to the first unassigned variable. Set it True no matter what
def decide(formula):
    for clause in formula:
        for lit in clause:
            if lit.value == None:
                lit.value = False # change back to True later
                return lit

# Given a node, updating all the value in formula
def update_formula(udpate_val, formula):
    for clause in formula:
        for lit in clause:
            if lit.name == udpate_val.name and lit.negate == udpate_val.negate:
                lit.value = udpate_val.value
            elif "-" + lit.name == udpate_val.name or lit.name == "-" + udpate_val.name:
                lit.value = not udpate_val.value

# Checking if there exist an unassigned variable in the formula
def has_unassigned(formula):
    for clause in formula:
        for lit in clause:
            if lit.value == None:
                return True
    return False

# Check for any unit clause in the formula. If there is, returning the index
def check_unit(formula):
    for (idx, clause) in enumerate(formula):
        counter = 0
        node = None
        for cl in clause:
            if cl.value == True:
                counter = len(clause) + 1
                break
            if cl.value == False:
                counter += 1
    
        if counter == len(clause):
            return "CONFLICT", idx
        elif counter == len(clause) - 1:
            return "UNIT", idx
    return "UNRESOLVED", -1

# Unit propagation. If any of the clause has all but one false value then it can be inferred that the 
# last unassigned value must be True. If so, we add it to the implication graph
def unit_prop(formula, graph, dl):
    is_unit, idx = check_unit(formula)
    while is_unit == "UNIT":
        clause = formula[idx]
        unassigned_lit = None
        for lit in clause:
            if lit.value == None:
                lit.value = True
                unassigned_lit = lit
                lit.dl = dl
                lit.antecedent = idx
        graph.add_vertices(lit, clause)
        update_formula(unassigned_lit, formula)
        is_unit, idx = check_unit(formula)
    return is_unit

# Last Literal Assigned at Level d
def lastAssignedAtLevel(d, levels):
    clause = levels.get(d)
    return clause.get(len(clause) - 1)

# Resolving means: (x1 | x2 | x3) (~x1 | ~x2 | x3) => x3 since x1 and ~x1 cancels each other out
def resolve(first_cl, second_cl, value):
    new_clause = []
    # for first_lit in first_cl:
    #     for second_lit in second_cl:
    #         if first_lit.name == second_lit.name:
    #             new_clause.append(first_lit)
    #         if first_lit.name != second_lit.name:
    #             new_clause.append(first)

# Sole Lit At Level d is the only lit at level d
def soleLitAtLevel(s, clause, dl):
    counter = 0
    for lit in clause:
        if lit.dl == dl:
            counter += 1
    return counter == 1
    

def CDCL(formula):
    graph = IGraph()
    level = 0
    if unit_prop(formula, graph, level) == "CONFLICT": return False
    while has_unassigned(formula):
        level += 1
        node = decide(formula)
        update_formula(node, formula)
        graph.add_node(node)
    return True
    

print(CDCL(formula))



        