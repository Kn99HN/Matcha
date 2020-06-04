class IGraph:
    def __init__(self):
        graph = {}
        history = []
        self.graph = graph
        self.history = history

            
    def get_edge(self, node):
        for key in self.graph:
            if key.name == node.name:
                return key
            elif "-" + key.name == node.name:
                return key
        return None
        
    def add(self, node):
        contain = False
        for key in self.graph:
            if key.name == node.name:
                contain = True
            elif "-" + key.name == node.name:
                contain = True
        
        if not contain:
            if "-" in node.name:
                new_node = Node(node.name.split("-")[1], not node.value, node.dl, node.antecedent, False)
                self.graph[new_node] = []
            else:
                self.graph[node] = []
            
    def add_edge(self, node, clause):
        for cl in clause:
            self.add(node)
            edge = self.get_edge(cl)
            if edge:
                self.graph.get(edge).append(cl)
            self.history.append(cl)
        self.history.append(node)

    def __str__(self):
        output = ""
        for vertex in self.graph:
            output += vertex.__str__() + " :{ "
            edges = self.graph.get(vertex)
            for edge in edges:
                output += edge.__str__() + " , "
            
            output += " } \n"
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
    
# Change to None later
file = open("test.txt", "r")
with file as f: 
    content = [line.rstrip() for line in f]
    formula = {}
    literals = {}
    form = ''
    counter = 1
    for command in content:
        values = command.split(' ')
        if 'p' in values:
            form = values[1]
        if 'c' not in values and 'p' not in values:
            for val in values:
                if val is not '0':
                    if '-' in val:
                        lit = val.split('-')[1]
                        node = Node(val, None, None, None, True)
                    else:
                        lit = val
                        node = Node(val, None, None, None, False)

                    if counter in formula:
                        formula.get(counter).add(node)
                    else:
                        formula[counter] = {node}
                    
                    if lit in literals:
                        ls = literals.get(lit)
                        if counter not in ls:
                            literals.get(lit).append(counter)
                    else:
                        literals[lit] = [counter]
            counter += 1

def has_unassigned_var(formula):
    for cl in formula:
        clause = formula.get(cl)
        for lit in clause:
            if lit.value == None:
                return True
    return False


def decide(formula, literals):
    for lit in literals:
        ls = literals.get(lit)
        for idx in ls:
            clause = formula.get(idx)
            for node in clause:
                if node.value == None:
                    print("Selecting literal: " + node.name)
                    node.value = False
                    return node

def back_track_level(formula, literals, conflic_clause_idx, conflict_level, graph):
    conflict_clause = formula.get(conflic_clause_idx)
    new_clause = {}
    p = None
    for lit in conflict_clause:
        if lit.level == conflict_level: 
            p = lit
        else:
            new_clause.add(lit)

    max_lv = -1
    for cl in new_clause:
        max_lv = max(max_lv, cl.dl)

    return max_lv


# for f in formula:
#     clause = formula.get(f)
#     output = ""
#     for c in clause:
#         if c.name == "-1": c.value = True
#         if c.name == "-3": c.value = True


def print_formula(formula):
    for f in formula:
        output = "clause " + str(f) + ": "
        clause = formula.get(f)
        for cl in clause:
            output += cl.__str__() + " "
        print(output)

print("There are unassigned variable: " + str(has_unassigned_var(formula)))

'''
    Checking if an unit clause exists in the formula.
    If there is one return True and the unassigned value
    name. Otherwise, False and None
'''
def check_unit(formula, literal):
    for lit in literal:
        cl = literal.get(lit)
        false_counter = 0
        unassigned_val = None
        for idx in cl:
            clause = formula.get(idx)
            for variable in clause:
                if(variable.value == True and variable.negate == False) or (variable.value == False and variable.negate == True):
                    return "UNRESOLVED", None
                if (variable.value == False and variable.negate == False):
                    false_counter += 1
                elif (variable.value == None):
                    if '-' in variable.name:
                        unassigned_val = variable.name.split('-')[1]
                    else:
                        unassigned_val = variable.name
            if false_counter == len(clause):
                return "CONFLICT", None
            if false_counter == (len(clause) - 1):
                return True, unassigned_val
    return "UNRESOLVED", None
                
print("Formula containing unit clause: " + str(check_unit(formula, literals)))

def unit_propagate(formula, literals, graph, dl):
    contain_unit, unassigned_lit = check_unit(formula, literals)
    while contain_unit == True:
        print("Before propagating:")
        print_formula(formula)
        ls = literals.get(unassigned_lit)
        print("Unit propagating for literal: " + str(unassigned_lit))
        for idx in ls:
            clause = formula.get(idx)
            counter = 0
            new_clause = set()
            for c in clause:
                if c.name != unassigned_lit:
                    new_clause.add(c)
                if c.name == unassigned_lit: 
                    c.value = True
                    node = Node(unassigned_lit, True, dl,idx, False)
                elif c.name == ("-" + unassigned_lit): 
                    c.value = False
                    node = Node(unassigned_lit, True, dl,idx, False)
                if (c.value == False and c.negate == False) or (c.value == True and c.negate == True):
                    counter += 1
            print("Adding node: " + node.__str__())
            graph.add_edge(node, new_clause)
            if counter == len(clause):
                return "CONFLICT"
        print("After propagating: ")
        print_formula(formula)
        print("----------------------------")
        contain_unit, unassigned_lit = check_unit(formula, literals)
    return contain_unit


def CDCL(formula, literals):
    graph = IGraph()
    if unit_propagate(formula, literals, graph, -1) == "CONFLICT": return False
    level = 0
    while has_unassigned_var(formula):
        level += 1
        node = decide(formula, literals)
        for idx in formula:
            clause = formula.get(idx)
            for cl in clause:
                if cl.name == node.name:
                    cl.value = node.value
                elif "-" + cl.name == node.name:
                    cl.value = not node.value
                elif cl.name == "-" + node.name:
                    cl.value = not node.value
        graph.add(node)
        res = unit_propagate(formula, literals, graph, level)
        # if res == "CONFLICT":
        #     graph.remove()
    print(graph.__str__())
    return True

if CDCL(formula, literals):
    print("Formula is SAT")
else:
    print("Formula is UNSAT")

print("After running CDCL: ")
print_formula(formula)