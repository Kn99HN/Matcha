class IGraph:
    def __init__(self):
        graph_dict = {}
        history = set()
        self.graph_dict = graph_dict

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
                    if val.name == lit.name or "-" + val.name == lit.name or val.name == "-" + lit.name:
                        self.graph_dict.get(val).append(node)
    
    def delete(self, dl):
        newG = {}
        for node in self.graph_dict:
            edges = self.graph_dict.get(node)
            for edge in edges:
                if edge.value == None:
                    self.graph_dict.get(node).remove(edge)
            if node.value != None:
                newG[node] = self.graph_dict.get(node)
        self.graph_dict = newG
    
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
    for idx in formula:
        output = "clause " + str(idx) + ": "
        clause = formula.get(idx)
        for lit in clause:
            output += lit.__str__() + " "
        print(output)

levels = {}
file = open("test2.txt", "r")
with file as f: 
    content = [line.rstrip() for line in f]
    formula = {}
    form = ''
    counter = 0
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
            formula[counter] = clause
            counter+=1

# Assigning a value to the first unassigned variable. Set it True no matter what
def decide(formula):
    for idx in formula:
        clause = formula.get(idx)
        for lit in clause:
            if lit.value == None:
                lit.value = False # change back to True later
                return lit

# Given a node, updating all the value in formula
def update_formula(update_val, formula):
    for idx in formula:
        clause = formula.get(idx)
        for lit in clause:
            if lit.name == update_val.name and lit.negate == update_val.negate:
                lit.value = update_val.value
                lit.dl = update_val.dl
            elif "-" + lit.name == update_val.name or lit.name == "-" + update_val.name:
                lit.value = not update_val.value
                lit.dl = update_val.dl

# Checking if there exist an unassigned variable in the formula
def has_unassigned(formula):
    for idx in formula:
        clause = formula.get(idx)
        for lit in clause:
            if lit.value == None:
                return True
    return False

# Check for any unit clause in the formula. If there is, returning the index
def check_unit(formula):
    for idx in formula:
        clause = formula.get(idx)
        counter = 0
        conflict_node = None
        for cl in clause:
            if cl.value == True:
                counter = len(clause) + 1
                break
            if cl.value == False:
                counter += 1
                conflict_node = cl
                cl.antecedent = idx
    
        if counter == len(clause):
            return "CONFLICT", idx, conflict_node
        elif counter == len(clause) - 1:
            return "UNIT", idx, None
    return "UNRESOLVED", -1, None

# Unit propagation. If any of the clause has all but one false value then it can be inferred that the 
# last unassigned value must be True. If so, we add it to the implication graph
def unit_prop(formula, graph, dl):
    is_unit, idx, node = check_unit(formula)
    while is_unit == "UNIT":
        clause = formula[idx]
        unassigned_lit = None
        for lit in clause:
            if lit.value == None:
                lit.value = True
                unassigned_lit = lit
                lit.dl = dl
                lit.antecedent = idx
                if dl in levels:
                    levels.get(dl).append(lit)
                else:
                    levels[dl] = [lit]
                break
        graph.add_vertices(unassigned_lit, clause)
        update_formula(unassigned_lit, formula)
        is_unit, idx, node = check_unit(formula)
        if is_unit == "CONFLICT":
            node.dl = dl
            # graph.add_vertices(node, formula.get(idx))
    return is_unit, node

# Last Literal Assigned at Level d
def lastAssignedLit(d, levels):
    clause = levels.get(d)
    node = clause[len(clause) - 1]
    clause.remove(node)
    return node

# Resolving means: (x1 | x2 | x3) (~x1 | ~x2 | x3) => x3 since x1 and ~x1 cancels each other out
def resolve(first_cl, second_cl, value):
    new_clause = set()
    ill_idx = set()
    combined_clause = first_cl | second_cl
    for (idx, lit) in enumerate(combined_clause):
        unique = False
        for (idx2, lit2) in enumerate(combined_clause):
            if idx2 > idx:
                if lit.name == lit2.name:
                    ill_idx.add(idx2)
                elif "-" + lit.name == lit2.name:
                    ill_idx.add(idx)
                    ill_idx.add(idx2)
                elif lit.name == "-" + lit2.name:
                    ill_idx.add(idx)
                    ill_idx.add(idx2)
    for (idx, lit) in enumerate(combined_clause):
        if idx not in ill_idx:
            new_clause.add(lit)
    return new_clause

# If the clause is unary, then we return 0. Otherwise, we return the second highest level
def assertingLevel(clause):
    if len(clause) - 1 == 1: return 0
    else:
        other_clause = clause.copy()
        max_dl = -1
        max_node = None
        for lit in other_clause:
            if lit.dl > max_dl:
                max_dl = lit.dl
                max_node = lit
        other_clause.remove(max_node)
        second_max_node = -1
        for lit in other_clause:
            second_max_node = max(second_max_node, lit.dl)
        return second_max_node

# Sole Lit At Level d is the only lit at level d
def soleLitAtLevel(s, clause, dl):
    counter = 0
    for lit in clause:
        if lit.dl == dl:
            counter += 1
    return counter == 1
    
def backtrack(formula, graph, dl):
    for idx in formula:
        clause = formula.get(idx)
        for lit in clause:
            if lit.dl > dl:
                lit.value = None
                lit.antecedent = None
                lit.dl = None
            elif lit.dl == dl:
                lit.value = not lit.value
    graph.delete(dl)

def analyze_conflict(formula, graph, conflict_node, levels):
    d = conflict_node.dl
    if d == 0: return -1
    c = formula.get(conflict_node.antecedent)
    while True:
        t = lastAssignedLit(d,levels)
        ante = t.antecedent
        c = resolve(c, formula.get(ante), 0)
        if soleLitAtLevel(None, c, d):
            break
    b = assertingLevel(c)
    return b,c

def reset(formula, conflicting_clause, b):
    for lit in conflicting_clause:
        if lit.dl is not None and lit.dl <= b:
            for idx in formula:
                clause = formula.get(idx)
                for var in clause:
                    if var.name == lit.name:
                        var.value = lit.value
                        var.dl = lit.dl
                    elif "-" + var.name == lit.name or var.name == "-" + lit.name:
                        var.value = not lit.value
                        var.dl = lit.dl
        else:
            lit.value = None
            lit.dl = None
            lit.antecedent = None

def CDCL(formula):
    graph = IGraph()
    level = 0
    if unit_prop(formula, graph, level) == "CONFLICT": return False
    while has_unassigned(formula):
        level += 1
        node = decide(formula)
        update_formula(node, formula)
        graph.add_node(node)
        levels[level] = [node]
        is_unit, conf_node = unit_prop(formula, graph, level)
        while is_unit == "CONFLICT":
            b,c = analyze_conflict(formula, graph, conf_node, levels)
            reset(formula, c)
            formula[len(formula)] = c
            if b < 0: return False
            else:
                backtrack(formula, graph, b)
                level = b
    return True







        