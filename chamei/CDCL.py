class Node: 
    def __init__(self, name, value, dl, antecedent, negate):
        self.name = name
        self.value = value
        self.dl = dl
        self.antecedent = antecedent
        self.negate = negate
    
    def __str__(self):
        return "(" + self.name + "," + str(self.value) + "," + str(self.dl) + "," + str(self.antecedent) + "," + str(self.negate) + ")"

class IGraph:
    def __init__(self):
        graph = {}
        history = set()
        self.graph = graph
        self.history = history
    
    def add_vertex(self, node):
        if not self.contain_vertex(node):
            new_node = None
            if node.negate:
                new_node = Node(node.name, not node.value, node.dl, node.antecedent, False)
            if new_node:
                self.graph[new_node] = set()
            else:
                self.graph[node] = set()
    
    def add_edge(self, node, pre_antecedent):
        self.add_vertex(node)
        self.history = []
        for lit in pre_antecedent:
            history_vertex = None
            for vertex in self.graph:
                if vertex.name == lit.name:
                    history_vertex = vertex
                    if node.negate:
                        new_node = Node(node.name, not node.value, node.dl, node.antecedent, False)
                        self.graph.get(vertex).add(new_node)
                    else:
                        self.graph.get(vertex).add(new_node)
            if history_vertex:
                self.history.append(history_vertex)

    def remove_edge(self, conflict_node):
        for node in self.history:
            new_edges = set()
            for edge in self.graph.get(node):
                if edge.name != conflict_node.name:
                    new_edges.add(edge)
            self.graph[node] = new_edges
        
    # def changing_value(self, node):
    #     for vertex in self.graph:
    #         if vertex.name == node.name:
    #             vertex.value = node.value
    #             vertex.negate = node.negate
        
    def contain_vertex(self, node):
        for vertex in self.graph:
            if vertex.name == node.name:
                return True
        return False

    def __str__(self):
        output = ""
        for vertex in self.graph:
            output += vertex.__str__() + " :{ "
            edges = self.graph.get(vertex)
            for edge in edges:
                output += edge.__str__() + " , "
            
            output += " } \n"
        return output


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
                        node = Node(lit, None, None, None, True)
                    else:
                        lit = val
                        node = Node(lit, None, None, None, False)

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

        
def print_formula(formula):
    for f in formula:
        output = "clause " + str(f) + ": "
        clause = formula.get(f)
        for cl in clause:
            output += cl.__str__() + " "
        print(output)
    
def decide(formula, literals, dl):
    chosen_lit = None
    for idx in formula:
        clause = formula.get(idx)
        for cl in clause:
            if cl.value == None:
                cl.value = False
                cl.dl = dl
                chosen_lit = cl
                break
        break

    update_formula(formula, literals, chosen_lit)
    return chosen_lit

def update_formula(formula, literals, chosen_lit):
    if not chosen_lit:
        return False
    clauses = literals.get(chosen_lit.name)
    for idx in clauses:
        clause = formula.get(idx)
        for cl in clause:
            if chosen_lit.name == cl.name and chosen_lit.negate == cl.negate:
                cl.value = chosen_lit.value
                cl.dl = chosen_lit.dl
                cl.antecedent = chosen_lit.antecedent
            elif chosen_lit.name == cl.name:
                cl.value = not chosen_lit.value
                cl.dl = chosen_lit.dl
                cl.antecedent = chosen_lit.antecedent

def check_unit(formula):
    for idx in formula:
        clause = formula.get(idx)
        counter = 0
        unassigned_var = None
        for cl in clause:
            if cl.value == False:
                counter += 1
            elif cl.value == None:
                unassigned_var = cl
        
        if counter == (len(clause) - 1):
            return "UNIT", unassigned_var, idx
        elif counter == len(clause):
            return "CONFLICT", None, None
    return "UNRESOLVED", None, None

# Adding graph implication
def unit_propagate(formula, literals, graph, dl):
    is_unit, unassigned_var, idx = check_unit(formula)
    while is_unit == "UNIT":
        print("Unit propagating for node: " + str(unassigned_var))
        if unassigned_var.negate == True:
            unassigned_var.value = False
            unassigned_var.antecedent = idx
            unassigned_var.dl = dl
        else:
            unassigned_var.value = True
            unassigned_var.antecedent = idx
            unassigned_var.dl = dl
        update_formula(formula, literals, unassigned_var)
        clause = formula.get(idx)
        false_clause = set()
        for cl in clause:
            if cl.name != unassigned_var.name:
                false_clause.add(cl)
        graph.add_edge(unassigned_var, false_clause)
        is_unit, unassigned_var, idx = check_unit(formula)

    return is_unit, unassigned_var

def has_unassigned_var(formula, literals):
    for idx in formula:
        clause = formula.get(idx)
        for cl in clause:
            if cl.value == None:
                return True
    return False
                

#     return True

def analyze_conflict(formula, literals, graph, dl):
    conflict_clause = graph.history
    new_conflict_clause = set()
    same_lvl_variable = None
    for cl in conflict_clause:
        if cl.dl == dl:
            conflict_variable = cl
    max_dl = -1
    for lit in conflict_clause:
        new_node = Node(lit.name, not lit.value, lit.dl, lit.antecedent, not lit.negate)
        new_conflict_clause.add(new_node)
        if conflict_variable:
            if cl.name != conflict_variable.name:
                max_dl = max(max_dl, cl.dl)
                conflict_variable = cl
        else:
            max_dl = max(max_dl, cl.dl)
            conflict_variable = cl

    return max_dl, new_conflict_clause, conflict_variable

def backtrack(formula, literals, graph, conflict_node, back_level):
    for idx in formula:
        clause = formula.get(idx)
        for cl in clause:
            if cl.dl == back_level:
                cl.value = not cl.value
                cl.negate = not cl.negate
    
    graph.remove(conflict_node)


def CDCL(formula, literals):
    graph = IGraph()
    dl = 0
    res, unassigned = unit_propagate(formula, literals, graph, dl)
    if res == "CONFLICT": return False
    print("Formula is: ")
    print_formula(formula)
    while has_unassigned_var(formula, literals):
        dl += 1
        node = decide(formula, literals, dl)
        print("Decided node: " + str(node) + "\n")
        if node:
            graph.add_vertex(node)
            res, unassigned = unit_propagate(formula, literals, graph, dl)
            while res == "CONFLICT":
                max_dl, new_clause, conflict_var = analyze_conflict(formula, literals, graph, dl)
                formula[len(formula) + 1] = new_clause
                res, unassigned = unit_propagate(formula, literals, graph, dl)
                print("After analyzing conflict: " + str(max_dl) + " , " + str(conflict_var))
                print("--------------------")
                print("Graph layout:")
                print(graph)
                print("Current formula is: ")
                print_formula(formula)
                print("--------------------")
                max_dl = -1
                if max_dl < 0:
                    return False

    return True


# node = decide(formula, literals, 0)
# print("Decided node: " + str(node))
# graph = IGraph()
# graph.add_vertex(node)
# print(unit_propagate(formula, literals, graph, 1) + "\n")
# print("Graph layout:")
# print(graph.__str__())
# print("-----------")
# print(analyze_conflict(formula, literals, graph, 1))
# print("Graph layout:")
# print(graph.__str__())
# print("-----------")
# print_formula(formula)

CDCL(formula, literals)