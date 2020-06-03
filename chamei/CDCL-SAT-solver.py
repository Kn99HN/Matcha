import json
import logging

class Node: 
    def __init__(self, name, value, dl, antecedent, negate):
        self.name = name
        self.value = value
        self.dl = dl
        self.antecedent = antecedent
        self.negate = negate
    
    def __str__(self):
        # return "(" + self.name + "," + self.value + "," + self.dl + "," + self.antecedent + ")"
        return "(Var: " + self.name + ")"

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
    
    # print(formula)
    # print(literals)

def has_unassigned_var(formula):
    for clause in formula:
        for lit in clause:
            if lit.value == None:
                return True
    return False


def select_literals(formula, literals):
    for lit in literals:
        ls = literals.get(lit)
        for idx in ls:
            clause = formula.get(idx)
            for l in clause:
                if l.value == None:
                    return lit

# working on the logic of this
# Handle the case when the value is none or we can delegate that to undefined variables.
def check_unit(formula, literal):
    non_unit = False
    for lit in literal:
        ls = literal.get(lit)
        print("Checking unit is selecting a literal: " + lit)
        for idx in ls:
            print("index is: " + str(idx))
            clause = formula.get(idx)
            unassigned_val = None
            counter = 0
            for node in clause:
                if (node.value == True and node.negate == False) or (node.value == False and node.negate == True):
                    non_unit = True
                if (node.value == False and node.negate == False) or (node.value == True and node.negate == True):
                    counter += 1
                if node.value == None:
                    unassigned_val = node.name
                    if '-' in unassigned_val:
                        unassigned_val = unassigned_val.split('-')[1]
                        
            if non_unit:
                return False, None
                
            if counter == (len(clause) - 1):
                print("Counter is: " + str(counter))
                return True, unassigned_val
    return False, None

# working on the logic of this
def unit_prop(formula, literals):
    contain_unit, unassigned_lit = check_unit(formula, literals)
    while contain_unit:
        ls = literals.get(unassigned_lit)
        print("Unit propagating for literal: " + unassigned_lit)
        for idx in ls:
            clause = formula.get(idx)
            counter = 0
            for c in clause:
                if c.name == unassigned_lit: 
                    c.value = True
                elif c.name == ("-" + unassigned_lit): 
                    c.value = False
                if (c.value == False and c.negate == False) or (c.value == True and c.negate == True):
                    counter += 1
                if counter == len(clause):
                    return "CONFLICT"
                #else invoke graph, handle negated value specially
        contain_unit, unassigned_lit = check_unit(formula, literals)
    return "UNRESOLVED"
    

for f in formula:
    clause = formula.get(f)
    output = ""
    for c in clause:
        if c.name == "-1": c.value = False
        output += "(" + c.name + " ,val: " + str(c.value) + " ,negation: " + str(node.negate) + ")"
    print(str(f) + ": " + output)

print(literals)

unit_prop(formula, literals)

for f in formula:
    clause = formula.get(f)
    output = ""
    for c in clause:
        output += c.name + " ,val: " + str(c.value) + "; "
    print(str(f) + ": " + output)
    