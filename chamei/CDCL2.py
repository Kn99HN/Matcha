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
                lit.value = True
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
        for cl in clause:
            if cl.value == True:
                break
            if cl.value == False:
                counter += 1
        
        if counter == len(clause):
            return "CONFLICT", idx
        elif counter == len(clause) - 1:
            return "UNIT", idx
    return "UNRESOLVED", -1

    
# print("Before:")
# print_formula(formula)
# node = decide(formula)
# update_formula(node, formula)
# print("After 1 decision:")
# print_formula(formula)
# node = decide(formula)
# update_formula(node, formula)
# print("After 2 decision:")
# print_formula(formula)
# node = decide(formula)
# update_formula(node, formula)
# print("After 3 decision:")
# print_formula(formula)
# print(check_unit(formula))