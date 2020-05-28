
def select_literals(clauses):
    for clause in clauses:
        for literal in clause:
            return literal[0]
        

# finding if any clause has all but one values are false 
# def unit_prop(clauses) 

'''
def CDCL(F, G):
    if unit_prop(F,G) == CONFLICT: return UNSAT
    dl = 0
    while ! allVariablesAssigned(F, G):
        (x,v) = decide(F,G) #decide stage
        dl += 1
        G = G U {(x,v)}
        if unit_prop(F,G) == CONFLICT:
            B = conflict_analysis(F,G)
            if B < 0: return UNSAT
            else backtrack(F,G, B):
                dl = B
    return true
'''

def all_variables_assigned(formulas, graph):
    for clause in formulas:
        for lit in clause:
            if lit[1] == "undefined":
                return True
    return False

def decide(formulas, graph):
    for clause in formulas:
        for lit in clause:
            if lit[1] == "undefined":
                return lit[0], True

def unit_prop(formulas, variables, graph):
    for variable in variables:
        ls = variables[variable]
        for i in ls:
            clause = formulas.get(i)
            counter = 0
            variable = ""
            for lit in clause:
                if lit[1] == False:
                    counter += 1
                elif lit[1] == True:
                    return "SAT"
            if counter == len(clause):
                return "CONFLICT"
            elif counter == len(clause) - 1:
                return "UNIT"
    return "UNRESOLVED"

def Node(var, value, dl):
    return (var, value,dl)

# Change to None later
file = open("test.txt", "r")
with file as f: 
    content = [line.rstrip() for line in f]
    formula = {}
    variables = {}
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
                    node = Node(val, "undefined", -1)
                    clause.add(node)
            formula[counter] = clause
            counter += 1
    for clause in formula:
        ls = formula.get(clause)
        for lit in ls:
            value = lit[0]
            if '-' in value:
                value = value.split('-')[1]
            if variables.get(value):
                variables.get(value).append(clause)
            else:
                variables[value] = [clause]

print(unit_prop(formula, variables, ""))