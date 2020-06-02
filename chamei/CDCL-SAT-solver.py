
# def select_literals(clauses):
#     for clause in clauses:
#         for literal in clause:
#             return literal[0]
        

# # finding if any clause has all but one values are false 
# # def unit_prop(clauses) 

# '''
# def CDCL(F, G):
#     if unit_prop(F,G) == CONFLICT: return UNSAT
#     dl = 0
#     while ! allVariablesAssigned(F, G):
#         (x,v) = decide(F,G) #decide stage
#         dl += 1
#         G = G U {(x,v)}
#         if unit_prop(F,G) == CONFLICT:
#             B = conflict_analysis(F,G)
#             if B < 0: return UNSAT
#             else backtrack(F,G, B):
#                 dl = B
#     return true
# '''

# # Change this to be more pythonic
# def all_variables_assigned(formulas, graph):
#     for i in formulas:
#         ls = formulas.get(i)
#         for clause in ls:
#             for lit in clause:
#                 if lit[1] == "undefined": 
#                     return True
#     return False

# def decide(formulas, graph):
#     for clause in formulas:
#         for lit in clause:
#             if lit[1] == "undefined":
#                 return lit[0], True

# def unit_prop(formulas, variables, graph):
#     for variable in variables:
#         ls = variables[variable]
#         for i in ls:
#             clause = formulas.get(i)
#             counter = 0
#             variable = ""
#             for lit in clause:
#                 if lit[1] == False:
#                     counter += 1
#                 elif lit[1] == True:
#                     return "SAT"
#             if counter == len(clause):
#                 return "CONFLICT"
#             elif counter == len(clause) - 1:
#                 return "UNIT"
#     return "UNRESOLVED"

# def Node(var, value, dl):
#     return (var, value,dl)


class Node: 
    def __init__(self, name, value, dl, antecedent, negate):
        self.name = name
        self.value = value
        self.dl = dl
        self.antecedent = antecedent
        self.negate = negate
    
    def __str__(self):
        return "(" + self.name + "," + self.value + "," + self.dl + "," + self.antecedent + ")"

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
                    else:
                        lit = val

                    if counter in formula:
                        formula.get(counter).add(Node(val, None, None, None, None))
                    else:
                        formula[counter] = {Node(val, None, None, None, None)}
                    
                    if lit in literals:
                        ls = literals.get(lit)
                        if counter not in ls:
                            literals.get(lit).append(counter)
                    else:
                        literals[lit] = [counter]
            counter += 1
    
    print(formula)
    print(literals)

def has_unassigned_var(formula):
    for clause in formula:
        for lit in clause:
            if lit.value == None:
                return True
    return False


