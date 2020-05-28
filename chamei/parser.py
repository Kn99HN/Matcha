########################################
#   Handle interpretation
########################################
def dpll(clauses, assignments = {}):
    if len(clauses) == 0: return True, assignments
    elif any([len(c) == 0 for c in clauses]):
        return False, None

    l = select_literal(clauses)
    new_clauses = []
    for c in clauses:
        if (l, True) not in c:
            new_clauses.append(c)
    for c in new_clauses:
        new_clauses.remove(c)
        new_clauses.append(c.difference({(l, False)}))
    assignments[l] = True
    sat, assignments = dpll(new_clauses, assignments)
    if sat:
        return sat, assignments
    
    assignments = {}
    new_clauses = [c for c in clauses if (l, False) not in c]
    for c in new_clauses:
        new_clauses.remove(c)
        new_clauses.append(c.difference({(l, True)}))
    assignments[l] = False
    sat, assignments = dpll(new_clauses, assignments)
    if sat:
        return sat, assignments
    return False, None


########################################
#    Helper Functions
########################################
def pretty_print(clauses):
    conjunct = ""
    for (i, c) in enumerate(clauses):
        for (i, literal) in enumerate(c):
            if literal[1] == False:
                conjunct += "-" + literal[0]
            else:
                conjunct += literal[0]
            
            if i != len(c) - 1:
                    conjunct += " || "
        if i != len(clauses):
            conjunct += " && "
    return conjunct

def select_literal(clauses):
    for c in clauses:
        for literal in c:
            return literal[0]

########################################
#    Opening Files Functions
########################################
file = open("test.txt", "r")
with file as f: 
    content = [line.rstrip() for line in f]
    clauses = []
    form = ''
    for command in content:
        values = command.split(' ')
        if 'p' in values:
            form = values[1]
        if 'c' not in values and 'p' not in values:
            clause = set()
            for val in values:
                if val is not '0':
                    value = int(val)
                    if value < 0:
                        literal = ('' + str(int(val) * -1), False)
                    else:
                        literal = (val, True)
                    clause.add(literal)
            clauses.append(clause)
    print("The clauses are: " + pretty_print(clauses))
    print(dpll(clauses))