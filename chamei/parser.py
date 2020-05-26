
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
    print(conjunct)

def interp(clauses, assignments = {}):
    if len(clauses) == 0: return True, assignments
    elif any([len(c) == 0 for c in clauses]):
        return False, None

    l = select_literal(clauses)
    new_clauses = []
    for c in clauses:
        if (l, True) not in c:
            new_clauses.append(c)
    new_cnf = [c for c in clauses if (l, True) not in c]
    new_cnf = [c.difference({(l, False)}) for c in new_cnf]
    for c in new_clauses:
        new_clauses.remove(c)
        new_clauses.append(c.difference({(l, False)}))
    assignments.add((l, True))
    sat = interp(new_clauses, assignments)

    # Handle the cases when it is not satisfiable
    if sat is not None:
        print(sat[0])
        

def select_literal(clauses):
    for c in clauses:
        for literal in c:
            return literal[0]



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
                    # literal = (int(val) * (-1), int(val) > 0)
                    value = int(val)
                    if value < 0:
                        literal = ('' + str(int(val) * -1), False)
                    else:
                        literal = (val, True)
                    clause.add(literal)
            clauses.append(clause)
    interp(clauses, set())

                