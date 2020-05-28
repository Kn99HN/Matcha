def select_literals(clauses):
    for clause in clauses:
        for literal in clause:
            return literal[0]
        

# finding if any clause has all but one values are false 
# def unit_prop(clauses) 
