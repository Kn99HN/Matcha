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
            clause = ()
            for val in values:
                if val is not '0':
                    clause = clause + (val,)
            clauses.append(clause)

    print(clauses)
    print(form)

def interp(form, clauses):
                