# @ToDo:
# - Figuring out how to identify the conflict node
# - Adding pieces together for analyze conflict
# - Working on backtrack
# - Fix all for loop with formula
# - How to reconstruct the graph

# def test(formula, name, dl):
#     for idx in formula:
#         clause = formula.get(idx)
#         for lit in clause:
#             if lit.name == name:
#                 lit.value = False
#                 lit.dl = dl
#                 node = lit
#                 return lit


# dl = 1
# graph = IGraph()
# node = test(formula, "7", dl)
# update_formula(node, formula)
# graph.add_node(node)
# levels[dl] = [node]
# # Level 2
# dl += 1
# node = test(formula, "8", dl)
# update_formula(node, formula)
# graph.add_node(node)
# levels[dl] = [node]
# dl += 1
# node = test(formula, "9", dl)
# update_formula(node, formula)
# graph.add_node(node)
# levels[dl] = [node]
# dl += 1
# node = test(formula, "1", dl)
# update_formula(node, formula)
# graph.add_node(node)
# levels[dl] = [node]

# is_unit, node = unit_prop(formula, graph, dl)
# print("After Unit Propagation:")
# print_formula(formula)
# print(graph.__str__())

# print("Conflicting Node:")
# print(node.__str__() + "\n")

# def print_level(levels):
#     output = ""
#     for idx in levels:
#         output += "Level " + str(idx) + ": "
#         variables = levels.get(idx)
#         for var in variables:
#             output += var.__str__()
#         output += "\n"
#     print(output)
    
# print_level(levels)

# b, c = analyze_conflict(formula, graph, node, levels)
# output = "Conflicting Clause: \n"
# for lit in c:
#     output += lit.__str__()
# print(output)

# backtrack(formula, graph, b)
# reset(formula, c, b)
# formula[len(formula)] = c
# dl = b

# print()
# node = decide(formula)
# dl += 1
# levels[dl] = [node]
# graph.add_node(node)
# update_formula(node, formula)

# dl += 1
# levels[dl] = [node]
# graph.add_node(node)
# update_formula(node, formula)

# print("New Formula")
# print_formula(formula)
# print(graph.__str__())