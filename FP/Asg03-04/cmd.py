from re import compile
from expense import *
from impl import *

re_add = compile(r"add\s+(\d+)\s+([a-z]+)")
re_insert = compile(r"insert\s+(\d+)\s+(\d+)\s+([a-z]+)")
re_remove = compile(r"remove\s+(?:([a-z]+)|(?:(\d+)(?:\s+to\s+(\d+))?))")
re_list = compile(r"list(?:\s+([a-z]+)(?:\s+([<=>])\s+(\d+))?)?")
re_sum = compile(r"sum\s+([a-z]+)")
re_max = compile(r"max day")
re_sort = compile(r"sort\s+(\d+|[a-z]+)")
re_filter = compile(r"filter\s+([a-z]+)(?:\s+([<=>])\s+(\d+))?")
re_exit = compile(r"exit")
re_undo = compile(r"undo")
re_redo = compile(r"redo")

def to_op(op):
  if op == '<':
    return lambda a, b: a < b
  elif op == '=':
    return lambda a, b: a == b
  elif op == '>':
    return lambda a, b: a > b
  else:
    raise Exception("Unreachable")

def handle_input(exps, undo, redo, line):
  def modify(exps, undo, redo, new_exps):
    undo.append(exps.copy())
    exps.clear()
    exps.extend(new_exps)
    redo.clear()

  if re_exit.match(line):
    exit()

  if re_undo.match(line):
    if not undo:
      print("Nothing to undo.")
    else:
      redo.append(exps.copy())
      exps.clear()
      exps.extend(undo.pop())

    return
    
  if re_redo.match(line):
    if not redo:
      print("Nothing to redo.")
    else:
      undo.append(exps.copy())
      exps.clear()
      exps.extend(redo.pop())

    return

  r = re_add.match(line)
  if r:
    amount, category = r.groups()
    modify(exps, undo, redo, insert(exps, today.day, int(amount), category))
    return

  r = re_insert.match(line)
  if r:
    day, amount, category = r.groups()
    modify(exps, undo, redo, insert(exps, int(day), int(amount), category))
    return

  r = re_remove.match(line)
  if r:
    category, start, end = r.groups()
    if category:
      modify(exps, undo, redo, remove_category(exps, category))
    else:
      if not end:
        end = start
        
      modify(exps, undo, redo, remove_range(exps, int(start), int(end)))
    
    return

  r = re_list.match(line)
  if r:
    cat, op, val = r.groups()
    if not cat:
      l = list_all(exps)
    elif not op:
      l = list_category(exps, cat)
    else:
      l = list_predicate(exps, cat, to_op(op), int(val))

    print(l)
    return

  r = re_sum.match(line)
  if r:
    cat, = r.groups()
    print(sum_category(exps, cat))
    return
    
  r = re_max.match(line)
  if r:
    print(max_day(exps))
    return

  r = re_sort.match(line)
  if r:
    v, = r.groups()
    try:
      v = int(v)
      print(sort_day(exps, v))
    except:
      print(sort_category(exps, v))

    return

  r = re_filter.match(line)
  if r:
    cat, op, val = r.groups()
    if not op:
      modify(exps, undo, redo, filter_category(exps, cat))
    else:
      modify(exps, undo, redo, filter_predicate(exps, cat, to_op(op), int(val)))

    return

  print("Command not recognized.")