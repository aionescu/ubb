from typing import List, Tuple

# Convert to EXCEL
# https://www.onlineocr.net/

# Then convert it to CSV here
# https://www.zamzar.com/convert/xls-to-csv/

csvFile = "Table.csv"
tableName = "Q"
columnTypes = ["int not null", "int not null", "int", "int", "int", "int", "int", "int"]
constraints = ["pk_Q primary key(ID1, ID2)"]

def parseCSV(csv: str) -> List[List[str]]:
  rows = []

  inQuote = False
  crrCell = ""
  cells = []

  for char in csv:
    if char == "\"":
      inQuote = not inQuote
    elif char == ",":
      if inQuote:
        crrCell += char
      else:
        cells.append(crrCell.strip())
        crrCell = ""
    elif char == "\n":
      if inQuote:
        crrCell += " "
      else:
        cells.append(crrCell.strip())
        rows.append(cells)
        crrCell = ""
        cells = []
    else:
      crrCell += char

  cells.append(crrCell.strip())
  rows.append(cells)
  return rows

def emitCreateTable(names: List[str]) -> None:
  print("drop table if exists " + tableName)
  print("create table " + tableName + " (")

  cols = map(lambda p: "  " + p[0] + " " + p[1], zip(names, columnTypes))
  cs = map(lambda c: "  constraint " + c, constraints)

  lines = list(cols)
  lines.extend(cs)

  print(",\n".join(lines))
  print(")\n")

def quote(p: Tuple[str, str]) -> str:
  type, s = p

  if "char" in type or "date" in type or "time" in type:
    return "'" + s + "'"
  else:
    return s

def emitInserts(names: List[str], values: List[List[str]]) -> None:
  print("insert into " + tableName + "(" + ", ".join(names) +  ") values")

  lines = map(lambda v: "  (" + ", ".join(map(quote, zip(columnTypes, v))) + ")", values)
  print(",\n".join(lines))

def emitSQL(csv: List[List[str]]) -> None:

  emitCreateTable(csv[0])
  emitInserts(csv[0], csv[1:])

def main():
  emitSQL(parseCSV(open(csvFile, "r").read()))

if __name__ == "__main__":
  main()
