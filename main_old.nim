import system/io
import std/strutils
import std/tables


type
  INSTRUCT_TYPE = enum
    NULL
    FUNC_DEF
    FUNC_CALL
    VAR_DECLARATION
    ASSIGNMENT
    RETURN

type
  Line_of_code = object
    id: int
    line_no: int
    line_content: string
    char_count: int
    isIndendet: bool
    indentation_count: int
    isEmpty: bool
    block_name: string

type
  Instruction = object
    instruction_type: INSTRUCT_TYPE
    line: Line_of_code
    isSideEffect: bool
    isLazy: bool
    instruction_name: string

type
  TYPE_KINDS = enum
    INTEGER
    STRING
    BOOLEAN
    FUNCTION_OBJ
    CLASS_OBJ

type
  StrValue = object
    value: string

type
  Return_obj = object
    return_type: TYPE_KINDS
    return_value: StrValue
    return_address: string

type
  TOKENS = enum
    SHORT_FUNC
    FUNC_NAME
    LEFT_PAREN
    RIGHT_PAREN
    ARGUMENT_NAME
    TYPE_SEPERATOR
    ARGUMENT_TYPE
    LEFT_SQUARE_BRACKET
    RIGHT_SQUARE_BRACKET
    TYPE_SEP
    TYPE_DEF
    EQUAL_SIGN
    ECHO_FUNC
    DOUBLE_QUOTES
    STRING_LITERAL
    RETURN_STATEMENT
    NUMBER
    CALL

type
  Variable = object
    variable_name: string
    variable_type: string
    variable_default_value: StrValue

type
  Argument = Variable

# type
#   Argument = object
#     argument_name: string
#     argument_type: string
#     argument_default_value: StrValue

type
  function_definition = object
    func_symbol: string
    func_name: string
    open_paren: string
    arguments: seq[Argument]
    close_paren: string
    type_seperator: string
    type_declaration: string
    equal_sign: string

type
  function_body = object
    lines: seq[Instruction]
    returns: Return_obj

type
  function_object = object
    definition: function_definition
    body: function_body

type 
  OPERATION_KIND = enum
    PRINT
    ADD

type
  Operand = Variable

type
  Operation = object
    operands: seq[Operand]
    operation_kind: OPERATION_KIND
  Expression = Operation

var
  LINES: seq[Line_of_code]
  FUNCTIONS: seq[function_object]
  # CONTEXT: {"constants": seq[string], "variables": seq[string]}.toTable

proc evaluate(expression: Expression) =
  if ord(expression.operation_kind) == ord(OPERATION_KIND.PRINT):
    for i in expression.operands:
      echo replaceWord(i.variable_default_value.value, "\\\"", "")
  elif ord(expression.operation_kind) == ord(OPERATION_KIND.ADD):
    var total: int = 0
    for i in expression.operands:
      total += parseInt(i.variable_default_value.value)
    echo total

proc get_function_by_name(name: string): int =
  for k, v in FUNCTIONS:
    if v.definition.func_name == name:
      return k

proc execute(func_name: string) =
  var operands: seq[Operand]
  let k: int = get_function_by_name(func_name)
  var f = FUNCTIONS[k]
  let instructions = f.body.lines
  var kind: OPERATION_KIND
  for i in instructions:
    if ord(i.instruction_type) == ord(INSTRUCT_TYPE.FUNC_CALL):
      let loc = i.line
      let li = loc.line_content.strip.split(" ")
      let fonk = li[0]
      for k, v in li:
        if k == 0:
          continue
        else:
          let val = StrValue(value: v)
          let temp_operand = Operand(
            # variable_name: "",
            # variable_type: "string",
            variable_default_value: val
          )
          operands.add(temp_operand)
      case fonk
      of "echo":
        kind = OPERATION_KIND.PRINT
      of "add":
        kind = OPERATION_KIND.ADD
      let op = Operation(
        operands: operands,
        operation_kind: kind
      )
      # echo "Operation :: ", op
      evaluate(op)
      operands = @[]

proc file_reader(): string =
  let f: File = open("main.wizard", fmread)
  let content: string = f.readAll
  f.close
  content

proc parser(): seq[string] =
  let content: string = file_reader()
  let lines: seq[string] = content.splitLines
  lines

proc instruction_from_line(loc: Line_of_code): Instruction =
  var some_flag: bool = false
  var instruct: Instruction 
  var f_name: string
  var instruction_kind: INSTRUCT_TYPE
  var instruction_name: string
  let line = loc.line_content
  if line == "":
    return
  let stripped_line = line.strip
  let indentation_count: int = line.len - stripped_line.len
  let symbols = stripped_line.split(" ")
  let symbol = symbols[0].strip
  case symbol
  of "#f":
    instruction_kind = INSTRUCT_TYPE.FUNC_DEF
    var args: seq[Argument]
    var arg: Argument
    var arg_str: string = ""
    var args_list: seq[string]
    let left_paren_idx = find(line, "(")
    let right_paren_idx = find(line, ")")
    ### *** TODO: Fix this! ***
    let li0 = symbols[1].split("(") 
    let li = li0[0].split(":")
    let func_name = li[0].strip
    let func_type = li[1].strip
    ### ***********************
    for i in left_paren_idx + 1..<right_paren_idx:
      arg_str = arg_str & line[i]
    echo arg_str
    if arg_str != "":
      args_list = arg_str.split(",")
      for j in args_list:
        let li = j.strip.split(":")
        arg.variable_name = li[0]
        arg.variable_type = li[1]
        args.add(arg)
    let func_def = function_definition(
      func_symbol: "#f",
      func_name: func_name,
      open_paren: "(",
      close_paren: ")",
      arguments: args,
      type_seperator: ":",
      type_declaration: func_type,
      equal_sign: "="
    )
    let f = function_object(definition: func_def)
    FUNCTIONS.add(f)
    instruction_name = func_name
  of "var":
    echo "var"
    instruction_kind = INSTRUCT_TYPE.ASSIGNMENT
  of "const":
    echo "const"
    instruction_kind = INSTRUCT_TYPE.ASSIGNMENT
  of "let":
    echo "let"
    instruction_kind = INSTRUCT_TYPE.ASSIGNMENT
  of "return":
    instruction_kind = INSTRUCT_TYPE.RETURN
    let val = StrValue(value: "0")
    let rkind = TYPE_KINDS.INTEGER
    let robj = Return_obj(
      return_value: val,
      return_type: rkind
    )
    # TODO: use loc.block_name to get_function_by_name
    FUNCTIONS[0].body.returns = robj
  else:
    instruction_kind = INSTRUCT_TYPE.FUNC_CALL
    if loc.isIndendet:
      f_name = loc.block_name
      some_flag = true
  if some_flag:
    instruct = Instruction(
      instruction_type: instruction_kind,
      instruction_name: f_name,
      line: loc
    )
    # TODO: use loc.block_name to get_function_by_name
    FUNCTIONS[0].body.lines.add(instruct)
  else:
    instruct = Instruction(
      instruction_type: instruction_kind,
      instruction_name: instruction_name,
    )
  instruct

proc create_line(line: string, line_no: int): Line_of_code =
  var isEmpty: bool = false
  var isIndendet: bool = false
  var char_count: int = 0
  var indentation_count: int = 0
  var block_name: string = "global"
  if line.strip == "":
    isEmpty = true
  else:
    let stripped_line: string = line.strip
    char_count = line.len
    indentation_count = char_count - stripped_line.len
    if indentation_count > 0:
      isIndendet = true
      let len_lines = LINES.len
      for i in 0..len_lines:
        let next_line = LINES[len_lines - i - 1]
        if not next_line.isIndendet:
          block_name = next_line.block_name
          break
    else:
      let char_count = count(line, " ")
      if char_count > 0:
        let li1 = line.split(" ")
        let li2 = li1[1].split(":")
        block_name = li2[0]
  let line_obj = Line_of_code(
    id: line_no,
    line_no: line_no,
    line_content: line,
    char_count: char_count,
    isIndendet: isIndendet,
    indentation_count: indentation_count,
    isEmpty: isEmpty,
    block_name: block_name
  )
  line_obj

proc push_line(loc: Line_of_code) =
  LINES.add(loc)

proc lexer(): seq[Instruction] =
  var instructions: seq[Instruction]
  var lines = parser()
  var counter: int = 0
  let line_count = lines.len
  for i in 0..<line_count:
    counter += 1
    let line = lines[i]
    let loc = create_line(line, counter)
    push_line(loc)
    let instruction = instruction_from_line(loc)
    instructions.add(instruction)
  instructions

proc runner() =
  let instructions = lexer()
  var flag_block: bool = false
  for i in instructions:
    if ord(i.instruction_type) == ord(INSTRUCT_TYPE.FUNC_DEF):
      flag_block = true
    elif ord(i.instruction_type) == ord(INSTRUCT_TYPE.FUNC_CALL) and not flag_block:
      execute(i.instruction_name)
    elif ord(i.instruction_type) == ord(INSTRUCT_TYPE.RETURN) and flag_block:
      flag_block = false
    else:
      discard

proc main() =
    runner()

main()
