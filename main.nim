import system/io
import std/strutils
import std/tables

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
    tokens: seq[string]

type
  TYPE_KINDS = enum
    INTEGER
    STRING
    BOOLEAN
    FUNCTION_OBJ
    CLASS_OBJ

type
  TOKENS = enum
    FUNC_IDENTIFIER
    FUNC_NAME
    TYPE_SEP
    TYPE_DEF
    LEFT_PAREN
    RIGHT_PAREN
    ARGUMENT_NAME
    ARGUMENT_TYPE
    LEFT_SQUARE_BRACKET
    RIGHT_SQUARE_BRACKET
    EQUAL_SIGN
    SYMBOL
    DOUBLE_QUOTES
    STRING_LITERAL
    RETURN_STATEMENT
    NUMBER
    CALL
    NAME
  OPERATION_KIND = enum
    ASSIGNMENT
    PRINT
    ADD
    SUBSTRACT
    MULTIPLY
    DIVISION

type
  Variable = object
    variable_name: string
    variable_value: string
    variable_type: string
    variable_default_value: string
  Argument = Variable
  Operand = Variable
  Operation = object
    operands: seq[Operand]
    operation_kind: OPERATION_KIND
  Expression = Operation
  Statement = object
    expression: Expression
    line: string
  Code_obj = object
    statement: Statement

type
  function_definition = object
    func_symbol: string
    func_name: string
    type_seperator: string
    type_declaration: string
    open_paren: string
    arguments: seq[Argument]
    close_paren: string
    equal_sign: string
  function_body = object
    lines: seq[Code_obj]
  function_object = object
    definition: function_definition
    body: function_body

type
  Token = object
    token_type: TOKENS
    token_content: string
    token_length: int

var
  LINES: seq[Line_of_code]
  FUNCTIONS: seq[function_object]
  STACK: seq[Code_obj]
  # CONTEXT: {"constants": seq[string], "variables": seq[string]}.toTable

proc evaluate(expression: Expression) =
  if ord(expression.operation_kind) == ord(OPERATION_KIND.PRINT):
    for i in expression.operands:
      echo replaceWord(i.variable_default_value, "\\\"", "")
  elif ord(expression.operation_kind) == ord(OPERATION_KIND.ADD):
    var total: int = 0
    for i in expression.operands:
      total += parseInt(i.variable_default_value)
    echo total

proc get_function_by_name(name: string): int =
  for k, v in FUNCTIONS:
    if v.definition.func_name == name:
      return k

# proc execute(func_name: string) =
#   var operands: seq[Operand]
#   let k: int = get_function_by_name(func_name)
#   var f = FUNCTIONS[k]
#   let instructions = f.body.lines
#   var kind: OPERATION_KIND
#   for i in instructions:
#     if ord(i.instruction_type) == ord(INSTRUCT_TYPE.FUNC_CALL):
#       let loc = i.line
#       let li = loc.line_content.strip.split(" ")
#       let fonk = li[0]
#       for k, v in li:
#         if k == 0:
#           continue
#         else:
#           let val = StrValue(value: v)
#           let temp_operand = Operand(
#             # variable_name: "",
#             # variable_type: "string",
#             variable_default_value: val
#           )
#           operands.add(temp_operand)
#       case fonk
#       of "echo":
#         kind = OPERATION_KIND.PRINT
#       of "add":
#         kind = OPERATION_KIND.ADD
#       let op = Operation(
#         operands: operands,
#         operation_kind: kind
#       )
#       # echo "Operation :: ", op
#       evaluate(op)
#       operands = @[]

proc file_reader(): string =
  let f: File = open("any.wizard", fmread)
  let content: string = f.readAll
  f.close
  content

proc parse_lines(): seq[string] =
  let content: string = file_reader()
  let lines: seq[string] = content.splitLines
  lines

# proc decider(token): Code_obj =
#   var token_kind: TOKENS
#   case token
#   of "#f":
#     token_kind = TOKENS.FUNC_IDENTIFIER
#     var args: seq[Argument]
#     var arg: Argument
#     var arg_str: string = ""
#     var args_list: seq[string]
#     let left_paren_idx = find(line, "(")
#     let right_paren_idx = find(line, ")")
#     let li0 = symbols[1].split("(")
#     let li = li0[0].split(":")
#     let func_name = li[0].strip
#     let func_type = li[1].strip
#     ### ***********************
#     for i in left_paren_idx + 1..<right_paren_idx:
#       arg_str = arg_str & line[i]
#     echo arg_str
#     if arg_str != "":
#       args_list = arg_str.split(",")
#       for j in args_list:
#         let li = j.strip.split(":")
#         arg.variable_name = li[0]
#         arg.variable_type = li[1]
#         args.add(arg)
#     let func_def = function_definition(
#       func_symbol: "#f",
#       func_name: func_name,
#       open_paren: "(",
#       close_paren: ")",
#       arguments: args,
#       type_seperator: ":",
#       type_declaration: func_type,
#       equal_sign: "="
#     )
#     let f = function_object(definition: func_def)
#     FUNCTIONS.add(f)
#   of "var":
#     echo "var"
#   of "const":
#     echo "const"
#   of "let":
#     echo "let"
#   of "return":
#     let rkind = TYPE_KINDS.INTEGER
#     FUNCTIONS[0].body.returns = robj
#   else:
#     if loc.isIndendet:
#       f_name = loc.block_name
#       some_flag = true
#     FUNCTIONS[0].body.lines.add(instruct)

proc line_analyzer(loc: Line_of_code) =
  let line = loc.line_content
  if line == "":
    return
  let stripped_line = line.strip
  let indentation_count: int = line.len - stripped_line.len
  let symbols = stripped_line.split(" ")
  let symbol = symbols[0].strip

proc create_line(line: string, line_no: int, tokens: seq[string]): Line_of_code =
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
    block_name: block_name,
    tokens: tokens
  )
  line_obj

proc push_line(loc: Line_of_code) =
  LINES.add(loc)

proc tokenize(): seq[string] =
  var counter: int = 0
  var tokens: seq[string]
  var lines = parse_lines()
  let line_count = lines.len
  for i in 0..<line_count:
    counter += 1
    let line: string = lines[i]
    var temp_str: string
    var indentation_count: int = 0
    var indentation_flag:bool = false
    var idx: int
    var prev_flag: bool = false
    var curr_flag: bool = false
    var pass_flag: bool = false
    var space_flag: bool = false
    var string_literal_flag: bool = false
    var left_paren_flag: bool = false
    var right_paren_flag: bool = false
    var left_curly_flag: bool = false
    var right_curly_flag: bool = false
    var left_square_flag: bool = false
    var right_square_flag: bool = false
    var percent_flag: bool = false
    var dollar_flag: bool = false
    var comment_flag: bool = false
    var multiline_comment_flag: bool = false
    const li = ['|', '<', '>', '=', '+', '-', '/', '*', '$', '%', '&', '{', '}', '(', ')', '[', ']', '^', '"', '\'', '?', '!', ':', ';', ',', '.']
    const parantheses = ['{', '}', '(', ')', '[', ']']
    for k, chr in line:
      idx = k - indentation_count
      var previous_char: char
      var next_char: char
      if k > 0:
        previous_char = line[k - 1]
      if k < line.len - 1:
        next_char = line[k + 1]
      pass_flag = false
      prev_flag = false
      curr_flag = false
      for c in li:
        if previous_char == c:
          prev_flag = true
        if chr == c:
          curr_flag = true
        if prev_flag and curr_flag:
          pass_flag = true
          break
      stdout.write "pass_flag: ", pass_flag, " | "
      stdout.write " *** k: #", k, " | idx: #", idx
      stdout.write " | indentation count: #", indentation_count, " , flag: ", indentation_flag
      stdout.write " | previous: '", $previous_char, "' | current: '", $chr, "' | next: '", $next_char, "' | Temporary String => '", temp_str, "' *** \n"
      if k == 0 and chr == ' ':
        indentation_flag = true
        indentation_count += 1
      elif idx == 0 and indentation_flag == true and chr == ' ':
        indentation_count += 1
      elif idx == 0 and (chr == ':' or chr == '|' or chr == '>' or chr == '<'):
        echo "error happened @ char: '", $chr, "', cannot be one of those | > < :"
        quit()
      elif string_literal_flag == true and chr != '"':
        continue
      elif string_literal_flag == true and chr == '"':
        string_literal_flag = false
      elif chr != ' ':
        if indentation_count > 0 and indentation_count mod 4 != 0:
          echo "indentation error"
          quit()
        elif indentation_count > 0 and indentation_count mod 4 == 0:
          var space_str = ""
          for it in 0..<indentation_count:
            space_str.add(" ")
          tokens.add(space_str)
        space_flag = false
        indentation_flag = false
        # TODO: indentation_count?
        indentation_count = 0
        if idx > 0 and (temp_str != "" or previous_char == ' '):
          if pass_flag == false:
            if curr_flag and temp_str != "":
              tokens.add(temp_str)
              temp_str = ""
            if chr == ':':
              tokens.add(":")
            elif chr == '|':
              echo "chr: ", chr, " | next: ", next_char
              if next_char == '>':
                tokens.add("|>")
              else:
                tokens.add("|")
            elif chr == '<':
              if next_char == '|':
                tokens.add("<|")
              else:
                tokens.add("<")
            elif chr == '>':
              echo "\t\t\t\t --- previous_char :: ", previous_char
              if previous_char == '|':
                discard
              elif previous_char == '-':
                discard
              else:
                tokens.add(">")
            elif chr == '=':
              var f: bool = false
              for s in ['*', '/', '+', '-', '~', '&', '^', '=']:
                if previous_char == s:
                  f = true
                  break
              if f == false:
                tokens.add("=")
              if next_char == '=':
                tokens.add("=")
            elif chr == '.':
              tokens.add(".")
            elif chr == '~':
              tokens.add("~")
            elif chr == '&':
              tokens.add("&")
            elif chr == '?':
              tokens.add("?")
            elif chr == '$':
              dollar_flag = true
              tokens.add("$")
            elif chr == '^':
              tokens.add("^")
            elif chr == '/':
              if next_char == '/':
                tokens.add("//")
                comment_flag = true
              elif next_char == '*':
                tokens.add("/*")
                multiline_comment_flag = true
              elif next_char == '=':
                tokens.add("/=")
              else:
                tokens.add("/")
            elif chr == '+':
              if next_char == '+':
                tokens.add("++")
              elif next_char == '=':
                tokens.add("+=")
              else:
                tokens.add("+")
            elif chr == '*':
              if next_char == '*':
                tokens.add("**")
              elif next_char == '=':
                tokens.add("*=")
              else:
                tokens.add("*")
            elif chr == '-':
              if next_char == '>':
                tokens.add("->")
              elif next_char == '=':
                tokens.add("-=")
              else:
                tokens.add("-")
            elif chr == '(':
              left_paren_flag = true
              if next_char == ')':
                tokens.add("()")
              else:
                tokens.add("(")
            elif chr == ')':
              right_paren_flag = true
              tokens.add(")")
            elif chr == '{':
              left_curly_flag = true
              if next_char == '}':
                tokens.add("{}")
              else:
                tokens.add("{")
            elif chr == '}':
              right_curly_flag = true
              tokens.add("}")
            elif chr == '[':
              left_square_flag = true
              if next_char == ']':
                tokens.add("[]")
              else:
                tokens.add("[")
            elif chr == ']':
              right_square_flag = true
              tokens.add("]")
            elif chr == ',':
              tokens.add(",")
            elif chr == ';':
              tokens.add(";")
            elif chr == '"':
              if string_literal_flag == true:
                string_literal_flag = false
              else:
                string_literal_flag = true
              tokens.add("\"")
            elif chr == '\'':
              tokens.add("'")
              let character = next_char
              # TODO: escape sequence "\" has to be considered
              if line[idx + 2] == '\'':
                tokens.add($character)
                tokens.add("'")
              else:
                echo "char error"
                quit()
        elif idx > 0 and temp_str == "" and chr != ' ':
          for p in parantheses:
            if chr == p:
              tokens.add($p)
              break

        elif idx == 0 and temp_str == "":
          if chr == '+':
            # positif number
            discard
          elif chr == '-':
            # negatif number
            discard
          elif chr == '(':
            # tuple ops
            discard
          elif chr == '{':
            # set, dict ops
            discard
          elif chr == '[':
            # sequence, list, array ops
            discard
          elif chr == '%':
            percent_flag = true
            tokens.add("%")
          elif chr == '"':
            # string ops
            discard

      elif chr == ' ':
        if temp_str != "":
          space_flag = true
          tokens.add(temp_str)
          temp_str = ""
      elif chr == ' ' and temp_str == "":
        stdout.write " {emptiness!} "
      else:
        stdout.write " [ERROR] in lexer! (chr: '", $chr, "') "
        # quit()
      # echo "\t Tokens => ", tokens

      # --------
      if dollar_flag == true:
        dollar_flag = false
      elif percent_flag == true:
        percent_flag = false
      elif not space_flag and not indentation_flag and not pass_flag:
        # temp_str.add(chr)
        if not curr_flag:
          temp_str &= $chr
          temp_str = strip(temp_str)
    if temp_str != "":
      tokens.add(temp_str)
      temp_str = ""
    tokens.add("\n")
    let loc = create_line(line, counter, tokens)
    push_line(loc)
  return tokens


proc pop_stack() =
  discard

proc push_stack() =
  discard

proc run_stack() =
  # let stack = create_stack()
  # for i in stack:
  #   execute(i)
  # echo tokenize()
  for token in tokenize():
    stdout.write(" '")
    stdout.write(token)
    stdout.write("' ")

proc main() =
    run_stack()

main()
