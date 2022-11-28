import tables
import fusion/matching

{.experimental: "overloadableEnums".}
{.experimental: "caseStmtMacros".}

type
  MagasmStatementKind* = enum
    none
    label
    functionCall
  
  ConditionalExecution* = enum
    no
    true
    false
  
  Function* = enum
    nop  ## "no operation"
    mov  ## "move", assignment and message sending
    slp  ## "sleep", to end execution
    ech  ## "echo", for debugging

  EchArgKind* = enum
    text
    reg
  EchArg* = object
    kind*: EchArgKind
    text*: string

  MagasmStatement* = object
    case kind*: MagasmStatementKind
    of none: discard
    of label:
      label*: string
    of functionCall:
      case function*: Function
      of slp:
        slp*: tuple[ms: int]
      of ech:
        ech*: seq[EchArg]
      else:
        args*: seq[string]

    ce*: ConditionalExecution

  MagasmCode* = seq[MagasmStatement]

  ParseError* = object of CatchableError

  MagasmErrorKind* = enum
    unknown
    invalidMemoryAddress = "invalid memory address"
    invalidPort = "invalid port"
    invalidFunction = "invalid function"

  MagasmVm* = object
    memory*: seq[char]
    ports*: Table[char, tuple[read: proc: int16, write: proc(v: int16)]]
    functions*: set[Function]
    code*: MagasmCode

  MagasmVmInstance* = object
    memory*: Table[char, int16]
    ports*: Table[char, tuple[read: proc: int16, write: proc(v: int16)]]
    functions*: set[Function]
    code*: MagasmCode
    i*: int  ## instruction pointer
    err*: tuple[i: int, k: MagasmErrorKind, s: string]  ## last error


proc parseMagasm*(code: string): MagasmCode =
  ## todo
  # let p = peg("code", r: MagasmCode):
  #   none <- 0

  #   label <- >*(1 - ':' - '\n') * ':':
  #     r.add MagasmStatement(kind: label, label: $1)

  #   nop <- "nop" * *(" " * +Alpha):
  #     r.add MagasmStatement(kind: functionCall, function: nop)
  #   slp <- "slp" * " " * >+Digit:
  #     r.add MagasmStatement(kind: functionCall, function: slp)

  #   functionCall <- nop

  #   line <- (none|label|functionCall) * '\n'
  #   code <- *line


proc step*(vm: var MagasmVmInstance): tuple[sleep: int16] =
  if vm.code.len == 0: return
  if vm.i >= vm.code.len: vm.i = 0

  let n = vm.code[vm.i]
  case n
  of none():
    inc vm.i
    echo "got it!"

