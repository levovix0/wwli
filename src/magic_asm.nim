import tables, unicode, strutils
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
    error
  
  Function* = enum
    nop  ## "no operation"
    mov  ## "move", assignment and message sending
    slp  ## "sleep", to end execution
    ech  ## "echo", for debugging

  RegisterSize* = enum
    r16
    r32
    r64

  Register* = object
    case size: RegisterSize
    of r16: reg: char
    of r32: reg2: array[2, char]
    of r64: reg4: array[4, char]

  IntOrReg* = object
    case isReg: bool
    of true: reg: Register
    of false: i: int64

  MagasmStatement* = object
    case kind*: MagasmStatementKind
    of none: discard
    of label:
      label*: string
    of functionCall:
      case function*: Function
      of slp:
        slp*: IntOrReg
      of ech:
        ech*: string
      of mov:
        mov*: tuple[i: IntOrReg, o: Register]
      else:
        args*: seq[IntOrReg]

    ce*: ConditionalExecution

  MagasmCode* = seq[MagasmStatement]


  ParseError* = object of CatchableError

  MagasmErrorKind* = enum
    unknown
    invalidAddress = "invalid port/memory address"
    invalidFunction = "invalid function"
    parseError = "parse error"

  MagasmError* = ref object of CatchableError
    kind: MagasmErrorKind
    line: int

  
  MagasmFlag* = enum
    errorHandling
    negativeNumbers
    floatNumbers
    reg2
    reg4
    semicolonsReqired

  MagasmVm* = object
    memory*: seq[char]
    ports*: Table[char, tuple[read: proc: int64, write: proc(v: int64)]]
    functions*: set[Function]
    code*: MagasmCode
    flags*: set[MagasmFlag]

  MagasmVmInstance* = object
    memory*: Table[char, int16]
    ports*: Table[char, tuple[read: proc: int64, write: proc(v: int64)]]
    functions*: set[Function]
    code*: MagasmCode
    flags*: set[MagasmFlag]
    i*: int  ## instruction pointer
    err*: Option[tuple[e: MagasmError, stepsPassed: int]]  ## last error
  

  StepResultKind* = enum
    ok
    sleep
    echo
    error
  
  StepResult* = object
    case kind*: StepResultKind
    of ok: discard
    of sleep: sleep*: int64
    of echo: echo*: string
    of error: error*: MagasmError


proc newMagasmError(kind: MagasmErrorKind, line: int, parent: ref Exception = nil): MagasmError =
  MagasmError(kind: kind, msg: $kind, line: line, parent: parent)


proc step*(vm: var MagasmVmInstance): StepResult =
  proc `[]`(vm: MagasmVmInstance, reg: char): int64 =
    try:
      if reg.Rune.isLower:
        vm.memory[reg].int64
      else:
        vm.ports[reg].read().int64

    except KeyError:
      raise newMagasmError(invalidAddress, vm.i, getCurrentException())

  proc `[]`(vm: MagasmVmInstance, reg: array[2, char]): int64 =
    vm[reg[0]] + vm[reg[1]] shl 8

  proc `[]`(vm: MagasmVmInstance, reg: array[4, char]): int64 =
    vm[reg[0]] + vm[reg[1]] shl 8 + vm[reg[2]] shl 16 + vm[reg[3]] shl 24


  proc `[]`(vm: MagasmVmInstance, x: Register): int64 =
    case x.size
    of r16: vm[x.reg]
    of r32: vm[x.reg2]
    of r64: vm[x.reg4]

  proc `[]`(vm: MagasmVmInstance, x: IntOrReg): int64 =
    if x.isReg: vm[x.reg]
    else: x.i
  

  proc `[]=`(vm: var MagasmVmInstance, reg: char, v: int64) =
    try:
      if reg.Rune.isLower:
        vm.memory[reg] = v.int16
      else:
        vm.ports[reg].write(v.int16)

    except KeyError:
      raise newMagasmError(invalidAddress, vm.i, getCurrentException())

  proc `[]=`(vm: var MagasmVmInstance, reg: array[2, char], v: int64) =
      vm[reg[0]] = v
      vm[reg[1]] = v shr 8

  proc `[]=`(vm: var MagasmVmInstance, reg: array[4, char], v: int64) =
      vm[reg[0]] = v
      vm[reg[1]] = v shr 8
      vm[reg[2]] = v shr 16
      vm[reg[3]] = v shr 24


  proc `[]=`(vm: var MagasmVmInstance, x: Register, v: int64) =
    case x.size
    of r16: vm[x.reg] = v
    of r32: vm[x.reg2] = v
    of r64: vm[x.reg4] = v


  if vm.code.len == 0: return
  if vm.i >= vm.code.len: vm.i = 0

  let n = vm.code[vm.i]
  try:
    case n
    of none() | label() | functionCall(function: nop):
      inc vm.i
    
    of functionCall(function: slp, slp: @i):
      ## note: builtin function
      return StepResult(kind: sleep, sleep: vm[i])
    
    of functionCall(function: ech, ech: @s):
      ## note: builtin function
      ## formatting:
      ##   {a} + {bc} = somthing
      ##   {a} and {bc} will be replaced by the value of a and bc
      var r = ""
      var i = 0
      while i < s.len:
        template alph(x): bool = (i+x < s.len and s[i+x].Rune.isAlpha)
        template st: bool = (s[i] == '{')
        template en(x): bool = (i+x < s.len and s[i+x] == '}')
        template ad(n, v) =
          i += n
          r.add $vm[v]
        
        if st and alph(1) and en(2):
          ad(2, s[i+1])
        elif (reg2 in vm.flags) and st and alph(1) and alph(2) and en(3):
          ad(3, [s[i+1], s[i+2]])
        elif (reg4 in vm.flags) and st and alph(1) and alph(2) and alph(3) and alph(4) and en(5):
          ad(5, [s[i+1], s[i+2], s[i+3], s[i+4]])
        else:
          r.add s[i]
        
        inc i

      return StepResult(kind: echo, echo: r)

    of functionCall(function: mov, mov: (@i, @o)):
      ## note: builtin function
      vm[o] = vm[i]
  
  except MagasmError:
    if errorHandling notin vm.flags:
      return StepResult(kind: error, error: getCurrentException().MagasmError)

    elif vm.err.isSome:
      result = StepResult(kind: error, error: vm.err.get.e)
      vm.err = some (getCurrentException().MagasmError, 0)
    
    else:
      vm.err = some (getCurrentException().MagasmError, 0)


proc parseMagasm*(code: string, flags: set[MagasmFlag]): MagasmCode =
  type
    TokenKind = enum
      eol
      colon
      semicolon
      num
      str
      op
      word

    Token = object
      case kind: TokenKind
      of num:
        num: int64
      of str:
        str: string
      of word:
        word: string
      of op:
        op: string
      else: discard
  
  proc tokenize(code: string, flags: set[MagasmFlag]): seq[Token] =
    var i = 0
    var line = 0
    var isString: bool
    
    proc peek(delta=0): char =
      if i + delta < code.len:
        code[i + delta]
      else:
        '\0'
    
    proc next(delta=0): char =
      for i in i .. min(i+delta, code.high):
        if code[i] == '\n':
          inc line
          isString = false
      i += delta
      defer: inc i
      if i < code.len:
        code[i]
      else:
        '\0'
    
    proc skip(n=1) =
      for i in i ..< min(i+n, code.len):
        if code[i] == '\n':
          inc line
          isString = false
      i += n

    while i < code.len:
      let c = peek()

      template parseNumber =
        var r = ""
        while true:
          let x = peek()
          if not(x in '0'..'9' or (floatNumbers in flags and x == '.') or (negativeNumbers in flags and x == '.')):
            break
          r.add x
          skip()
        if '.' in r:
          result.add Token(kind: num, num: cast[int32](parseFloat(r).float32).int64)
        else:
          result.add Token(kind: num, num: parseInt(r).int64)
      
      template parseWord =
        var r = ""
        while true:
          let x = peek()
          if not(x in 'a'..'z' or x in 'A'..'Z'):
            break
          r.add x
          skip()
        if r == "ech":
          isString = true
          skip()
        result.add Token(kind: word, word: r)

      template parseOp =
        var r = ""
        while true:
          let x = peek()
          if x notin "+-*/!=$%&><|":
            break
          r.add x
          skip()
        result.add Token(kind: op, op: r)
      
      template parseString =
        var r = ""
        while true:
          let x = next()
          if x == '\n': break
          r.add x
        result.add Token(kind: str, str: r)
        result.add Token(kind: eol)
      
      
      if isString:
        parseString

      elif c in '0'..'9':
        parseNumber

      elif (c in 'a'..'z') or (c in 'A'..'Z'):
        parseWord

      elif c == '-' and (negativeNumbers in flags) and (peek(1) in '0'..'9'):
        parseNumber
      elif c in "+-*/!=$%&><|":
        parseOp

      elif c == ';':
        result.add Token(kind: semicolon)
        while next() != '\n': discard
        result.add Token(kind: eol)

      elif c in {'\n', '\0'}:
        result.add Token(kind: eol)
        inc line
        isString = false
        inc i

      elif c == ':':
        result.add Token(kind: colon)
        inc i

      elif c notin " \r\t":
        raise newMagasmError(parseError, line)

      else:
        inc i

  proc parse(tokens: seq[Token]): seq[MagasmStatement] =
    ## todo
    #[
        if r.len == 3:
          block a:
            for x in Function.nop..Function.ech:
              if r == $x:
                result.add Token(kind: fn, fn: x)
                if x == ech:
                  isString = true
                break a
            ## todo: add custom function
        elif r.len == 1:
          result.add Token(kind: reg, reg: Register(size: r16, reg: r[0]))
        elif reg2 in flags and r.len == 2:
          result.add Token(kind: reg, reg: Register(size: r32, reg2: [r[0], r[1]]))
        elif reg4 in flags and r.len == 4:
          result.add Token(kind: reg, reg: Register(size: r64, reg4: [r[0], r[1], r[2], r[3]]))]#
  
  echo code.tokenize(flags)

discard parseMagasm(
"""
start:
  ba = ab
  slp 1
  ->start
""",
  {reg2, reg4, negativeNumbers, floatNumbers}
)
