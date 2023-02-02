import tables, unicode, strutils, options, macros, math, sets, sequtils
import hmatching, fusion/astdsl

{.experimental: "overloadableEnums".}
{.experimental: "caseStmtMacros".}
{.experimental: "callOperator".}

type
  StatementKind* = enum
    none
    label
    functionCall
    advancedCall
  
  ConditionalExecution* = enum
    no
    true
    false
    error
  
  Function* = enum
    nop  ## "no operation"
    mov  ## "move", assignment and message sending
    jmp  ## "jump", goto label
    slp  ## "sleep", to end execution

    add  ## "add", add second value to first register
    sub  ## "sub", a -= b
    mul  ## "mul", a *= b
    `div`  ## "div", a /= b
    `mod`  ## "mod", a %= b
    pow  ## "pow", a ^= b
    rot  ## "root", a = pow(a, 1/b)

    neg  ## "negative", a = -a

    jmo  ## "jump offset", jump by X lines
    jlo  ## "jump label offset", jump to label with offset

    ceq  ## "compare equal", if a == b, sets ce to +, else to -, do not touch *
    cne  ## "compare not equal", if a != b, sets ce to +, else to -, do not touch *
    clt  ## "compare less than", if a < b, sets ce to +, else to -, do not touch *
    cgt  ## "compare greater than", if a > b, sets ce to +, else to -, do not touch *
    cle  ## "compare less or equal", if a >= b, sets ce to +, else to -, do not touch *
    cge  ## "compare greater or equal", if a <= b, sets ce to +, else to -, do not touch *

    cal  ## "call", pushes instruction pointer and jumps to label
    ret  ## "return", pops instruction pointer

    puf  ## "push frame", pushes value of specified variables to stack
    pof  ## "pop frame", pops value of some variables from stack
    
    sce  ## "set conditional execution", set vm.ce flag to mask (1|0 + 2|0 + 4|0)
    gce  ## "get conditional execution", get vm.ce flag to mask (1|0 + 2|0 + 4|0)

    sip  ## "set instruction pointer", set vm.i (and bound it to low..high)
    gip  ## "get instruction pointer", get vm.i
    pui  ## "push instruction pointer", pushes vm.i to vm.istack
    poi  ## "pop instruction pointer", pops vm.i from vm.istack (and store it to reg)
    
    ech  ## "echo", for debugging

    custom  ## costom function from mod or player


  RegisterSize* = enum
    r16
    r32
    r64

  Register* = object
    case size: RegisterSize
    of r16: reg: char
    of r32: reg2: array[2, char]
    of r64: reg4: array[4, char]

  IsReg* = enum
    true
    false

  IntOrReg* = object
    case isReg: IsReg
    of true: reg: Register
    of false: i: int64

  FunctionCall* = object
    case kind*: Function
    of mov:
      mov*: tuple[i: IntOrReg, o: Register]
    of jmp:
      jmp*: string
    of slp:
      slp*: IntOrReg

    of add:
      add*: tuple[a: Register, b: IntOrReg]
    of sub:
      sub*: tuple[a: Register, b: IntOrReg]
    of mul:
      mul*: tuple[a: Register, b: IntOrReg]
    of `div`:
      `div`*: tuple[a: Register, b: IntOrReg]
    of `mod`:
      `mod`*: tuple[a: Register, b: IntOrReg]
    of pow:
      pow*: tuple[a: Register, b: IntOrReg]
    of rot:
      rot*: tuple[a: Register, b: IntOrReg]
    
    of neg:
      neg*: Register

    of jmo:
      jmo*: IntOrReg
    of jlo:
      jlo*: tuple[l: string, o: IntOrReg]

    of ceq:
      ceq*: tuple[a, b: IntOrReg]
    of cne:
      cne*: tuple[a, b: IntOrReg]
    of clt:
      clt*: tuple[a, b: IntOrReg]
    of cgt:
      cgt*: tuple[a, b: IntOrReg]
    of cle:
      cle*: tuple[a, b: IntOrReg]
    of cge:
      cge*: tuple[a, b: IntOrReg]

    of cal:
      cal*: string
    of ret:
      ret*: tuple[]

    of puf:
      puf*: seq[Register]
    of pof:
      pof*: tuple[]

    of sce:
      sce*: IntOrReg
    of gce:
      gce*: Register
    
    of sip:
      sip*: IntOrReg
    of gip:
      gip*: Register
    of pui:
      pui*: IntOrReg
    of poi:
      poi*: Register

    of ech:
      ech*: string
    
    of custom:
      custom*: tuple[name: string, args: seq[IntOrReg]]

    else:
      args*: seq[IntOrReg]

  Statement* = object
    case kind*: StatementKind
    of none: discard
    of label:
      label*: string
    of functionCall:
      functionCall*: FunctionCall
    of advancedCall:
      advancedCall*: tuple[label: string, args: seq[tuple[arg: Register, val: IntOrReg]]]

  StatementCe* = object
    statement*: Statement
    ce*: ConditionalExecution

  MagasmCode* = seq[StatementCe]


  ParseError* = object of CatchableError

  MagasmErrorKind* = enum
    unknown
    invalidAddress = "invalid port/memory/instruction address"
    invalidFunction = "invalid function"
    parseError = "parse error"
    divisionByZero = "division by 0"
    recursionLimit = "recursion limit"
    rangeError = "range error"

  MagasmError* = object of CatchableError
    kind: MagasmErrorKind
    line: int

  
  MagasmFlag* = enum
    errorHandling
    negativeNumbers
    floatNumbers
    reg2
    reg4
    semicolonsReqired
    autoRoundFloats
    divisionByZeroError
    advancedCallOperator
    autoFrames

  MagasmVm* = object
    memory*: set[char]
    ports*: Table[char, tuple[read: proc: int64, write: proc(v: int64)]]
    functions*: set[Function.add..Function.poi]
    customFunctions*: Table[string, proc(vm: var MagasmVmInstance, args: seq[IntOrReg])]
    code*: MagasmCode
    flags*: set[MagasmFlag]
    recursionLimit: int

  MagasmVmInstance* = object
    memory*: Table[char, int16]
    
    ports*: Table[char, tuple[read: proc: int64, write: proc(v: int64)]]
    functions*: set[Function.add..Function.poi]
    customFunctions*: Table[string, proc(vm: var MagasmVmInstance, args: seq[IntOrReg])]
    code*: MagasmCode
    flags*: set[MagasmFlag]
    recursionLimit: int

    i*: int  ## instruction pointer
    istack*: seq[int]  ## instruction pointer stack
    fstack*: seq[seq[tuple[reg: Register, val: int64]]]  ## frame stack
    err*: Option[tuple[e: MagasmError, stepsPassed: int]]  ## last error
    ce*: set[ConditionalExecution.true..ConditionalExecution.error]
  

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


proc `[]`(x: FunctionCall, i: static FieldIndex, kind: static Function): auto =
  ## todo: fork hmatching
  ## todo: create this proc automaticaly
  when kind == mov:
    x.mov
  # todo: other functions
  else:
    x.args


proc newMagasmError(kind: MagasmErrorKind, line: int, parent: ref Exception = nil): ref MagasmError =
  (ref MagasmError)(kind: kind, msg: "line " & $line & ": " & $kind, line: line, parent: parent)


proc `[]`*(vm: MagasmVmInstance, reg: char): int64 =
  try:
    if reg == '_':
      0'i64
    elif reg.Rune.isLower:
      cast[int64](vm.memory[reg])
    else:
      vm.ports[reg].read()

  except KeyError:
    raise newMagasmError(invalidAddress, vm.i, getCurrentException())

proc `[]`*(vm: MagasmVmInstance, reg: array[2, char]): int64 =
  let
    a = vm[reg[0]]
    b = vm[reg[1]]
  if b < 0:  # negative number
    cast[int64]([cast[int16](a), cast[int16](b), -1, -1])
  else:
    cast[int64]([cast[int16](a), cast[int16](b), 0, 0])

proc `[]`*(vm: MagasmVmInstance, reg: array[4, char]): int64 =
  let
    a = vm[reg[0]]
    b = vm[reg[1]]
    c = vm[reg[2]]
    d = vm[reg[3]]
  cast[int64]([cast[int16](a), cast[int16](b), cast[int16](c), cast[int16](d)])


proc `[]`*(vm: MagasmVmInstance, x: Register): int64 =
  case x.size
  of r16: vm[x.reg]
  of r32: vm[x.reg2]
  of r64: vm[x.reg4]

proc `[]`*(vm: MagasmVmInstance, x: IntOrReg): int64 =
  if x.isReg == true: vm[x.reg]
  else: x.i


proc `[]=`*(vm: var MagasmVmInstance, reg: char, v: int64) =
  try:
    if reg == '_':
      discard
    elif reg.Rune.isLower:
      vm.memory[reg] = cast[int16](v and 0xffff)
    else:
      vm.ports[reg].write(v)

  except KeyError:
    raise newMagasmError(invalidAddress, vm.i, getCurrentException())

proc `[]=`*(vm: var MagasmVmInstance, reg: array[2, char], v: int64) =
  for i, c in reg:
    vm[c] = v shr (i * 16)

proc `[]=`*(vm: var MagasmVmInstance, reg: array[4, char], v: int64) =
  for i, c in reg:
    vm[c] = v shr (i * 16)


proc `[]=`*(vm: var MagasmVmInstance, x: Register, v: int64) =
  case x.size
  of r16: vm[x.reg] = v
  of r32: vm[x.reg2] = v
  of r64: vm[x.reg4] = v


proc findLabel*(vm: MagasmVmInstance, l: string): int =
  for j, x in vm.code:
    if x.ce != no:
      if x.ce notin vm.ce: continue
    if x.statement.kind == label and x.statement.label == l:
      return j
  raise newMagasmError(invalidAddress, vm.i)

proc step*(vm: var MagasmVmInstance): StepResult =
  defer:
    if vm.err.isSome:
      vm.ce.incl error
    else:
      vm.ce.excl error

  if vm.code.len == 0: return
  if vm.i >= vm.code.len: vm.i = 0

  let n = vm.code[vm.i]
  try:
    if n.ce != no:
      if n.ce notin vm.ce:
        inc vm.i
        return

    template makeSureVmHasFunction(x: Function) =
      if x notin vm.functions:
        raise newMagasmError(invalidFunction, vm.i)

    template setCond(x: bool) =
      if x:
        vm.ce.incl true
        vm.ce.excl false
      else:
        vm.ce.excl true
        vm.ce.incl false
      inc vm.i
    
    proc puf(vm: var MagasmVmInstance, vars: seq[Register]) =
      makeSureVmHasFunction puf
      var frame: seq[tuple[reg: Register, val: int64]]
      for r in vars:
        frame.add (r, vm[r])
      vm.fstack.add frame

    proc pof(vm: var MagasmVmInstance) =
      makeSureVmHasFunction pof
      if vm.fstack.len == 0:
        raise newMagasmError(rangeError, vm.i)
      let frame = vm.fstack.pop
      for (r, v) in frame:
        vm[r] = v

    case n.statement
    of none() | label() | functionCall(functionCall: nop()):
      inc vm.i


    of functionCall(functionCall: mov(mov: (@i, @o))):
      ## note: builtin function
      vm[o] = vm[i]
      inc vm.i

    of functionCall(functionCall: jmp(jmp: @l)):
      ## note: builtin function
      vm.i = vm.findLabel(l) + 1
    
    of functionCall(functionCall: slp(slp: @x)):
      ## note: builtin function
      result = StepResult(kind: sleep, sleep: vm[x])
      inc vm.i


    of functionCall(functionCall: add(add: (@a, @b))):
      makeSureVmHasFunction add
      vm[a] = vm[a] + vm[b]
      inc vm.i
    
    of functionCall(functionCall: sub(sub: (@a, @b))):
      makeSureVmHasFunction sub
      vm[a] = vm[a] - vm[b]
      inc vm.i
    
    of functionCall(functionCall: mul(mul: (@a, @b))):
      makeSureVmHasFunction mul
      vm[a] = vm[a] * vm[b]
      inc vm.i
    
    of functionCall(functionCall: `div`(`div`: (@a, @b))):
      makeSureVmHasFunction `div`
      let c = vm[b]
      if c == 0:
        if divisionByZeroError in vm.flags:
          raise newMagasmError(divisionByZero, vm.i)
        else:
          vm[a] = 0
      else:
        vm[a] = vm[a] div c
      inc vm.i
    
    of functionCall(functionCall: `mod`(`mod`: (@a, @b))):
      makeSureVmHasFunction `mod`
      let c = vm[b]
      if c == 0:
        if divisionByZeroError in vm.flags:
          raise newMagasmError(divisionByZero, vm.i)
      else:
        vm[a] = vm[a] mod c
      inc vm.i
    
    of functionCall(functionCall: pow(pow: (@a, @b))):
      makeSureVmHasFunction pow
      let c = vm[b]
      if c < 0:
        vm[a] = 0
      else:
        vm[a] = vm[a] ^ c
      inc vm.i
    
    of functionCall(functionCall: rot(rot: (@a, @b))):
      makeSureVmHasFunction rot
      if autoRoundFloats in vm.flags:
        vm[a] = pow(vm[a].float, 1 / vm[b].float).round.int64
      else:
        vm[a] = pow(vm[a].float, 1 / vm[b].float).int64
      inc vm.i


    of functionCall(functionCall: neg(neg: @a)):
      makeSureVmHasFunction neg
      vm[a] = -vm[a]
      inc vm.i


    of functionCall(functionCall: jmo(jmo: @o)):
      makeSureVmHasFunction jmo
      vm.i = vm.i + vm[o].int

    of functionCall(functionCall: jlo(jlo: (@l, @o))):
      makeSureVmHasFunction jlo
      vm.i = vm.findLabel(l) + vm[o].int


    of functionCall(functionCall: ceq(ceq: (@a, @b))):
      makeSureVmHasFunction ceq
      setCond vm[a] == vm[b]

    of functionCall(functionCall: cne(cne: (@a, @b))):
      makeSureVmHasFunction cne
      setCond vm[a] != vm[b]

    of functionCall(functionCall: clt(clt: (@a, @b))):
      makeSureVmHasFunction clt
      setCond vm[a] < vm[b]

    of functionCall(functionCall: cgt(cgt: (@a, @b))):
      makeSureVmHasFunction cgt
      setCond vm[a] > vm[b]

    of functionCall(functionCall: cle(cle: (@a, @b))):
      makeSureVmHasFunction cle
      setCond vm[a] <= vm[b]

    of functionCall(functionCall: cge(cge: (@a, @b))):
      makeSureVmHasFunction cge
      setCond vm[a] >= vm[b]


    of functionCall(functionCall: cal(cal: @l)):
      makeSureVmHasFunction cal
      if autoFrames in vm.flags:
        vm.puf(@[])
      if vm.istack.len >= vm.recursionLimit:
        raise newMagasmError(recursionLimit, vm.i)
      vm.istack.add vm.i
      vm.i = vm.findLabel(l) + 1
    
    of functionCall(functionCall: ret()):
      makeSureVmHasFunction ret
      if autoFrames in vm.flags:
        pof vm
      if vm.istack.len == 0:
        raise newMagasmError(rangeError, vm.i)
      vm.i = vm.istack.pop + 1


    of functionCall(functionCall: puf(puf: @vars)):
      vm.puf(vars)
      inc vm.i
    
    of functionCall(functionCall: pof()):
      pof vm
      inc vm.i


    of functionCall(functionCall: sce(sce: @x)):
      makeSureVmHasFunction sce
      let mask = vm[x]
      vm.ce = {}
      if ((mask and 0x1) == 0x1): vm.ce.incl true
      if ((mask and 0x2) == 0x2): vm.ce.incl false
      if ((mask and 0x4) == 0x4): vm.ce.incl error
      inc vm.i
    
    of functionCall(functionCall: gce(gce: @x)):
      makeSureVmHasFunction gce
      var mask: int64
      if true in vm.ce: mask += 0x1
      if false in vm.ce: mask += 0x2
      if error in vm.ce: mask += 0x4
      vm[x] = mask
      inc vm.i
    

    of functionCall(functionCall: sip(sip: @x)):
      makeSureVmHasFunction sip
      vm.i = vm[x].max(vm.code.low).min(vm.code.high).int
      inc vm.i
    
    of functionCall(functionCall: gip(gip: @x)):
      makeSureVmHasFunction gip
      vm[x] = vm.i
    
    of functionCall(functionCall: pui(pui: @x)):
      makeSureVmHasFunction pui
      vm.istack.add vm[x].int
      inc vm.i
    
    of functionCall(functionCall: poi(poi: @x)):
      makeSureVmHasFunction poi
      if vm.istack.len == 0:
        raise newMagasmError(rangeError, vm.i)
      vm[x] = vm.istack.pop
      inc vm.i


    # todo: of functionCall(ech(@s)):
    of functionCall(functionCall: ech(ech: @s)):
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
          r.add $vm[v]
          i += n
        
        if st and alph(1) and en(2):
          ad(2, s[i+1])
        elif (reg2 in vm.flags) and st and alph(1) and alph(2) and en(3):
          ad(3, [s[i+1], s[i+2]])
        elif (reg4 in vm.flags) and st and alph(1) and alph(2) and alph(3) and alph(4) and en(5):
          ad(5, [s[i+1], s[i+2], s[i+3], s[i+4]])
        else:
          r.add s[i]
        
        inc i

      result = StepResult(kind: echo, echo: r)
      inc vm.i
    
    of functionCall(functionCall: custom(custom: (@name, @args))):
      try:
        vm.customFunctions[name](vm, args)
      except KeyError:
        raise newMagasmError(invalidFunction, vm.i)


    of functionCall(functionCall: (kind: @x)):
      raise newMagasmError(invalidFunction, vm.i)
  

    of advancedCall(advancedCall: (label: @l, args: @args)):
      makeSureVmHasFunction cal
      if autoFrames in vm.flags:
        vm.puf(args.mapit(it.arg))
      for (a, v) in args:
        vm[a] = vm[v]
      if vm.istack.len >= vm.recursionLimit:
        raise newMagasmError(recursionLimit, vm.i)
      vm.istack.add vm.i
      vm.i = vm.findLabel(l) + 1

  except MagasmError:
    inc vm.i
    if errorHandling notin vm.flags:
      return StepResult(kind: error, error: ((ref MagasmError)getCurrentException())[])

    elif vm.err.isSome:
      result = StepResult(kind: error, error: vm.err.get.e)
      vm.err = some (((ref MagasmError)getCurrentException())[], 0)
    
    else:
      vm.err = some (((ref MagasmError)getCurrentException())[], 0)


proc parseMagasm*(
  code: string,
  flags: set[MagasmFlag],
  unaryOperators: Table[string, string],
  postfixUnaryOperators: Table[string, string],
  binaryOperators: Table[string, tuple[f: string, reversed: bool]]
): MagasmCode =
  type
    TokenKind = enum
      eol
      colon
      semicolon
      num
      str
      word
      op

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
  
  proc tokenize(code: string): seq[Token] =
    let code = code.toRunes
    var i = 0
    var line = 0
    var isString: bool
    
    proc peek(delta=0): Rune =
      if i + delta < code.len:
        code[i + delta]
      else:
        "\0".runeAt(0)
    
    proc next(delta=0): Rune =
      for i in i .. min(i+delta, code.high):
        if code[i] == "\n".runeAt(0):
          inc line
          isString = false
      i += delta
      defer: inc i
      if i < code.len:
        code[i]
      else:
        "\0".runeAt(0)
    
    proc skip(n=1) =
      for i in i ..< min(i+n, code.len):
        if code[i] == "\n".runeAt(0):
          inc line
          isString = false
      i += n

    while i < code.len:
      let c = peek()

      template parseNumber =
        var r = ""
        while true:
          let x = peek()
          if x.size != 1 or not(($x)[0] in '0'..'9' or (floatNumbers in flags and x == ".".runeAt(0)) or (negativeNumbers in flags and x == "-".runeAt(0))):
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
          if not(x.isAlpha) and x != "_".runeAt(0):
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
          if x.size != 1 or ($x)[0] notin "+-*/!=$%&^|<>()[]{}":
            break
          r.add x
          skip()
        result.add Token(kind: op, op: r)
      
      template parseString =
        var r = ""
        while true:
          let x = next()
          if x == "\n".runeAt(0): break
          r.add x
        result.add Token(kind: str, str: r)
        result.add Token(kind: eol)
      
      
      if isString:
        parseString

      elif c.size == 1 and ($c)[0] in '0'..'9':
        parseNumber

      elif c.isAlpha or c == "_".runeAt(0):
        parseWord

      elif (negativeNumbers in flags) and c.size == 1 and ($c)[0] == '-' and (peek(1).size == 1 and ($peek(1))[0] in '0'..'9'):
        parseNumber
      elif c.size == 1 and ($c)[0] in "+-*/!=$%&^|<>()[]{}":
        parseOp

      elif c == ";".runeAt(0):
        result.add Token(kind: semicolon)
        while next() != "\n".runeAt(0): discard
        result.add Token(kind: eol)

      elif c in ["\n".runeAt(0), "\0".runeAt(0)]:
        result.add Token(kind: eol)
        inc line
        isString = false
        inc i

      elif c == ":".runeAt(0):
        result.add Token(kind: colon)
        inc i

      elif not c.isWhiteSpace:
        raise newMagasmError(parseError, line)

      else:
        inc i

  proc parse(tokens: seq[Token]): seq[StatementCe] =
    var i = 0
    var line = 0
    
    proc peek(delta=0): Token =
      if i + delta < tokens.len:
        tokens[i + delta]
      else: Token(kind: eol)
    
    proc toRegister(x: Token): Register =
      case x
      of word(word: @a is (len: 1)):
        Register(size: r16, reg: a[0])
      of word(word: @a is (len: 2)):
        if reg2 notin flags: raise newMagasmError(parseError, line)
        Register(size: r32, reg2: [a[0], a[1]])
      of word(word: @a is (len: 4)):
        if reg4 notin flags: raise newMagasmError(parseError, line)
        Register(size: r64, reg4: [a[0], a[1], a[2], a[3]])
      else: raise newMagasmError(parseError, line)
    
    template newline(semicolonReq: untyped = true) =
      if not(
        peek().kind == semicolon and peek(1).kind == eol or
        (semicolonsReqired notin flags or not semicolonReq) and peek().kind == eol
      ):
        raise newMagasmError(parseError, line)
      if peek().kind == semicolon and peek(1).kind == eol:
        inc i, 2
      else:
        inc i
      inc line
      result.add r
      continue
    
    proc toIntOrReg(x: Token): IntOrReg =
      if x.kind == num:
        IntOrReg(isReg: false, i: x.num)
      else:
        IntOrReg(isReg: true, reg: x.toRegister)

    proc parseMathExpression(): IntOrReg =
      result = peek().toIntOrReg
      inc i
    
    while i < tokens.len:
      var r: StatementCe

      if (let x = peek(); x.kind == op and x.op in ["+", "-", "*"]):
        case x.op
        of "+": r.ce = true
        of "-": r.ce = false
        of "*": r.ce = error
        inc i
      
      if (let x = peek(); x.kind == word and peek(1).kind == colon):
        r.statement = Statement(kind: label, label: x.word)
        inc i, 2
        newline false
      
      if (advancedCallOperator in flags) and (let (a, o) = (peek(0), peek(1)); a.kind == word and o.kind == op and o.op in ["(", "()"]):
        var args: seq[tuple[arg: Register, val: IntOrReg]]
        if o.op == "(":
          inc i, 2
          while true:
            let a = peek(0)
            if a.kind == op and a.op == ")":
              inc i
              break
            let eq = peek(1)
            if eq.kind != op or eq.op != "=": raise newMagasmError(parseError, line)
            inc i, 2
            args.add (a.toRegister, parseMathExpression())
        r.statement = Statement(kind: advancedCall, advancedCall: (a.word, args))
        newline

      if (let (a, o, b) = (peek(0), peek(1), peek(2)); o.kind == op and b.kind notin {eol, colon, semicolon, op}):
        if o.op in binaryOperators:
          try:
            let (op, rev) = binaryOperators[o.op]
            if rev:
              r.statement = parse(@[Token(kind: word, word: op), b, a])[0].statement
            else:
              r.statement = parse(@[Token(kind: word, word: op), a, b])[0].statement
          except:
            raise newMagasmError(parseError, line)
        else:
          raise newMagasmError(parseError, line)
        
        inc i, 3
        newline

      if (let (a, o) = (peek(0), peek(1)); o.kind == op):
        if o.op in postfixUnaryOperators:
          try:
            let op = postfixUnaryOperators[o.op]
            r.statement = parse(@[Token(kind: word, word: op), a])[0].statement
          except:
            raise newMagasmError(parseError, line)
        else:
          raise newMagasmError(parseError, line)
        
        inc i, 2
        newline

      if (let (o, a) = (peek(0), peek(1)); o.kind == op):
        if o.op in unaryOperators:
          try:
            let op = unaryOperators[o.op]
            r.statement = parse(@[Token(kind: word, word: op), a])[0].statement
          except:
            raise newMagasmError(parseError, line)
        else:
          raise newMagasmError(parseError, line)
        
        inc i, 2
        newline

      if (let x = peek(); x.kind == word):
        inc i
        block a:
          if x.word == "ech":
            if peek().kind != str: raise newMagasmError(parseError, line)
            r.statement = Statement(kind: functionCall, functionCall: FunctionCall(kind: ech, ech: peek().str))
            inc i
          else:
            proc replace(x: NimNode, a: NimNode, b: NimNode): NimNode =
              if x == a: return b
              result = copyNimNode x
              for x in x:
                result.add x.replace(a, b)

            macro handle(w: string, r: FunctionCall, body, bodyElse: untyped) =
              buildAst caseStmt:
                w
                for x in Function.nop..<Function.ech:
                  ofBranch newLit $x:
                    whenStmt:
                      elifBranch:
                        call bindSym"compiles":
                          dotExpr(r, ident $x)
                        stmtList:
                          body
                            .replace(ident"V", buildAst(call(bindSym"typeof", dotExpr(r, ident $x))))
                            .replace(ident"fnc", ident $x)
                Else:
                  bodyElse

            proc read(t: type, line: int): t =
              when t is string:
                if peek().kind != word: raise newMagasmError(parseError, line)
                result = peek().word
                inc i
              elif t is Register:
                if peek().kind != word: raise newMagasmError(parseError, line)
                result = peek().toRegister
                inc i
              elif t is IntOrReg:
                if peek().kind != word and peek().kind != num: raise newMagasmError(parseError, line)
                result = peek().toIntOrReg
                inc i
              elif t is tuple:
                for x in result.fields:
                  x = read(typeof(x), line)
              elif t is seq:
                while peek().kind != eol:
                  result.add read(typeof(result[0]), line)

            handle x.word, r.statement.functionCall:
              r.statement = Statement(kind: functionCall, functionCall: FunctionCall(kind: fnc, fnc: read(V, line)))
            do:
              var args: seq[IntOrReg]
              while peek().kind notin {semicolon, eol}:
                args.add read(IntOrReg, line)
              r.statement = Statement(kind: functionCall, functionCall: FunctionCall(kind: custom, custom: (x.word, args)))
        newline
      
      result.add r
      while peek().kind != eol: inc i
      inc line
      inc i
  
  code.tokenize.parse


proc instance*(vm: MagasmVm): MagasmVmInstance =
  for x in vm.memory:
    result.memory[x] = 0
  result.ports = vm.ports
  result.functions = vm.functions
  result.customFunctions = vm.customFunctions
  result.code = vm.code
  result.flags = vm.flags
  result.recursionLimit = vm.recursionLimit


when isMainModule:
  import terminal

  let flags = {reg2, reg4, negativeNumbers, floatNumbers, autoRoundFloats, advancedCallOperator, autoFrames}
  var vm = MagasmVm(
    flags: flags,
    functions: block:
      var x: set[Function.add..Function.poi]
      for f in Function.add..Function.poi:
        x.incl f
      x,
    customFunctions: {
      "mut": proc(vm: var MagasmVmInstance, args: seq[IntOrReg]) {.closure.} =
        defer: inc vm.i
        if args.len < 1: return
        let i = vm[args[0]]
        if i notin 0..vm.code.high: return
        vm.code[i] = StatementCe(statement: Statement(kind: functionCall, functionCall: FunctionCall(kind: slp, slp: IntOrReg(isReg: false, i: 1))))
    }.toTable,
    ports: {
      'A': (
        read: proc: int64 {.closure.} =
          stdin.readLine.parseInt,
        write: proc(v: int64) {.closure.} =
          echo $v
      ),
    }.toTable,
    memory: {'a', 'b'},
    recursionLimit: 50,
    code: parseMagasm(
      flags=flags,
      code="""
        factorial(a=5)
        A = b
        slp 1

        factorial:  ; args: a (x), result: b (factorial of x)
          b = 1
        factorial_cycle:
          a <= 1
        + ret
          b *= a
          a -= 1
          jmp factorial_cycle
      """,
      unaryOperators={
        "-=": "neg",
        "->": "jmp",
      }.toTable,
      postfixUnaryOperators={
        "()": "cal",
        "!": "mut",
      }.toTable,
      binaryOperators={
        "=": ("mov", system.true),
        "+=": ("add", system.false),
        "-=": ("sub", system.false),
        "*=": ("mul", system.false),
        "/=": ("div", system.false),
        "%=": ("mod", system.false),
        "^=": ("pow", system.false),
        ")=": ("rot", system.false),
        "==": ("ceq", system.false),
        "!=": ("cne", system.false),
        "<": ("clt", system.false),
        ">": ("cgt", system.false),
        "<=": ("cle", system.false),
        ">=": ("cge", system.false),
      }.toTable,
    )
  ).instance
  stdout.styledWrite(fgBlue, $vm.code & "\n\n")
  block a:
    for _ in 1..100:
      let r = vm.step
      case r
      of ok(): discard
      of sleep(): break a
      else: echo r
  stdout.styledWrite(fgBlue, "\n" & $vm.memory & "\n")
