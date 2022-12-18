import tables, unicode, strutils, options, macros, math
import hmatching, fusion/astdsl

{.experimental: "overloadableEnums".}
{.experimental: "caseStmtMacros".}
{.experimental: "callOperator".}

type
  StatementKind* = enum
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
    jmp  ## "jump", goto label
    slp  ## "sleep", to end execution

    add  ## "add", add second value to first register
    sub  ## "sub", a -= b
    mul  ## "mul", a *= b
    `div`  ## "div", a /= b
    `mod`  ## "mod", a %= b
    pow  ## "pow", a ^= b
    rot  ## "root", a = pow(a, 1/b)

    jmo  ## "jump offset", jump by X lines
    jlo  ## "jump label offset", jump to label with offset

    cal  ## "call", pushes instruction pointer and jumps to label
    ret  ## "return", pops instruction pointer
    
    sce  ## "set conditional execution", set ce flag to mask (1|0 + 2|0 + 4|0)
    
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

    of jmo:
      jmo*: IntOrReg
    of jlo:
      jlo*: tuple[l: string, o: IntOrReg]

    of cal:
      cal*: string
    of ret:
      ret*: tuple[]

    of sce:
      sce*: IntOrReg

    of ech:
      ech*: string
    else:
      args*: seq[IntOrReg]

  Statement* = object
    case kind*: StatementKind
    of none: discard
    of label:
      label*: string
    of functionCall:
      functionCall*: FunctionCall

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
    basicOperators
    advancedOperators
    autoRoundFloats
    divisionByZeroError
    customOperators  ## todo
    customFunctions  ## todo

  MagasmVm* = object
    memory*: seq[char]
    ports*: Table[char, tuple[read: proc: int64, write: proc(v: int64)]]
    functions*: set[Function]
    code*: MagasmCode
    flags*: set[MagasmFlag]
    recursionLimit: int

  MagasmVmInstance* = object
    memory*: Table[char, int16]
    
    ports*: Table[char, tuple[read: proc: int64, write: proc(v: int64)]]
    functions*: set[Function.add..Function.sce]
    code*: MagasmCode
    flags*: set[MagasmFlag]
    recursionLimit: int

    i*: int  ## instruction pointer
    istack*: seq[int]  ## instruction pointer stack
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


proc step*(vm: var MagasmVmInstance): StepResult =
  proc `[]`(vm: MagasmVmInstance, reg: char): int64 =
    try:
      if reg.Rune.isLower:
        cast[int64](vm.memory[reg])
      else:
        vm.ports[reg].read()

    except KeyError:
      raise newMagasmError(invalidAddress, vm.i, getCurrentException())

  proc `[]`(vm: MagasmVmInstance, reg: array[2, char]): int64 =
    let
      a = vm[reg[0]]
      b = vm[reg[1]]
    if b < 0:  # negative number
      cast[int64]([cast[int16](a), cast[int16](b), -1, -1])
    else:
      cast[int64]([cast[int16](a), cast[int16](b), 0, 0])

  proc `[]`(vm: MagasmVmInstance, reg: array[4, char]): int64 =
    let
      a = vm[reg[0]]
      b = vm[reg[1]]
      c = vm[reg[2]]
      d = vm[reg[3]]
    cast[int64]([cast[int16](a), cast[int16](b), cast[int16](c), cast[int16](d)])


  proc `[]`(vm: MagasmVmInstance, x: Register): int64 =
    case x.size
    of r16: vm[x.reg]
    of r32: vm[x.reg2]
    of r64: vm[x.reg4]

  proc `[]`(vm: MagasmVmInstance, x: IntOrReg): int64 =
    if x.isReg == true: vm[x.reg]
    else: x.i
  

  proc `[]=`(vm: var MagasmVmInstance, reg: char, v: int64) =
    try:
      if reg.Rune.isLower:
        vm.memory[reg] = cast[int16](v and 0xffff)
      else:
        vm.ports[reg].write(v)

    except KeyError:
      raise newMagasmError(invalidAddress, vm.i, getCurrentException())

  proc `[]=`(vm: var MagasmVmInstance, reg: array[2, char], v: int64) =
    for i, c in reg:
      vm[c] = v shr (i * 16)

  proc `[]=`(vm: var MagasmVmInstance, reg: array[4, char], v: int64) =
    for i, c in reg:
      vm[c] = v shr (i * 16)


  proc `[]=`(vm: var MagasmVmInstance, x: Register, v: int64) =
    case x.size
    of r16: vm[x.reg] = v
    of r32: vm[x.reg2] = v
    of r64: vm[x.reg4] = v


  proc findLabel(vm: MagasmVmInstance, l: string): int =
    for j, x in vm.code:
      if x.ce != no:
        if x.ce notin vm.ce: continue
      if x.statement.kind == label and x.statement.label == l:
        return j
    raise newMagasmError(invalidAddress, vm.i)


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


    of functionCall(functionCall: jmo(jmo: @o)):
      makeSureVmHasFunction jmo
      vm.i = vm.i + vm[o].int

    of functionCall(functionCall: jlo(jlo: (@l, @o))):
      makeSureVmHasFunction jlo
      vm.i = vm.findLabel(l) + vm[o].int


    of functionCall(functionCall: cal(cal: @l)):
      makeSureVmHasFunction cal
      if vm.istack.len >= vm.recursionLimit:
        raise newMagasmError(recursionLimit, vm.i)
      vm.istack.add vm.i
      vm.i = vm.findLabel(l) + 1
    
    of functionCall(functionCall: ret()):
      makeSureVmHasFunction ret
      if vm.istack.len == 0:
        raise newMagasmError(rangeError, vm.i)
      vm.i = vm.istack.pop + 1


    of functionCall(functionCall: sce(sce: @x)):
      makeSureVmHasFunction sce
      let mask = vm[x]
      vm.ce = {}
      if ((mask and 0x1) == 0x1): vm.ce.incl true
      if ((mask and 0x2) == 0x2): vm.ce.incl false
      if ((mask and 0x4) == 0x4): vm.ce.incl error
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

    of functionCall(functionCall: (kind: @x)):
      raise newMagasmError(invalidFunction, vm.i)

  except MagasmError:
    inc vm.i
    if errorHandling notin vm.flags:
      return StepResult(kind: error, error: ((ref MagasmError)getCurrentException())[])

    elif vm.err.isSome:
      result = StepResult(kind: error, error: vm.err.get.e)
      vm.err = some (((ref MagasmError)getCurrentException())[], 0)
    
    else:
      vm.err = some (((ref MagasmError)getCurrentException())[], 0)


proc parseMagasm*(code: string, flags: set[MagasmFlag]): MagasmCode =
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
          if not(x in '0'..'9' or (floatNumbers in flags and x == '.') or (negativeNumbers in flags and x == '-')):
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

      if (let x = peek(); x.kind == word and x.word.len == 3):
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

            handle x.word, r.statement.functionCall:
              r.statement = Statement(kind: functionCall, functionCall: FunctionCall(kind: fnc, fnc: read(V, line)))
            do:
              ## todo: custom functions
        newline
      
      if (
        let
          a = peek(0)
          o = peek(1)
          b = peek(2)
        o.kind == op and
        ((a.kind == word and a.word.len in [1, 2, 4]) or a.kind == num) and
        ((b.kind == word and b.word.len in [1, 2, 4]) or b.kind == num)
      ):
        if basicOperators in flags and o.op in ["="]:
          case o.op
          of "=":
            if a.kind == num:
              raise newMagasmError(parseError, line)
            r.statement = Statement(kind: functionCall, functionCall: FunctionCall(kind: mov, mov: (b.toIntOrReg, a.toRegister)))
          # insert new binary operators here
        else:
          raise newMagasmError(parseError, line)
        
        inc i, 3
        newline
      
      if (
        let
          o = peek(1)
          a = peek(2)
        o.kind == op and
        ((a.kind == word and a.word.len in [1, 2, 4]) or a.kind == num)
      ):
        if basicOperators in flags and o.op in [""]:
          case o.op
          # insert new unary operators here
          else: discard
        else:
          raise newMagasmError(parseError, line)
        
        inc i, 2
        newline
      
      if advancedOperators in flags and peek().kind == op and peek().op == "->" and peek(1).kind == word:
        r.statement = Statement(kind: functionCall, functionCall: FunctionCall(kind: jmp, jmp: peek(1).word))
        inc i, 2
        newline

      result.add r
      while peek().kind != eol: inc i
      inc line
      inc i
  
  code.tokenize.parse


when isMainModule:
  let flags = {reg2, reg4, negativeNumbers, floatNumbers, advancedOperators, basicOperators, autoRoundFloats}
  var vm = MagasmVmInstance(
    flags: flags,
    functions: block:
      var x: set[Function.add..Function.sce]
      for f in Function.add..Function.sce:
        x.incl f
      x,
    ports: {
      'A': (
        read: proc: int64 {.closure.} =
          stdin.readLine.parseInt,
        write: proc(v: int64) {.closure.} =
          echo $v
      ),
    }.toTable,
    memory: {
      'a': 0'i16,
      'b': 0'i16,
      'c': 0'i16,
    }.toTable,
    recursionLimit: 50,
    code: parseMagasm(flags=flags, code="""
      ab = AA
      c = a
      add c b
      cal out
      sub c b
      cal out
      mul c b
      cal out
      div c b
      cal out
      mod c b
      cal out
      pow c b
      cal out
      rot c b
      cal out
      slp 1
    out:
      A = c
      c = a
      ret
    """)
  )
  echo vm.code
  block a:
    for _ in 1..100:
      let r = vm.step
      case r
      of ok(): discard
      of sleep(): break a
      else: echo r
  echo vm.memory
