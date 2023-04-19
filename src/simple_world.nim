{.used.}
import strutils, tables, hmatching
import vmath
import godot, godotapi/[engine, node_2d, input]
import seqprop
import magic, magic_asm
import magic_view

{.experimental: "overloadableEnums".}
{.experimental: "caseStmtMacros".}

const pxpm = 100


gdobj SimpleWorld of Node2D:
  var magicView: MagicView
  var magicParticles: ref SeqProp[MagicParticle]
  var vm: MagasmVmInstance
  var velocityChange: seq[Vec2]
  var player: Node2D
  var fJustPressed: bool
  var deltaTime: float


  proc addParticle*(pos: vmath.Vec2) =
    {.cast(gcsafe).}:
      self.magicParticles[].add MagicParticle(pos: pos, mass: 1)
    self.velocityChange.add vmath.vec2()


  proc updateCode*(s: string) {.gdExport.} =
    try:
      let flags = {reg2, reg4, negativeNumbers, floatNumbers, autoRoundFloats, advancedCallOperator, autoFrames, mathExpressions}
      self.vm = MagasmVm(
        flags: flags,
        functions: block:
          var x: set[Function.add..Function.poi]
          for f in Function.add..Function.poi:
            x.incl f
          x,
        customFunctions: {
          "mut": (proc(vm: var MagasmVmInstance, args: seq[magic_asm.Expression]) {.closure.} =
            defer: inc vm.i
            if args.len < 1: return
            let i = vm[args[0]]
            if i notin 0..vm.code.high: return
            vm.code[i] = StatementCe(statement: Statement(
              kind: functionCall,
              functionCall: FunctionCall(kind: slp, slp: magic_asm.Expression(kind: intOrReg, intOrReg: IntOrReg(isReg: irFalse, i: 1)))
            ))
          ),
        }.toTable,
        ports: {
          'A': (  # up force (float)
            read: proc: int64 {.closure.} =
              0
            ,
            write: proc(v: int64) {.closure.} =
              discard actVelocity(
                MagicField(
                  pos: vmath.vec2(self.player.position.x, self.player.position.y) / pxpm,
                  normal: vmath.vec2(0, -1).normalize,
                  force: cast[float64](v),
                  kind: directedMovement,
                ),
                self.magicParticles.data,
                self.velocityChange,
              )
          ),
          'B': (  # right force (float)
            read: proc: int64 {.closure.} =
              0
            ,
            write: proc(v: int64) {.closure.} =
              discard actVelocity(
                MagicField(
                  pos: vmath.vec2(self.player.position.x, self.player.position.y) / pxpm,
                  normal: vmath.vec2(1, 0).normalize,
                  force: cast[float64](v),
                  kind: directedMovement,
                ),
                self.magicParticles.data,
                self.velocityChange,
              )
          ),
          'C': (  # particle creation and 'f' key read
            read: proc: int64 {.closure.} =
              result = 0
              if self.fJustPressed:
                result = 1
                self.fJustPressed = false
            ,
            write: proc(v: int64) {.closure.} =
              self.addParticle(vmath.vec2(self.player.position.x, self.player.position.y) / pxpm + vmath.vec2(cast[float64](v), 0))
          ),
          'D': (  # delta time (float)
            read: proc: int64 {.closure.} =
              cast[int64](self.deltaTime.float64)
            ,
            write: proc(v: int64) {.closure.} =
              discard
          ),
        }.toTable,
        memory: {'a', 'b', 'c', 'd', 'e', 'f', 'h', 'i', 'x', 'y', 'z', 'u'},
        recursionLimit: 50,
        code: parseMagasm(
          flags=flags,
          code=s,
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
    except MagasmError as x:
      if s == "":
        print "magasm error: " & $x[]
        raise
      else:
        self.updateCode("")


  method ready* =
    self.setProcess(true)
    self.magicView = self.getChild(self.getChildCount-1).MagicView
    self.player = self.getChild(0).Node2D
    new self.magicParticles
    self.magicView.setParticles(self.magicParticles)
    self.magicView.pxpm = pxpm
    
    self.updateCode """
        abcd = 20.0
        fmul abcd D
        fadd efhi abcd
        abcd = efhi
        fmul abcd D
        fadd xyzu abcd

        C == 1
      - jmp createParticle_end
      createParticle:
        xyzu = 0.25
        efhi = 0.0
        C = xyzu
      createParticle_end:
      
      undoGravity:
        abcd = 9.8
        fmul abcd xyzu
        fmul abcd xyzu
        A = abcd
      
      forceRight:
        abcd = 20.0
        fmul abcd xyzu
        fmul abcd xyzu
        B = abcd

      end:
        slp 1
    """


  proc processCode(delta: float) =
    self.deltaTime = delta  # !--- set deltaTime ---! #
    block a:
      for _ in 1..100:
        {.cast(gcsafe).}:
          let r = self.vm.step
        case r
        of ok(): discard
        of sleep(): break a
        else: print r
      


  proc processMagic(delta: float) =
    for i, velocity in self.velocityChange:
      self.magicParticles.data[i].velocity += velocity * delta
      self.velocityChange[i] = vmath.vec2()

      # gravity
      self.magicParticles.data[i].velocity += vmath.vec2(0, 9.8) * delta
    
    for v in self.magicParticles.data.mitems:
      v.pos += v.velocity * delta


  method physicsProcess(delta: float) =
    self.processCode(delta)
    self.processMagic(delta)
  
  
  method process(delta: float) =
    if isActionJustPressed("f"):
      self.fJustPressed = true
