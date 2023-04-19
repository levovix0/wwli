{.used.}
import magic, vmath
import godot, godotapi/[engine, node_2d, packed_scene, resource_loader]
import seqprop

{.experimental: "overloadableEnums".}

gdobj MagicView of Node2D:
  var magicParticlePrefab: PackedScene
  var particles: ref SeqProp[MagicParticle]
  var subscriptionProcs: tuple[
    added: proc(x: var SeqProp[MagicParticle]),
    removed: proc(x: var SeqProp[MagicParticle], i: int)
  ]
  var pxpm*: float

  method ready* =
    self.setProcess(true)
    self.magicParticlePrefab = load("res://resuorces/scenes/magic-particle.tscn").PackedScene
    self.subscriptionProcs = (
      added: proc(x: var SeqProp[MagicParticle]) {.closure.} =
        self.addChild self.magicParticlePrefab.instance
      ,
      removed: proc(x: var SeqProp[MagicParticle], i: int) {.closure.} =
        let x = self.getChild(i)
        self.removeChild x
        x.deinit
        if i != self.getChildCount-1:
          self.moveChild self.getChild(x.getChildCount-1), i
    )
  
  method physicsProcess*(delta: float) =
    for i in 0..<self.getChildCount:
      let x = self.getChild(i).Node2D
      x.position = godot.vec2(self.particles.data[i].pos.x, self.particles.data[i].pos.y) * self.pxpm


proc unsubscribeFromParticles(self: MagicView) =
  if self.particles == nil: return

proc setParticles*(self: MagicView, particles: ref SeqProp[MagicParticle]) =
  if self.particles != particles:
    unsubscribeFromParticles self
    self.particles = particles
  particles.onItemAdded.add self.subscriptionProcs.added
  particles.beforeItemRemoved.add self.subscriptionProcs.removed
