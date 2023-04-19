import vmath

{.experimental: "overloadableEnums".}

type
  MagicParticle* = object
    pos*: Vec2
      ## in м
    mass*: float32
      ## in 1e-15 кг
    velocity*: Vec2
      ## in м/с
    rate*: float32
      ## in Гц
  
  MagicFieldKind* = enum
    particleGravity
      ## magic particles get mutual attraction
    fieldGravity
      ## magic particles get gravity to field center, field loose energy when it changes impulse sum
    directedMovement
      ## magic particles forces on direction, perpendicular to magic field source's rotation, field loose energy
    convertion
      ## magic particles converts to... something
    rotation
      ## forces magic particles to change their rate, field loose energy when it changes impulse sum
    communication
      ## todo

  MagicField* = object
    pos*: Vec2
      ## in м
    rate*: float32
      ## in Гц
    normal*: Vec2
      ## (perpendicular)
      ## angle vector
      ## should always be normalized
    force*: float32
      ##// in Н
      ## can be negative

    case kind*: MagicFieldKind
    of convertion:
      ## todo
    else: discard

proc actVelocity*(field: MagicField, particles: seq[MagicParticle], velocityChange: var seq[Vec2]): float32 =
  ## returns: energyRequired: float32
  case field.kind
  of particleGravity:
    for i, p1 in particles:
      for p2 in particles:
        velocityChange[i] += (p2.pos - p1.pos).normalize * field.force / (p2.pos - p1.pos).lengthSq / p1.mass
  
  of fieldGravity:
    var energy: Vec2
    for i, p in particles:
      let impulse = (field.pos - p.pos).normalize * field.force / (field.pos - p.pos).lengthSq
      velocityChange[i] += impulse / p.mass
      energy += impulse * velocityChange[i] / 2
    result = energy.length
  
  of directedMovement:
    var energy: Vec2
    for i, p in particles:
      let impulse = field.normal * field.force / (field.pos - p.pos).lengthSq
      velocityChange[i] += impulse / p.mass
      energy += impulse * velocityChange[i] / 2
    result = energy.length
  
  else: discard
