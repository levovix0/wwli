import vmath, bumpy

type
  Poly* = ref object
    pos*: Vec2
      ## in м
    points*: seq[Vec2]
      ## in м
    s*, t*, kgps*, tpkg*, endurance*: float
    connected*: seq[Poly]


