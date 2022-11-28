{.used.}
import strutils
import godot, godotapi/[engine, sprite, packed_scene, resource_loader]

var bulletPrefab {.threadvar.}: PackedScene

gdobj Enemy of Sprite:
  method ready* =
    self.setProcess(true)
    if bulletPrefab == nil:
      bulletPrefab = load("res://scenes/enemy-bullet.tscn").PackedScene

  method onShootTimerTimeout* {.base.} =
    let x = bulletPrefab.instance.Sprite
    self.getParent.addChild x
    x.position = self.position
