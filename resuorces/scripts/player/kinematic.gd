extends KinematicBody2D

const walkForce = 1000
const stopForce = 2000
const maxWalkSpeed = 300
const jumpSpeed = 300

var velocity = Vector2()
var horizontalDirection = 1

onready var gravity = ProjectSettings.get_setting("physics/2d/default_gravity")


func onSlipperyFloor():
	return get_floor_angle(Vector2.UP) > (PI/6)


func _process(_delta):
	if horizontalDirection > 0:
		$visual/sprite.flip_h = true
	elif horizontalDirection < 0:
		$visual/sprite.flip_h = false


func _physics_process(delta):
	var target = walkForce * (Input.get_action_strength("move_right") - Input.get_action_strength("move_left"))
	
	if target > 0:
		horizontalDirection = 1
	elif target < 0:
		horizontalDirection = -1
	
	if abs(target) < walkForce * 0.2:
		velocity.x = move_toward(velocity.x, 0, stopForce * delta)
	else:
		velocity.x += target * delta

	velocity.x = clamp(velocity.x, -maxWalkSpeed, maxWalkSpeed)
	velocity.y += gravity * delta

	if is_on_floor() and Input.is_action_just_pressed("jump"):
		velocity.y = -jumpSpeed

	velocity = move_and_slide_with_snap(velocity, Vector2.DOWN, Vector2.UP, not onSlipperyFloor())
