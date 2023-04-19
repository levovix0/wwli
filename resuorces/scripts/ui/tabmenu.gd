extends Panel

const interpolatingDuration = 0.2

var mode = 0  # 0 - menu isn't open, 1 - menu is open
var isInterpolating = false
var targetMode = 0

export var targetPath: NodePath
var target


func _ready():
	target = get_node(targetPath)


func animateParams(t):
	modulate = Color(1, 1, 1, t)


func _process(delta):
	if Input.is_action_just_pressed("ui_open"):
		isInterpolating = true
		if targetMode == 0:
			targetMode = 1
			animateParams(0)
			visible = true
		else:
			targetMode = 0
	
	if isInterpolating and mode != targetMode:
		if targetMode == 1:
			mode = min(mode + delta / interpolatingDuration, 1)
		else:
			mode = max(mode - delta / interpolatingDuration, 0)
		if abs(mode - targetMode) < 0.01:
			mode = targetMode
	
	if isInterpolating:
		animateParams(mode)
	
	if isInterpolating and mode == targetMode:
		isInterpolating = false
		get_tree().paused = targetMode == 1
		if targetMode == 0:
			visible = false


func _on_TextEdit_text_changed():
	target.update_code($TextEdit.text)
