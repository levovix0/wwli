extends TextEdit

func _ready():
	focus_mode = FOCUS_ALL

func _process(_delta):
	if Input.is_action_just_pressed("ui_open") and get_focus_owner() == self:
		release_focus()
		var s = text
		undo()
		# todo: check if undo contains not only tab
