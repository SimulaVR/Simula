
extends StaticBody

var animating = false

const CLOSED = 0
const OPEN = 1

export var locked = false
var state = CLOSED

func _ready():
    # Called every time the node is added to the scene.
    # Initialization here
    pass

func interact(relate):
    if !animating:
        if state == CLOSED:
            $AnimationPlayer.play("open")
        else:
            $AnimationPlayer.play_backwards("open")

func _on_AnimationPlayer_animation_finished(anim_name):
    animating = false

    if state == OPEN:
        state = CLOSED
    else:
        state = OPEN


func _on_AnimationPlayer_animation_started(anim_name):
    animating = true
