let ( %> ) f g x = g (f x)
let ( <% ) f g x = f (g x)
let flip f x y = f y x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y
external identity : 'a -> 'a = "%identity"
