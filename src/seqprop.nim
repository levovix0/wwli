
type SeqProp*[T] = object
  data*: seq[T]
  onItemAdded*: seq[proc(v: var SeqProp[T])]
    ## note item will be last item
  beforeItemRemoved*: seq[proc(v: var SeqProp[T], i: int)]
    ## note item will be replaced with last item

proc add*[T](x: var SeqProp[T], v: T) =
  x.data.add v
  for f in x.onItemAdded:
    f(x)

proc del*[T](x: var SeqProp[T], i: int) =
  for f in x.beforeItemRemoved:
    f(x, i)
  x.data.del i
