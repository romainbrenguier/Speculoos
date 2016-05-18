let int s i = Expression.var s (Type.int i)
let bool s = Expression.var s Type.bool
let unit s = Expression.var s Type.unit
