import pkg/[prelude]
import std/unittest

#[ func `a=`(self: var A, value: PackedSet[int]) =
  dbg "Assigning `value` to `self`"
  packedsets.assign(self.a, value) ]#
when isMainModule:
  type A = object
    a, c: int
    b*: int

  test "a":
    let info = getFieldInfo(A, b)
    check info.T is int
    check info.isExported
  import macros
  dumpTree:
    A(b: 8)

#[   func initA(): A =
    result = A()
    when compiles(A(a: default(tnewTree(nnkAccQuoted, ident"=destroy")ypeof(result.a)))):
      dbg "A.a can be set in an object constructor def."
      # TODO: add `a` to constructor params
    elif compiles(`a=`(result, default(typeof(result.a)))):
      dbg "Using `a=` for assigning the initializer value."
      `a=`(result, default(typeof(result.a)))
      # TODO: add `a` to assignment params
    elif compiles(assign(result.a, default(typeof(result.a)))):
      dbg "Using `assign` for deep copying the initializer value."
      assign(result.a, default(typeof(result.a)))
      # TODO: add `a` to `assign` params
    elif compiles(for _ in result.a: break):
      dbg "Using `incl` to copy each element of the value iterator."
      for v in value:
        incl(result.a, v) ]#
  #let a = initA()
#var aa = A(a:{0})
#[ with aa:
  with a:
    assign(toPackedSet([0, 1])) ]#