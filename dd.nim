#[ import std/packedsets
type B* = object
  b: PackedSet[int]
func `b=`(self: var B, value: PackedSet[int]) =
  assign(self.b, value) ]#

import pkg/prelude/[alias, lib]

func getFieldInfoNodes*[T: object](self: typedesc[T]; fieldIdent: string): (NimNode, NimNode) {.compileTime.} =
  for field in getTypeImpl(self)[2]:
    if field[0].strVal == fieldIdent:
      return (field[1], newLit(field[0].isExported))

func getFieldInfoNodes*(self: NimNode; fieldIdent: string): (NimNode, NimNode) {.compileTime.} =
  getImpl(self).matchAst:
  of nnkTypeDef(_, _, nnkObjectTy(_, _, `params` @ nnkRecList)):
    for field in params:
      var ident = field[0].repr
      if ident[^1] == '*':
        if ident[0..^2] == fieldIdent:
          return (field[1], newLit true)
      else:
        if ident == fieldIdent:
          return (field[1], newLit false)

type ObjectFieldInfo*[T] = object
  t*: typedesc[T]
  x*: bool

macro getFieldInfo*[T: object](self: typedesc[T]; field: untyped): ObjectFieldInfo =
  macro inner(s, f): (NimNode, NimNode) =
    quote do: getFieldInfoNodes(`s`, `f`.strVal)
  let (t, x) = inner[T](self, field)
  quote do: ObjectFieldInfo[`t`](x: `x`)