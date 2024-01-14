type cell = "*" | "."
type board = cell[][]

type rowToString<
    T extends cell[],
    acc extends string = ""
> = T extends [infer Head extends cell, ...infer Tail extends cell[]]
    ? acc extends ""
      ? rowToString<Tail, `${Head}`>
      : rowToString<Tail, `${acc}${Head}`>
    : `${acc}`

type boardToString<
    T extends board,
    acc extends string = "",
> = T extends [infer Head extends cell[], ...infer Tail extends board]
    ? acc extends ""
      ? boardToString<Tail, `${rowToString<Head>}`>
      : boardToString<Tail, `${acc}\n${rowToString<Head>}\n`>
    : acc

type stringToBoard<
    s extends string, 
    accBoard extends board = [], 
    accRow extends cell[] = []
> = s extends `${infer Head}${infer Tail}`
    ? Head extends '\n'
      ? accRow['length'] extends 0
        ? stringToBoard<Tail, accBoard, []>
        : stringToBoard<Tail, [...accBoard, accRow], []>
      : Head extends cell
        ? stringToBoard<Tail, accBoard, [...accRow, Head]>
        : never
    : accBoard

type shift<T extends unknown[]> = 
    T extends [infer Head, ...infer Tail]
    ? [...Tail, Head]
    : []

type innerShift<T extends unknown[][]> =
    T extends [infer Head extends unknown[], ...infer Tail extends unknown[][]]
    ? [shift<Head>, ...innerShift<Tail>]
    : []

type unshift<T extends unknown[]> =
    T extends [...infer Head, infer Tail]
    ? [Tail, ...Head]
    : []

type innerUnshift<T extends unknown[][]> =
    T extends [infer Head extends unknown[], ...infer Tail extends unknown[][]]
    ? [unshift<Head>, ...innerUnshift<Tail>]
    : []

type cellToNum<T extends cell> =
    T extends "*"
    ? 1
    : 0

type rowToNum<T extends cell[]> =
    T extends [infer Head extends cell, ... infer Tail extends cell[]]
    ? [cellToNum<Head>, ...rowToNum<Tail>]
    : []

type boardToNum<T extends board> =
    T extends [infer Head extends cell[], ... infer Tail extends board]
    ? [rowToNum<Head>, ...boardToNum<Tail>]
    : []

type Length<T extends any[]> = 
    T['length'] extends number
    ? T['length']
    : never

type BuildTuple<L extends number, T extends any[] = []> = 
    T extends { length: L } ? T : BuildTuple<L, [...T, any]>;

type Add<A extends number, B extends number> = 
    Length<[...BuildTuple<A>, ...BuildTuple<B>]>;

type sumArr<T extends number[], U extends number[], acc extends number[] = []> =
    T extends [infer THead extends number, ...infer TTail extends number[]]
    ? U extends [infer UHead extends number, ...infer UTail extends number[]]
      ? sumArr<TTail, UTail, [...acc, Add<THead, UHead>]>
      : acc
    : acc

type sumReduce<T extends number[][], U extends number[][], acc extends number[][] = []> = 
    T extends [infer THead extends number[], ...infer TTail extends number[][]]
    ? U extends [infer UHead extends number[], ...infer UTail extends number[][]]
      ? sumReduce<TTail, UTail, [...acc, sumArr<THead, UHead>]>
      : acc
    : acc

type sumNReduce<T extends number[][][], acc extends number[][] = []> =
    T extends [infer Head extends number[][], ...infer Tail extends number[][][]]
    ? acc['length'] extends 0
      ? sumNReduce<Tail, Head>
      : sumNReduce<Tail, sumReduce<Head, acc>>
    : acc

type numToCell<T extends number, U extends cell> =
    U extends "*"
    ? T extends (3 | 4)
      ? "*"
      : "."
    : T extends 3
      ? "*"
      : "."

type numArrToCellArr<T extends number[], U extends cell[], acc extends cell[] = []> =
    T extends [infer THead extends number, ...infer TTail extends number[]]
    ? U extends [infer UHead extends cell, ...infer UTail extends cell[]]
      ? numArrToCellArr<TTail, UTail, [...acc, numToCell<THead, UHead>]>
      : "numArr failed"
    : acc

type strAble = string | number | bigint | boolean | null | undefined

type toString<T extends strAble[], acc extends string = ""> =
    T extends [infer Head extends strAble, ...infer Tail extends strAble[]]
    ? acc extends ""
      ? toString<Tail, `[${Head}`>
      : toString<Tail, `${acc}, ${Head}`>
    : `${acc}]`

type matToString<T extends strAble[][], acc extends string = ""> =
    T extends [infer Head extends strAble[], ...infer Tail extends strAble[][]]
    ? acc extends ""
      ? matToString<Tail, `[${toString<Head>}`>
      : matToString<Tail, `${acc}, ${toString<Head>}`>
    : `${acc}]`

type numArrArrToCellArrArr<T extends number[][], U extends board, acc extends board = []> =
    T extends [infer THead extends number[], ...infer TTail extends number[][]]
    ? U extends [infer UHead extends cell[], ...infer UTail extends board]
      ? numArrArrToCellArrArr<TTail, UTail, [...acc, numArrToCellArr<THead, UHead>]>
      : `numArrArr failed THead: ${toString<THead>} U: ${matToString<U>}`
    : acc
  
type nxt<T extends string> = boardToString<numArrArrToCellArrArr<sumNReduce<
    [ sumNReduce<[innerShift<shift<boardToNum<stringToBoard<T>>>>  , shift<boardToNum<stringToBoard<T>>>  , innerUnshift<shift<boardToNum<stringToBoard<T>>>>  ]>
    , sumNReduce<[innerShift<boardToNum<stringToBoard<T>>>         , boardToNum<stringToBoard<T>>         , innerUnshift<boardToNum<stringToBoard<T>>>         ]>
    , sumNReduce<[innerShift<unshift<boardToNum<stringToBoard<T>>>>, unshift<boardToNum<stringToBoard<T>>>, innerUnshift<unshift<boardToNum<stringToBoard<T>>>>]>
    ]>, stringToBoard<T>>>

type init = nxt<nxt<nxt<nxt<nxt<nxt<nxt<`
..*.......
..........
.....*....
...*.*....
....**....
..........
..........
`>>>>>>>