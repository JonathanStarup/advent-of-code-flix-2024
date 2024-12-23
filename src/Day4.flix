mod Day4 {
    use Foldable.sum
    use Foldable.toList
    use Foldable.toVector
    use Foldable.size
    use Util.Foldable.countWithIndex
    use Functor.map
    use Indexable.get

    pub type alias Vector2[t] = Vector[Vector[t]]

    def isChar(row: Int32, col: Int32, c: Char, m: Vector2[Char]): Bool \ OutOfBounds = {
        if (0 <= row and row < size(m))
            if (0 <= col and col < size(m[row]))
                m[row][col] == c
            else false
        else false
    }

    pub def solve1(m: Vector2[Char]): Int32 \ OutOfBounds = {
        use Vector.mapWithIndex;

        def checkFrom(row, col): Int32 = {
            use Util.Bool.toInt32;

            def checkDir(rowD, colD): Int32 \ OutOfBounds = toInt32(
                isChar(row+rowD, col+colD, 'M', m)
                and isChar(row+rowD*2, col+colD*2, 'A', m)
                and isChar(row+rowD*3, col+colD*3, 'S', m)
            );
            checkDir(-1, 0)
            + checkDir(-1, -1)
            + checkDir(0, -1)
            + checkDir(+1, -1)
            + checkDir(+1, 0)
            + checkDir(+1, +1)
            + checkDir(0, +1)
            + checkDir(-1, +1)
        };
        m |> mapWithIndex(row -> mapWithIndex(col -> c -> {
            if (c == 'X') checkFrom(row, col) else 0
        }) >> sum) |> sum
    }

    pub def solve2(m: Vector2[Char]): Int32 \ OutOfBounds = {
        use Vector.mapWithIndex;

        def checkFrom(row, col): Bool = {
            def checkDir(rowD, colD) = {
                isChar(row+rowD, col+colD, 'M', m)
                and isChar(row-rowD, col-colD, 'S', m)
            };
            (checkDir(+1, +1) or checkDir(-1, -1))
            and (checkDir(+1, -1) or checkDir(-1, +1))
        };
        m |> mapWithIndex(row -> countWithIndex(col -> c -> {
            c == 'A' and checkFrom(row, col)
        })) |> sum
    }

    pub def parse(l: List[String]): Vector2[Char] =
        l |> map(String.toVector) |> toVector

    pub def solve1FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day4.txt")
            |> parse
            |> solve1
        })

    @Test
    pub def assertPart1(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 2390, solve1FromFile())
    })

    pub def solve2FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day4.txt")
            |> parse
            |> solve2
        })

    @Test
    pub def assertPart2(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 1809, solve2FromFile())
    })

}
