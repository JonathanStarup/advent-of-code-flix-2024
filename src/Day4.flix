mod Day4 {
    use Foldable.sum
    use Foldable.toList
    use Foldable.toVector
    use Foldable.size
    use Util.Foldable.countWithIndex
    use Functor.map

    pub type alias Vector2[t] = Vector[Vector[t]]

    def isChar(row: Int32, col: Int32, c: Char, m: Vector2[Char]): Bool = {
        use Vector.get;
        if (0 <= row and row < size(m))
            if (0 <= col and col < size(get(row, m))) get(col, get(row, m)) == c
            else false
        else false
    }

    pub def solve1(m: Vector2[Char]): Int32 = {
        use Vector.{mapWithIndex, sum};

        def checkFrom(row, col): Int32 = {
            use Util.Bool.{toInt32 => i};

            def checkDir(rowD, colD): Int32 = i(
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

    pub def solve2(m: Vector2[Char]): Int32 = {
        use Vector.{get, mapWithIndex, sum};

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
    pub def assertPart1(): Bool \ IO =
        solve1FromFile() == 2390

    pub def solve2FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day4.txt")
            |> parse
            |> solve2
        })

        @Test
    pub def assertPart2(): Bool \ IO =
        solve2FromFile() == 1809

}
