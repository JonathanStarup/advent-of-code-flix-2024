mod Day2 {
    use Eq.eq
    use Foldable.count
    use FromString.fromString
    use Functor.map

    pub def solve1(reports: List[List[Int32]]): Int32 =
        reports |> count(report ->
            closeLevels(report) and (increasing(report) or decreasing(report))
        )

    pub def closeLevels(report: List[Int32]): Bool = match report {
        case x :: y :: xs =>
            let diff = Int32.abs(x - y);
            diff >= 1 and diff <= 3 and closeLevels(y :: xs)
        case _ :: Nil =>
            true
        case Nil =>
            true
    }

    pub def increasing(l: List[Int32]): Bool = match l {
        case x :: y :: xs => x <= y and increasing(y :: xs)
        case _ :: Nil => true
        case Nil => true
    }

    pub def decreasing(l: List[Int32]): Bool = match l {
        case x :: y :: xs => x >= y and decreasing(y :: xs)
        case _ :: Nil => true
        case Nil => true
    }

    pub def solve2(reports: List[List[Int32]]): Int32 = {
        use List.drop;

        let checkInc = seqSkipCheck((x, y) -> x + 1 <= y and y <= x + 3);
        let checkDec = seqSkipCheck((x, y) -> x - 3 <= y and y <= x - 1);
        reports |> count(report -> {
            checkInc(report, canSkip = true)
            or checkInc(report |> drop(1), canSkip = false)
            or checkDec(report, canSkip = true)
            or checkDec(report |> drop(1), canSkip = false)
        })
    }

    pub def seqSkipCheck(
        cond: (Int32, Int32) -> Bool \ ef,
        l: List[Int32],
        canSkip: {canSkip = Bool}
    ): Bool \ ef =
        match l {
            case x :: y :: xs =>
                if (cond(x, y))
                    seqSkipCheck(cond, y :: xs, canSkip)
                else if (canSkip#canSkip)
                    seqSkipCheck(cond, x :: xs, canSkip = false)
                else
                    false
            case _ :: Nil =>
                true
            case Nil =>
                true
        }

    pub def parse(lines: List[String]): List[List[Int32]] \ Abort = {
        use Regex.split;
        use Util.Option.get;

        let parseLine = split(regex = regex" +") >> map(fromString >> get);

        lines |> map(parseLine)
    }

    pub def solve1FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day2.txt")
            |> parse
            |> solve1
        })

    @Test
    pub def assertPart1(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 502, solve1FromFile())
    })

    pub def solve2FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day2.txt")
            |> parse
            |> solve2
        })

    @Test
    pub def assertPart2(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 544, solve2FromFile())
    })

}
