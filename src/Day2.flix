mod Day2 {
    use Eq.eq
    use Foldable.count
    use FromString.fromString
    use Functor.map
    use Regex.split

    pub def solve1(reports: List[List[Int32]]): Int32 = {
        reports |> count(report -> {
            closeLevels(report) and (increasing(report) or decreasing(report))
        })
    }

    pub def closeLevels(report: List[Int32]): Bool = match report {
        case x :: y :: xs =>
            let diff = Int32.abs(x - y);
            diff >= 1 and diff <= 3 and closeLevels(y :: xs)
        case _ :: Nil => true
        case Nil => true
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
        reports |> count(report -> {
            seqSkipCheck((x, y) -> x + 1 <= y and y <= x + 3, report, canSkip = true)
            or seqSkipCheck((x, y) -> x + 1 <= y and y <= x + 3, report |> List.drop(1), canSkip = false)
            or seqSkipCheck((x, y) -> x - 3 <= y and y <= x - 1, report, canSkip = true)
            or seqSkipCheck((x, y) -> x - 3 <= y and y <= x - 1, report |> List.drop(1), canSkip = false)
        })
    }

    pub def seqSkipCheck(cond: (Int32, Int32) -> Bool \ ef, l: List[Int32], canSkip: {canSkip = Bool}): Bool \ ef = {
        match l {
            case x :: y :: xs =>
                if (cond(x, y)) seqSkipCheck(cond, y :: xs, canSkip)    
                else if (canSkip#canSkip) seqSkipCheck(cond, x :: xs, canSkip = false)
                else false
            case _ :: Nil => true
            case Nil => true
        }
    }

    pub def parse(lines: List[String]): List[List[Int32]] \ Abort = {
        use Util.Option.get;

        lines |> map(line -> {
            line |> split(regex = regex" +") |> map(fromString >> get)
        })
    }

    pub def solve1FromFile(): Unit \ IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr();
        Util.Effect.runWith(handler)(() -> {
            FileRead.readLines("./inputs/Day2.txt") |> parse |> solve1 |> println
        })
    }

    pub def solve2FromFile(): Unit \ IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr();
        Util.Effect.runWith(handler)(() -> {
            FileRead.readLines("./inputs/Day2.txt") |> parse |> solve2 |> println
        })
    }

    @Test
    pub def text01(): Bool \ IO = Util.Effect.runWith(Util.Abort.handleWithErr(false))(() -> {
        List#{"2 1 5 6"} |> parse |> solve2 |> eq(1)
    })

}
