mod Day1 {
    import java.lang.System

    use Functor.map

    pub def solve1(l1: List[Int32], l2: List[Int32]): Int32 = {
        use Int32.abs;
        use List.{sort, sum, zip};

        (sort(l1), sort(l2)) ||> zip |> map(match (x, y) -> abs(x - y)) |> sum
    }

    pub def solve2(l1: List[Int32], l2: List[Int32]): Int32 = {
        use List.frequency;
        use Map.{foldWithKey, getWithDefault};

        let m1 = frequency(l1);
        let m2 = frequency(l2);
        (0, m1) ||> foldWithKey((sum, id, freq) -> sum + id * getWithDefault(id, 0, m2) * freq)
    }

    pub def parse(lines: List[String]): (List[Int32], List[Int32]) \ Abort = {
        use Int32.fromString;
        use List.unzip;
        use Regex.split;
        use Util.Option.get;

        lines |> map(s -> {
            match split(regex = regex" +", s) {
                case one :: two :: Nil => (fromString(one) |> get, fromString(two) |> get)
                case other => Abort.abort("Expected two ints, not %{other}.")
            }
        }) |> unzip
    }

    pub def solve1FromFile(): Unit \ IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr();
        Util.Effect.runWith(handler)(() -> {
            FileRead.readLines("./inputs/Day1.txt") |> parse ||> solve1 |> println
        })
    }

    pub def solve2FromFile(): Unit \ IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr();
        Util.Effect.runWith(handler)(() -> {
            FileRead.readLines("./inputs/Day1.txt") |> parse ||> solve2 |> println
        })
    }

}
