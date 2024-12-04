mod Day1 {
    import java.lang.System

    use Functor.map
    use Int32.abs
    use Int32.fromString
    use List.frequency
    use List.sort
    use List.sum
    use List.unzip
    use List.zip
    use Map.mapWithKey
    use Regex.split
    use Util.Option.get

    pub def solve1(l1: List[Int32], l2: List[Int32]): Int32 = {
        (sort(l1), sort(l2)) ||> zip |> map(match (x, y) -> abs(x - y)) |> sum
    }

    pub def solve2(l1: List[Int32], l2: List[Int32]): Int32 = {
        let m1 = frequency(l1);
        let m2 = frequency(l2);
        (0, m1) ||> Map.foldWithKey((sum, id, freq) -> sum + id * Map.getWithDefault(id, 0, m2) * freq)
    }

    pub def parse(l: List[String]): (List[Int32], List[Int32]) \ Abort = {
        l |> map(s -> {
            match split(regex = regex" +", s) {
                case one :: two :: Nil => (fromString(one) |> get, fromString(two) |> get)
                case other => Abort.abort("Expected two ints, not %{other}.")
            }
        }) |> unzip
    }

    pub def solve1FromFile(): Unit \ IO = {
        Util.Effect.runWith(Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr)(() -> {
            FileRead.readLines("./inputs/Day1.txt") |> parse ||> solve1 |> println
        })
    }

    pub def solve2FromFile(): Unit \ IO = {
        Util.Effect.runWith(Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr)(() -> {
            FileRead.readLines("./inputs/Day1.txt") |> parse ||> solve2 |> println
        })
    }

}
