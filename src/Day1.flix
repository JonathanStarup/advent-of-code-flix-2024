mod Day1 {
    use Int32.abs;
    import java.lang.System

    use Regex.split
    use List.unzip
    use Int32.fromString
    use String.splitOn
    use List.sum
    use List.map
    use List.zip
    use List.sort
    use Util.Option.get

    pub def solv(l1: List[Int32], l2: List[Int32]): Int32 = {
        (sort(l1), sort(l2)) ||> zip |> map(match (x, y) -> abs(x - y)) |> sum
    }

    pub def parse(l: List[String]): (List[Int32], List[Int32]) \ Abort = {
        l |> map(s -> {
            match split(regex = regex" +", s) {
                case one :: two :: Nil => (fromString(one) |> get, fromString(two) |> get)
                case other => Abort.abort("Expected two ints, not %{other}.")
            }
        }) |> unzip
    }

    pub def solveFromFile(): Unit \ IO = (() -> {
        FileRead.readLines("./inputs/Day1.txt")
            |> parse
            ||> solv
            |> println
    }) |> Util.FileRead.handleWithIOAbort |> Util.Abort.handleWithErr |> Util.Func.apply

    pub def solveFromFile2(): Unit \ IO = {
        Util.Effect.runWith(Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr)(() -> {
            FileRead.readLines("./inputs/Day1.txt") |> parse ||> solv |> println
        })
    }

}
