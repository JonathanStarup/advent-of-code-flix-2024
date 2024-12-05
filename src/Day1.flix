mod Day1 {
    import java.lang.System

    use FromString.fromString
    use Functor.map
    use Util.Func.>|>

    pub def solve1(l1: List[Int32], l2: List[Int32]): Int32 = {
        use Int32.abs;
        use List.{sort, sum, zip};

        (sort(l1), sort(l2))
        ||> zip
        |> map(match (x, y) -> abs(x - y))
        |> sum
    }

    pub def solve2(l1: List[Int32], l2: List[Int32]): Int32 = {
        use List.frequency;
        use Map.{foldWithKey, getWithDefault};

        let m1 = frequency(l1);
        let m2 = frequency(l2);
        (0, m1) ||> foldWithKey(
            (sum, id, freq) -> sum + id * getWithDefault(id, 0, m2) * freq
        )
    }

    pub def parse(lines: List[String]): (List[Int32], List[Int32]) \ Abort = {
        use List.unzip;
        use Regex.split;
        use Util.List.take2;
        use Util.Option.get;
        use Util.Tuple.app2;

        let parseLine =
            split(regex = regex" +") >> take2 >> app2(fromString >> get);

        lines |> map(parseLine) |> unzip
    }

    pub def solve1FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day1.txt")
            |> parse
            ||> solve1
        })

    @Test
    def assertPart1(): Bool \ IO =
        solve1FromFile() == 1765812

    pub def solve2FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day1.txt")
            |> parse
            ||> solve2
        })

    @Test
    pub def assertPart2(): Bool \ IO =
        solve2FromFile() == 20520794

}
