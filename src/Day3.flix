mod Day3 {
    use Option.getWithDefault;
    use FromString.fromString
    use Functor.map

    pub def solve1(l: List[(Int32, Int32)]): Int32 =
        l |> map(match (x, y) -> x * y) |> List.sum

    pub def parse1(s: String): List[(Int32, Int32)] \ Abort = {
        use Regex.submatches;
        use Util.List.take2;
        use Util.Option.get;
        use Util.Tuple.app2;

        let parseLine =
            submatches(substr = regex"\\d\\d?\\d?")
            >> take2 >> app2(fromString >> get);

        s |> submatches(substr = regex"mul\\(\\d\\d?\\d?,\\d\\d?\\d?\\)")
            |> map(parseLine)
    }

    pub enum Parsed {
        case Mul(Int32, Int32)
        case Dont
        case Do
    }

    pub def solve2(do: {do = Bool}, acc: Int32, l: List[Parsed]): Int32 =
        match l {
            case Parsed.Mul(x, y) :: rest =>
                solve2(do, if (do#do) acc + x*y else acc, rest)
            case Parsed.Dont :: rest =>
                solve2(do = false, acc, rest)
            case Parsed.Do :: rest =>
                solve2(do = true, acc, rest)
            case Nil =>
                acc
        }

    pub def parse2(s: String): List[Parsed] \ Abort = {
        use Util.Option.get;
        use Regex.submatches;
        use Util.List.take2;
        use Util.Tuple.app2;
        use Util.Tuple.lift2;
        use Parsed.{Do, Dont, Mul};

        def parseInstruction(ins: String) = match ins {
            case "do()" => Do
            case "don't()" => Dont
            case other =>
                other
                |> submatches(substr = regex"\\d\\d?\\d?")
                |> take2
                |> app2(fromString >> get)
                |> lift2(Mul)
        };

        let instructionRegex =
            regex"(mul\\(\\d\\d?\\d?,\\d\\d?\\d?\\))|(don't\\(\\))|(do\\(\\))";

        s |> Regex.submatches(substr = instructionRegex)
            |> map(parseInstruction)
    }

    pub def solve1FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.read("./inputs/Day3.txt")
            |> parse1
            |> solve1
        })

    @Test
    pub def assertPart1(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 190604937, solve1FromFile())
    })

    pub def solve2FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.read("./inputs/Day3.txt")
            |> parse2
            |> solve2(do = true, 0)
        })

    @Test
    pub def assertPart2(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 82857512, solve2FromFile())
    })

}
