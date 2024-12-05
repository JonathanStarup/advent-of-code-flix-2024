mod Day3 {
    use Functor.map

    pub def solve1(l: List[(Int32, Int32)]): Int32 =
        l |> map(match (x, y) -> x * y) |> List.sum

    pub def parse1(s: String): List[(Int32, Int32)] \ Abort = {
        use Util.Option.get;
        use Int32.fromString;

        s |> Regex.submatches(substr = regex"mul\\(\\d\\d?\\d?,\\d\\d?\\d?\\)")
            |> map(Regex.submatches(substr = regex"\\d\\d?\\d?"))
            |> map(nums -> match nums {
                case one :: two :: Nil => (fromString(one) |> get, fromString(two) |> get)
                case other => Abort.abort("expected two ints, not ${other}")
            })
    }

    pub enum Parsed {
        case Mul(Int32, Int32)
        case Dont
        case Do
    }

    pub def solve2(do: {do = Bool}, acc: Int32, l: List[Parsed]): Int32 = match l {
        case Parsed.Mul(x, y) :: rest => solve2(do, if (do#do) acc + x*y else acc, rest)
        case Parsed.Dont :: rest => solve2(do = false, acc, rest)
        case Parsed.Do :: rest => solve2(do = true, acc, rest)
        case Nil => acc
    }

    pub def parse2(s: String): List[Parsed] \ Abort = {
        use Util.Option.get;
        use Int32.fromString;

        def parsePart(part: String): Parsed = {
            match Regex.submatches(substr = regex"\\d\\d?\\d?", part) {
                case one :: two :: Nil => Parsed.Mul(fromString(one) |> get, fromString(two) |> get)
                case _ if part == "don't()" => Parsed.Dont
                case _ if part == "do()" => Parsed.Do
                case _ => Abort.abort("'${part}' did not match anything.")
            }
        };

        s |> Regex.submatches(substr = regex"(mul\\(\\d\\d?\\d?,\\d\\d?\\d?\\))|(don't\\(\\))|(do\\(\\))")
            |> map(parsePart)
    }

    pub def solve1FromFile(): Int32 \ IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr(-1);
        Util.Effect.runWith(handler)(() -> {
            FileRead.read("./inputs/Day3.txt") |> parse1 |> solve1
        })
    }

    @Test
    pub def assertDay2Part1(): Bool \ IO = {
        solve1FromFile() == 190604937
    }

    pub def solve2FromFile(): Int32 \ IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr(-1);
        Util.Effect.runWith(handler)(() -> {
            FileRead.read("./inputs/Day3.txt") |> parse2 |> solve2(do = true, 0)
        })
    }

    @Test
    pub def assertDay2Part2(): Bool \ IO = {
        solve2FromFile() == 82857512
    }

}
