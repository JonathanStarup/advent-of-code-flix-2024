mod Day5 {
    use Foldable.{foldLeft, nonEmpty, size, sum}
    use FromString.fromString
    use Functor.map

    pub type alias Problem = {
        rules = List[(Int32, Int32)],
        updates = List[List[Int32]]
    }

    pub def solve1(p: Problem): Int32 \ {OutOfBounds, KeyNotFound} = {
        use List.filter;

        let back = ruleMap(p#rules);
        p#updates
            |> filter(checkUpdates(Set.empty(), back))
            |> map(middleVal)
            |> sum
    }

    def middleVal(l: List[Int32]): Int32 \ OutOfBounds =
        Util.List.nth(size(l) / 2, l)

    def ruleMap(l: List[(Int32, Int32)]): MultiMap[Int32, Int32] =
        (MultiMap.empty(), l)
        ||> foldLeft(m -> match (pre, post) -> MultiMap.insert(pre, post, m))

    def checkUpdates(
        seen: Set[Int32], rules: MultiMap[Int32, Int32], updates: List[Int32]
    ): Bool \ KeyNotFound = {
        use Set.{insert, intersection};

        match updates {
            case update :: rest =>
                if (rules[update] |> intersection(seen) |> nonEmpty) false
                else checkUpdates(seen |> insert(update), rules, rest)
            case Nil =>
                true
        }
    }

    def fixUpdates(
        acc: List[Int32],
        seen: Set[Int32],
        rules: MultiMap[Int32, Int32],
        updates: List[Int32]
    ): List[Int32] = {
        use List.reverse;
        use MultiMap.get;
        use Set.{insert, intersection};

        match updates {
            case update :: rest =>
                let forward = get(update, rules);
                if (forward |> intersection(seen) |> nonEmpty) {
                    let acc1 =
                        insertUpTo(update, forward, reverse(acc))
                        |> reverse;
                    fixUpdates(acc1, seen, rules, rest)
                } else
                    fixUpdates(update :: acc, seen
                    |> insert(update), rules, rest)
            case Nil =>
                reverse(acc)
        }
    }

    def insertUpTo(x: t, to: Set[t], l: List[t]): List[t] with Order[t] =
        match l {
            case y :: _ if Set.memberOf(y, to) => x :: l
            case y :: ys => y :: insertUpTo(x, to, ys)
            case Nil => Nil
        }

    pub def solve2(p: Problem): Int32 \ {OutOfBounds, KeyNotFound} = {
        use List.filter;

        let back = ruleMap(p#rules);
        p#updates
            |> filter(u -> not checkUpdates(Set.empty(), back, u))
            |> map(fixUpdates(Nil, Set.empty(), back))
            |> map(middleVal)
            |> sum
    }

    pub def parse(l: List[String]): Problem \ Abort = {
        use Regex.submatches;
        use String.splitOn;
        use Util.List.take2;
        use Util.Option.get;
        use Util.Tuple.app2;

        let splitPoint = List.indexOf("", l) |> get;
        let (rulesInput, updatesInput0) = List.splitAt(splitPoint, l);
        let updatesInput = List.drop(1, updatesInput0);

        let rules = rulesInput |> map(
            submatches(substr = regex"\\d+")
            >> take2
            >> app2(fromString >> get)
        );
        let updates = updatesInput |> map(
            splitOn(substr = ",") >> map(fromString >> get)
        );
        {rules = rules, updates = updates}
    }

    pub def solve1FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day5.txt")
            |> parse
            |> solve1
        })

    @Test
    pub def assertPart1(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 4957, solve1FromFile())
    })

    pub def solve2FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day5.txt")
            |> parse
            |> solve2
        })

    @Test
    pub def assertPart2(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 6938, solve2FromFile())
    })

}
