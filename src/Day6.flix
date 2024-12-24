mod Day6 {
    use Functor.map
    use Foldable.toVector
    use Foldable.size
    use Abort.abort

    pub type alias Vector2[t] = Vector[Vector[t]]

    pub enum Cell with Eq, ToString {
        case Empty
        case Blocked
        case Up, Down, Left, Right
    }

    pub enum Direction with Eq, ToString {
        case Up, Down, Left, Right
    }

    pub def solve1(p: Vector2[Cell]): Int32 \ Abort = region local {
        let {dim = (h, w), blocked, guard = (dir0, (x0, y0))} = extractGuard(p);
        let covered = MutSet.empty(local);
        def loop(dir, x, y) = {
            if (x < 0 or x >= h or y < 0 or y >= w)
                ()
            else {
                covered |> MutSet.add((x, y));
                let walk = match dir {
                    case Direction.Up => (x-1, y)
                    case Direction.Down => (x+1, y)
                    case Direction.Left => (x, y-1)
                    case Direction.Right => (x, y+1)
                };
                if (Set.memberOf(walk, blocked)) {
                    match dir {
                        case Direction.Up => loop(Direction.Right, x, y)
                        case Direction.Down => loop(Direction.Left, x, y)
                        case Direction.Left => loop(Direction.Up, x, y)
                        case Direction.Right => loop(Direction.Down, x, y)
                    }
                } else loop(dir, fst(walk), snd(walk))
            }
        };
        loop(dir0, x0, y0);
        MutSet.size(covered)
    }

    def extractGuard(p: Vector2[Cell]): {dim = (Int32, Int32), blocked = Set[(Int32, Int32)], guard = (Direction, (Int32, Int32))} \ Abort = region local {
        let dim = if (size(p) <= 0) (0, 0) else (size(p), unsafely OutOfBounds run size(p[0]));
        let blocked = MutSet.empty(local);
        let guard = Ref.fresh(local, None);
        def addGuard(dir, loc) = {
            if (Ref.get(guard) != None) abort("Multiple guards")
            else guard |> Ref.put(Some((dir, loc)))
        };
        foreach (i <- List.range(0, size(p)); j <- List.range(0, size(unsafely OutOfBounds run p[i]))) {
            match unsafely OutOfBounds run p[i][j] {
                case Cell.Empty => ()
                case Cell.Blocked => blocked |> MutSet.add((i, j))
                case Cell.Up => addGuard(Direction.Up, (i, j))
                case Cell.Down => addGuard(Direction.Down, (i, j))
                case Cell.Left => addGuard(Direction.Left, (i, j))
                case Cell.Right => addGuard(Direction.Right, (i, j))
            }
        };
        {dim = dim, blocked = blocked |> MutSet.toSet, guard = Util.Option.get(Ref.get(guard))}
    }

    pub def parse(l: List[String]): Vector2[Cell] \ Abort = {
        l |> map(String.toVector >> map(
            s -> match s {
                case '.' => Cell.Empty
                case '#' => Cell.Blocked
                case '^' => Cell.Up
                case '>' => Cell.Left
                case '<' => Cell.Right
                case 'v' => Cell.Down
                case other => abort("'${other}' is not valid input")
            }
        )) |> toVector
    }

    pub def solve1ExampleFromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day6Example.txt")
            |> parse
            |> solve1
        })

    @Test
    pub def assertPart1Example(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 41, solve1ExampleFromFile())
    })

    pub def solve1FromFile(): Int32 \ IO =
        Util.runAll(-1)(() -> {
            FileRead.readLines("./inputs/Day6.txt")
            |> parse
            |> solve1
        })

    @Test
    pub def assertPart1(): Bool \ IO = Util.Test.runTest(() -> {
        Util.Test.assertEq(expected = 5318, solve1FromFile())
    })

}
