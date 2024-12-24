mod Util {

    mod Test {
        pub def assertEq(expected: {expected = t}, actual: t): Unit \ Abort with Eq[t], ToString[t] = {
            if (expected#expected == actual)
                ()
            else {
                Abort.abort("Expected ${expected#expected}, found ${actual}")
            }
        }

        pub def assertEqDebug(expected: {expected = t}, actual: t): Unit \ Abort with Eq[t] = {
            if (expected#expected == actual)
                ()
            else {
                Abort.abort("Expected %{expected#expected}, found %{actual}")
            }
        }

        pub def runTest(test: Unit -> Unit \ ef): Bool \ ef - Abort + IO = {
            (() -> {test(); true}) |> Util.Effect.runWith(Util.Abort.handleWithPrintDefault(false))
        }
    }

    mod Option {
        pub def get(o: Option[t]): t \ Abort = match o {
            case Some(v) => v
            case None => Abort.abort("Called 'get' on None.")
        }
    }

    mod Abort {
        import java.lang.System;

        pub def handleWithPrintDefault(default: t, f: a -> t \ ef): a -> t \ ef + IO - Abort = {
            x -> run f(x) with Abort {
                def abort(m, _) = {System.err.println("Err: ${m}"); default}
            }
        }
    }

    mod Func {
        pub def apply(x: a, f: a -> b \ ef): b \ ef =
            f(x)

        pub def compose(f: b -> c \ ef2, g: a -> b \ ef1): a -> c \ ef1 + ef2 =
            g >> f

        pub def >|>(f: a -> (b, c) \ ef1, g: (b, c) -> d \ ef2): a -> d \ ef1 + ef2 =
            f >> Util.Tuple.lift2(g)
    }

    mod IoError {
        pub def toString(err: IoError): String = match err {
            case IoError.IoError(kind, msg) => "${kind}: ${msg}"
        }
    }

    mod Effect {
        pub def runWith(handler: (Unit -> a \ ef1) -> (Unit -> b \ ef2), f: Unit -> a \ ef1): b \ ef2 =
            handler(f)()
    }

    mod FileRead {
        pub def handleWithIOAbort(f: a -> b \ ef): a -> b \ ef - FileRead + Abort + IO = {
            x -> match FileRead.handle(f)(x) {
                case Ok(v) => v
                case Err(err) => Abort.abort(Util.IoError.toString(err))
            }
        }
    }

    mod Bool {
        pub def toInt32(b: Bool): Int32 = if (b) 1 else 0
    }

    mod Tuple {
        pub def mk2(x: a, y: b): (a, b) =
            (x, y)

        pub def lift2(f: (a, b) -> c \ ef): ((a, b)) -> c \ ef =
            match (x, y) -> f(x, y)

        pub def app2(f: a -> b \ ef, t: (a, a)): (b, b) \ ef =
            match t {
                case (x, y) => (f(x), f(y))
            }

        pub def swap2(t: (a, b)): (b, a) =
            (snd(t), fst(t))
    }

    mod List {
        use List.length;

        pub def nth(n: Int32, l: List[t]): t \ OutOfBounds = {
            def loop(n0, l0) = match l0 {
                case x :: xs =>
                    if (n0 == 0) x
                    else loop(n0 - 1, xs)
                case Nil =>
                    OutOfBounds.outOfBounds("${n} out of bounds for list of length ${List.length(l)}")
            };
            if (n < 0) OutOfBounds.outOfBounds("${n} out of bounds for list of length ${List.length(l)}")
            else loop(n, l)
        }

        pub def take2(l: List[t]): (t, t) \ Abort = match l {
            case one :: two :: Nil => (one, two)
            case _ => Abort.abort("called take2 on list of length ${length(l)}")
        }

        pub def take2Opt(l: List[t]): Option[(t, t)] = match l {
            case one :: two :: Nil => Some((one, two))
            case _ => None
        }

        /// List.map2 should be called zipMap
        pub def map2(f: (a, a) -> b \ ef, l: List[a]): List[b] \ ef = {
            def loop(l0, acc) = match l0 {
                case one :: two :: rest =>
                    loop(two :: rest, Cons(f(one, two)))
                case _ =>
                    acc(Nil)
            };
            loop(l, x -> checked_ecast(x))
        }
    }

    mod Foldable {
        pub def countWithIndex(
            f: (Int32, t) -> Bool \ ef, l: m[t]
        ): Int32 \ ef + Foldable.Aef[m] with Foldable[m] = {
            use Add.add;
            use Foldable.foldLeft;
            use Ref.{fresh, get, transform};

            region local {
                let index = fresh(local, -1);
                (0, l) ||> foldLeft((count, x) -> {
                    index |> transform(add(1));
                    if (f(get(index), x)) count + 1
                    else count
                })
            }
        }
    }

    mod OutOfBounds {
        pub def handleWithAbort(f: a -> b \ ef): a -> b \ ef - OutOfBounds + Abort =
            x -> run f(x) with OutOfBounds {
                def outOfBounds(msg, _k) = Abort.abort("OutOfBounds: ${msg}")
            }
    }

    mod KeyNotFound {
        pub def handleWithAbort(f: a -> b \ ef): a -> b \ ef - KeyNotFound + Abort =
            x -> run f(x) with KeyNotFound {
                def keyNotFound(msg, _k) = Abort.abort("KeyNotFound: ${msg}")
            }
    }

    pub def runAll(default: t, f: Unit -> t \ ef): t \ ef - {Abort, FileRead, OutOfBounds, KeyNotFound} + IO = {
        let handler =
            Util.FileRead.handleWithIOAbort
            >> OutOfBounds.handleWithAbort
            >> KeyNotFound.handleWithAbort
            >> Util.Abort.handleWithPrintDefault(default);
        Effect.runWith(handler)(f)
    }

}
