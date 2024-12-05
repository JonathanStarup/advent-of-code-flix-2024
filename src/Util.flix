mod Util {

    mod Option {
        pub def get(o: Option[t]): t \ Abort = match o {
            case Some(v) => v
            case None => Abort.abort("Called 'get' on None.")
        }
    }

    mod Abort {
        import java.lang.System;

        pub def handleWithErr(default: t, f: a -> t \ ef): a -> t \ ef + IO - Abort = {
            x -> run f(x) with Abort {
                def abort(m, _) = {System.err.println("Err: ${m}"); default}
            }
        }
    }

    mod Func {
        pub def _apply(f: Unit -> t \ ef): t \ ef =
            f()

        pub def _compose(f: b -> c \ ef2, g: a -> b \ ef1): a -> c \ ef1 + ef2 =
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
        pub def _toInt32(b: Bool): Int32 = if (b) 1 else 0
    }

    mod Tuple {
        pub def lift2(f: (a, b) -> c \ ef): ((a, b)) -> c \ ef =
            match (x, y) -> f(x, y)

        pub def app2(f: a -> b \ ef, t: (a, a)): (b, b) \ ef =
            match t {
                case (x, y) => (f(x), f(y))
            }
    }

    mod List {
        use List.length;
        pub def take2(l: List[t]): (t, t) \ Abort = match l {
            case one :: two :: Nil => (one, two)
            case _ => Abort.abort("called take2 on list of length ${length(l)}")
        }

        pub def take2Opt(l: List[t]): Option[(t, t)] = match l {
            case one :: two :: Nil => Some((one, two))
            case _ => None
        }
    }

    pub def runAll(default: t, f: Unit -> t \ ef): t \ ef - {Abort, FileRead} + IO = {
        let handler = Util.FileRead.handleWithIOAbort >> Util.Abort.handleWithErr(default);
        Effect.runWith(handler)(f)
    }

}
