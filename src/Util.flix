mod Util {

    mod Option {
        pub def get(o: Option[t]): t \ Abort = match o {
            case Some(v) => v
            case None => Abort.abort("Called 'get' on None.")
        }
    }

    mod Abort {
        import java.lang.System

        pub def handleWithErr(f: a -> Unit \ ef): a -> Unit \ ef + IO - Abort = {
            x -> run f(x) with Abort {
                def abort(m, _) = System.err.println("Err: ${m}")
            }
        }
    }

    mod Func {
        pub def apply(f: Unit -> t \ ef): t \ ef =
            f()

        pub def compose(f: b -> c \ ef2, g: a -> b \ ef1): a -> c \ ef1 + ef2 =
            g >> f
    }

    mod IoError {
        pub def toString(err: IoError): String = match err {
            case IoError.IoError(kind, msg) => "${kind}: ${msg}"
        }
    }

    mod Effect {
        pub def runWith(handler: (Unit -> Unit \ ef1) -> (Unit -> b \ ef2), f: Unit -> Unit \ ef1): b \ ef2 =
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

}
