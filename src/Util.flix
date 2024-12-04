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

}
