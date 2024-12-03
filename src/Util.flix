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

        pub def apply(f: Unit -> t \ ef): t \ ef = f()

    }

}
