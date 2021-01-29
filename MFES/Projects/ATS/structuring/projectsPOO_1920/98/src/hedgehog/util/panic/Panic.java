package hedgehog.util.panic;

@SuppressWarnings("serial")
public final class Panic extends RuntimeException {
    public static <T> T panic() {
        throw new Panic();
    }

    public static <T> T panic(final String msg) {
        throw new Panic(msg);
    }

    public Panic() {
        super();
    }

    public Panic(final String msg) {
        super("panicked at '" + msg + '\'');
    }
}
