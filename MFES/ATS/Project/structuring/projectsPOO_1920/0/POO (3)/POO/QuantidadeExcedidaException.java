public class QuantidadeExcedidaException extends Exception {

    public QuantidadeExcedidaException() {
        super("Excedeu a quantidade maxima");
    }

    public QuantidadeExcedidaException(String msg) {
        super(msg);
    }
}
