public interface ETransportadoraMedicamentosI extends Empresa_TransportadoraI{
    void aceitaMedicamentos(boolean state);
    boolean aceitoTransporteMedicamentos();
    void leETM(String cod, String[] p);
}