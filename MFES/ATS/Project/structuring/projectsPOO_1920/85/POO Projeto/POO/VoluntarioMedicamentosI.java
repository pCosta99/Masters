public interface VoluntarioMedicamentosI extends VoluntarioI{
    void aceitaMedicamentos(boolean state);
    boolean aceitoTransporteMedicamentos();
    void leVlM(String cod, String[] p);
}
