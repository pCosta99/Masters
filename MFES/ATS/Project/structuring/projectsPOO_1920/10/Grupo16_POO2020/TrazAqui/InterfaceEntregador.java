package TrazAqui;

/**
 * Interface Utilizada para representar os métodos que um objeto instância de uma  Subclasse da Classe Entregas, deve implementar obrigatoriamente.
 */
public interface InterfaceEntregador{
    boolean aceitoTransporteMedicamentos();
    void aceitaMedicamentos(boolean state);
    boolean estaDisponivel();
    void mudaDisponibilidade(boolean state);
    int compareTo(InterfaceEntregador entregador);
}