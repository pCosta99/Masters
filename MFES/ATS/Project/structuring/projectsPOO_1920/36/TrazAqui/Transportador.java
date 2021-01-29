
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
public interface Transportador {
    public boolean disponivelParaTransporte();
    public void setDisponibilidade(boolean disponivel);
    public double calculaCustoTransporte(double distancia, double duracao);
}
