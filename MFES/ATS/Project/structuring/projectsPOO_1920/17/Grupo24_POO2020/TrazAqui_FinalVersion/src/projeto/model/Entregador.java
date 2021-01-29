package projeto.model;

import java.util.List;


public interface Entregador {
	
	public abstract boolean possoRecolher();
	public abstract boolean aceitoTransporteMedicamentos();
	public abstract void aceitaMedicamentos(boolean state);
	public abstract List<String> verHistoricoEntregas();
	public abstract double distanciaAteDestino(double pontoX, double pontoY);
	public abstract boolean estaDentroDoRaio(double dist);
	//public abstract double duracaoEntregaMin(Encomenda enc, Loja l);
	
}
