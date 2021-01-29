public interface ITransporteMedico extends ITransporte{
    /**
	 * Devolve o boolean aceitaEncMedica da classe
	 * @return boolean aceitaEncMedica da classe
	 */
    boolean aceitoTransporteMedicamentos();
	
	void aceitaMedicamentos(boolean state);
}
