package controller;

import interfaces.IEmpresa;
import interfaces.IEncomenda;
import interfaces.ISistema;
import interfaces.IVoluntario;
import model.NaoExisteEncomendaException;
import view.InterfaceLoja;

import java.util.Map;

/**
 * Classe que implementa o controller para as lojas
 */
public class ControllerLoja {
	private final ISistema sistema;
	private final String loja;

	/**
	 * Construtor de um controller para as lojas
	 * @param s sistema
	 * @param loja loja
	 */
	public ControllerLoja(ISistema s, String loja){
		this.sistema = s;
		this.loja = loja;
	}

	/**
	 * Menu lojas
	 */
	public void menu() {

		boolean sair = false;

		while(!sair) {
			InterfaceLoja.menu();
			InterfaceLoja.message(">> ");
			int opcao = Input.lerInt();

			switch(opcao){
				case 1:
					try{
						encPronta();
					} catch (NaoExisteEncomendaException e) {
						InterfaceLoja.message(e.getMessage());
					}
					break;

				case 2:
					InterfaceLoja.message("Indique quantas pessoas estão em fila de espera: ");
					int i = Input.lerInt();
					this.sistema.getLojas().get(loja).setFilaSize(i);
					break;

				case 3:
					entregas();
					break;

				case 4:
					sair = true;
					break;
			}
		}
	}

	/**
	 * Sinalizar que uma encomenda está pronta para ser entregue
	 */
	public void encPronta() throws NaoExisteEncomendaException{
		int i = InterfaceLoja.listaEncs(this.loja, this.sistema.getEncomendas(), this.sistema.getEncomendasAceites());
		InterfaceLoja.message(">> ");
		String e = Input.lerString();

		if (i==0) throw new NaoExisteEncomendaException("Não existem encomendas disponíveis\n");

		if(this.sistema.getEncomendas().containsKey(e)){
			this.sistema.aceitaEnc(e);
			InterfaceLoja.message("Atualização efetuada com sucesso!\n");
		}
		else throw new NaoExisteEncomendaException("A encomenda escolhida não existe.\n");
	}

	/**
	 * Informação das entregas de um certo voluntário ou empresa
	 */
	private void entregas() {

		InterfaceLoja.message("Pretente voluntário ou empresa transportadora? (V ou E) ");
		String opcao = Input.lerString();

		InterfaceLoja.message("Intervalo de tempo [mes1,mes2]:\n mes1: ");
		int mes1 = Input.lerInt();
		InterfaceLoja.message(" mes2: ");
		int mes2 = Input.lerInt();

		if(opcao.equals("V")) {
			Map<String, IVoluntario> voluntarios = this.sistema.getVoluntarios();

			InterfaceLoja.listaVoluntarios(voluntarios);
			InterfaceLoja.message(">> ");
			String v = Input.lerString();

			while (!voluntarios.containsKey(v)) {
				InterfaceLoja.message(">> ");
				v = Input.lerString();
			}
			for(IEncomenda e: voluntarios.get(v).getEncomendas().values())
				if(e.getData().getMonthValue() >= mes1 && e.getData().getMonthValue() <= mes2)
					InterfaceLoja.printEncomenda(e);


		}
		else if(opcao.equals("E")) {

			Map<String, IEmpresa> empresas = this.sistema.getTransportadoras();

			InterfaceLoja.listaEmpresas(empresas);
			InterfaceLoja.message(">> ");
			String e = Input.lerString();

			while (!empresas.containsKey(e)) {
				InterfaceLoja.message(">> ");
				e = Input.lerString();
			}

			for(IEncomenda enc: empresas.get(e).getEncomendas().values())
				if(enc.getData().getMonthValue() >= mes1 && enc.getData().getMonthValue() <= mes2)
					InterfaceLoja.printEncomenda(enc);
		}

	}
}
