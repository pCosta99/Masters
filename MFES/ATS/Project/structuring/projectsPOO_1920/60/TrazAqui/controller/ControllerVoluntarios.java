package controller;

import interfaces.*;
import model.NaoExisteEncomendaException;
import model.VoluntarioIndisponivelException;
import view.InterfaceVoluntario;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Classe que implementa o controller para os voluntarios
 */
public class ControllerVoluntarios{
	private final ISistema sistema;
	private final String voluntario;
	private IEncomenda encomenda;

	/**
	 * Construtor de um controller para os voluntarios
	 * @param s sistema
	 * @param voluntario voluntário
	 */
	public ControllerVoluntarios(ISistema s, String voluntario){
		this.sistema = s;
		this.voluntario = voluntario;
	}

	/**
	 * Menu voluntário
	 */
	public void menu(){

		boolean sair = false;
		IVoluntario voluntario = this.sistema.getVoluntarios().get(this.voluntario);

		while(!sair) {

			InterfaceVoluntario.menu();
			InterfaceVoluntario.message(">> ");
			int opcao = Input.lerInt();


			switch (opcao) {

				case 1:
					voluntario.setLivre(true);
					InterfaceVoluntario.printLivre(voluntario);
					break;

				case 2:
					try {
						escolherEncomenda(voluntario);
					} catch (VoluntarioIndisponivelException | NaoExisteEncomendaException e) {
						InterfaceVoluntario.message(e.getMessage());
					}
					break;

				case 3:
					try {
						transporteDuracao(voluntario);
					} catch (NaoExisteEncomendaException e) {
						InterfaceVoluntario.message(e.getMessage());
					}
					break;

				case 4:
					entregas();
					break;

                case 5:
					IVoluntario v = this.sistema.getVoluntarios().get(this.voluntario);

                    if(v.aceitoTransporteMedicamentos()) {
						v.aceitaMedicamentos(false);
						InterfaceVoluntario.message("Já não está apto para o transporte de medicamentos\n");
					}
                    else {
						v.aceitaMedicamentos(true);
						InterfaceVoluntario.message("Está apto para o transporte de medicamentos\n");
					}

                    break;

				case 6:
					sair = true;
					break;
			}
		}
	}

	/**
	 * Escolher a encomenda para entregar
	 * @param voluntario voluntário
	 */
	private void escolherEncomenda(IVoluntario voluntario) throws VoluntarioIndisponivelException, NaoExisteEncomendaException{

		if(voluntario.isLivre()) {
			if(printaEncsRaio(this.sistema.getEncomendasAceites(),voluntario) == 0)
				throw new NaoExisteEncomendaException("Não tem encomendas disponíveis de momento\n");

			InterfaceVoluntario.message(">> ");
			String enc = Input.lerString();

			if(this.sistema.getEncomendas().containsKey(enc)){
				IEncomenda e = this.sistema.getEncomendas().get(enc);
				e.setData(LocalDateTime.now());
				this.sistema.removeEnc(enc);
				this.encomenda = e;
				voluntario.setLivre(false);

				InterfaceVoluntario.message("Está encarregue de ir buscar a encomenda " + enc + "\n");
			}
			else throw new NaoExisteEncomendaException("A encomenda escolhida não existe.\n");

		}
		else throw new VoluntarioIndisponivelException("O seu estado atual é indisponível\n");
	}

	/**
	 * Faz print das encomendas disponíveis no raio do voluntário
	 * @param encs lista de encomendas no sistema
	 * @param vol voluntário
	 * @return número de encomendas disponíveis no raio
	 */
	private int printaEncsRaio(List<String> encs, IVoluntario vol){
		IEncomenda e;
		int i = 0;

		for(String enc : encs) {
			e = this.sistema.getEncomendas().get(enc);
			if (vol.inRaio(e.getVendedor()) && vol.inRaio(e.getComprador()) && apto(vol, e) && !e.getJaTransportada()) {
				InterfaceVoluntario.message(enc + "\n");
				i++;
			}
		}
		return i;
	}

	/**
	 * Verifica se o voluntário pode transportar a encomenda, sendo ela de medicamentos
	 * @param v voluntário
	 * @param e encomenda
	 * @return Se pode transportar a encomenda ou não
	 */
	private boolean apto(IEntregas v, IEncomenda e) {
		return !e.getMedicine() || v.aceitoTransporteMedicamentos();
	}

	/**
	 * Validar transporte da encomenda e duração da entrega
	 * @param voluntario voluntário
	 */
	private void transporteDuracao(IVoluntario voluntario) throws NaoExisteEncomendaException{

		//encomendas disponíveis
		Set<IEncomenda> listEncs = this.sistema.getVoluntarios().get(this.voluntario).getEncomendas().values().stream()
																					  .filter(e -> !e.getJaTransportada())
																					  .collect(Collectors.toSet());
		listEncs.add(this.encomenda);
		if (!listEncs.isEmpty()) {
			InterfaceVoluntario.x(listEncs);
			InterfaceVoluntario.message(">> ");
			String opcao = Input.lerString();
			if(!this.sistema.getEncomendas().containsKey(opcao)) throw new NaoExisteEncomendaException("Inválido.\n");
			IEncomenda e = this.sistema.getEncomendas().get(opcao);
			
			InterfaceVoluntario.message("O transporte da encomenda " + e.getCode() + " inicia agora\n");
			ILoja loja = this.sistema.getLojas().get(e.getVendedor());
			IUser user = this.sistema.getUtilizadores().get(e.getComprador());

			double velocidade = voluntario.getVelocidade();
			double distLoja = voluntario.distancia(loja);
			double distCliente = voluntario.distancia(user);

			double tempo = (distLoja + distCliente) / velocidade;

			InterfaceVoluntario.message("O transporte durou " + tempo + " horas\n");
			this.sistema.getLojas().get(e.getVendedor()).diminuiFila();
			e.setJaTransportada(true);
			this.sistema.getVoluntarios().get(this.voluntario).addEncomenda(e);
			this.encomenda = null;
		}
		else throw new NaoExisteEncomendaException("Nenhuma encomenda escolhida para ir buscar.\n");
	}

	/**
	 * Informação das entregas de um certo voluntário ou empresa
	 */
	private void entregas() {

		InterfaceVoluntario.message("Pretente voluntário ou empresa transportadora? (V ou E) ");
		String opcao = Input.lerString();

		InterfaceVoluntario.message("Intervalo de tempo [mes1,mes2]:\n mes1: ");
		int mes1 = Input.lerInt();
		InterfaceVoluntario.message(" mes2: ");
		int mes2 = Input.lerInt();

		if(opcao.equals("V")) {
			Map<String, IVoluntario> voluntarios = this.sistema.getVoluntarios();

			InterfaceVoluntario.listaVoluntarios(voluntarios);
			InterfaceVoluntario.message(">> ");
			String v = Input.lerString();

			while (!voluntarios.containsKey(v)) {
				InterfaceVoluntario.message(">> ");
				v = Input.lerString();
			}
			for(IEncomenda e: voluntarios.get(v).getEncomendas().values())
				if(e.getData().getMonthValue() >= mes1 && e.getData().getMonthValue() <= mes2)
					InterfaceVoluntario.printEncomenda(e);


		}
		else if(opcao.equals("E")) {

			Map<String,IEmpresa> empresas = this.sistema.getTransportadoras();

			InterfaceVoluntario.listaEmpresas(empresas);
			InterfaceVoluntario.message(">> ");
			String e = Input.lerString();

			while (!empresas.containsKey(e)) {
				InterfaceVoluntario.message(">> ");
				e = Input.lerString();
			}

			for(IEncomenda enc: empresas.get(e).getEncomendas().values())
				if (enc.getData().getMonthValue() >= mes1 && enc.getData().getMonthValue() <= mes2)
					InterfaceVoluntario.printEncomenda(enc);
		}

	}
}
