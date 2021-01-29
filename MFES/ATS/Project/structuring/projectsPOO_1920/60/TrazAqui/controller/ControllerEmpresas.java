package controller;

import interfaces.*;
import model.EmpresaIndisponivelException;
import model.NaoExisteEncomendaException;
import model.RandomGenerator;
import view.InterfaceEmpresa;
import view.InterfaceGeral;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Classe que implementa o controller para as empresas
 */
public class ControllerEmpresas {
	private final ISistema sistema;
	private final String empresa;
	private final List<IEncomenda> encomenda;

	/**
	 * Construtor de um controller para as empresas
	 * @param s sistema
	 * @param empresa empresa
	 */
	public ControllerEmpresas(ISistema s, String empresa) {
		this.sistema = s;
		this.empresa = empresa;
		this.encomenda = new ArrayList<>();
	}

	/**
	 * Menu empresas
	 */
	public void menu() {

		boolean sair = false;
		//Empresa transportadora = this.sistema.getTransportadoras().get(this.empresa);

		while (!sair) {

			InterfaceEmpresa.menu();
			InterfaceEmpresa.message(">> ");
			int opcao = Input.lerInt();

			switch (opcao) {

				case 1:
					this.sistema.getTransportadoras().get(empresa).setLivre(true);
					InterfaceEmpresa.printLivre(this.sistema.getTransportadoras().get(empresa));

					break;

				case 2:
					try {
						determinarCusto();
					} catch (EmpresaIndisponivelException | NaoExisteEncomendaException e) {
						InterfaceEmpresa.message(e.getMessage());
					}
					break;

				case 3:
					try {
						validarTransporte();
					} catch (EmpresaIndisponivelException | NaoExisteEncomendaException e) {
						InterfaceEmpresa.message(e.getMessage());
					}
					break;

				case 4:
					entregas();
					break;

				case 5:
					IEmpresa v = this.sistema.getTransportadoras().get(this.empresa);

					if(v.aceitoTransporteMedicamentos()) {
						v.aceitaMedicamentos(false);
						InterfaceEmpresa.message("Já não está apto para o transporte de medicamentos\n");
					}
					else {
						v.aceitaMedicamentos(true);
						InterfaceEmpresa.message("Está apto para o transporte de medicamentos\n");
					}

					break;

				case 6:
					sair = true;
			}
		}
	}

	/**
	 * Determina custo de transporte de encomenda
	 */
	public void determinarCusto() throws EmpresaIndisponivelException, NaoExisteEncomendaException{
		IEmpresa transportadora = this.sistema.getTransportadoras().get(empresa);

		if(transportadora.isLivre()) {
			if(this.encomenda.size() < transportadora.getNumEnc()) {
				if(printaEncsRaio(this.sistema.getEncomendasAceites()) == 0)
					throw new NaoExisteEncomendaException("Não existem encomendas disponíveis de momento\n");

				InterfaceGeral.message(">> ");
				String e = Input.lerString();

				if(this.sistema.getEncomendas().containsKey(e)) {
					double custo = transportadora.custoEntrega(this.sistema, e);

					InterfaceEmpresa.message("O custo da entrega " + e + " é " + custo + "\n");

					boolean userAceita = RandomGenerator.generateBoolean();
					if (userAceita) {
						InterfaceEmpresa.message("O utilizador aceitou o custo " + custo + "\n");

						IEncomenda enc = this.sistema.getEncomendas().get(e);
						enc.setData(LocalDateTime.now());
						this.encomenda.add(enc);
						this.sistema.getTransportadoras().get(empresa).addEncomenda(enc);
						this.sistema.getEncomendas().get(e).setJaTransportada(true);
						this.sistema.removeEnc(e);

					} else InterfaceEmpresa.message("O utilizador não aceitou o custo " + custo + "\n");
				}
				else throw new NaoExisteEncomendaException("A encomenda escolhida não existe.\n");

			} else throw new NaoExisteEncomendaException("Atingiu o limite de encomendas.\n");
		}
		else {
			throw new EmpresaIndisponivelException("O seu estado atual é indisponível.\n");
		}
	}

	/**
	 * valida transporte de encomenda
	 */
	public void validarTransporte() throws EmpresaIndisponivelException, NaoExisteEncomendaException{
		IEmpresa transportadora = this.sistema.getTransportadoras().get(empresa);

		if(transportadora.isLivre()) {
			if (this.encomenda.size() != 0) {
				int i = 0;
				while(i < this.encomenda.size()) {
					IEncomenda enc = this.encomenda.get(i);
					InterfaceEmpresa.message("O transporte da encomenda " + enc.getCode() + " inicia agora\n");

					ILoja loja = this.sistema.getLojas().get(enc.getVendedor());
					IUser user = this.sistema.getUtilizadores().get(enc.getComprador());

					double velocidade = transportadora.getVelocidade();
					double distLoja = transportadora.distancia(loja);
					double distCliente = loja.distancia(user);

					double tempo = (distLoja + distCliente) / velocidade;
					double custo = transportadora.custoEntrega(this.sistema, enc.getCode());

					InterfaceEmpresa.message("O transporte demorou " + tempo + " horas com o custo de " + custo + "\n");
					i++;

				}
				this.encomenda.clear();
				transportadora.setLivre(true);
			}
			else throw new NaoExisteEncomendaException("Nenhuma encomenda escolhida.\n");
		}
		else throw new EmpresaIndisponivelException("O seu estado atual é indisponível.\n");
	}

	/**
	 * Faz print das encomendas disponíveis no raio da empresa
	 * @param encs lista de encomendas no sistema
	 * @return número de encomendas disponíveis no raio
	 */
	public int printaEncsRaio(List<String> encs){
		int i = 0;
		IEncomenda e;
		IEmpresa transportadora = this.sistema.getTransportadoras().get(empresa);

		for(String enc : encs) {
			e = this.sistema.getEncomendas().get(enc);
			if (transportadora.inRaio(e.getVendedor()) && transportadora.inRaio(e.getComprador())
					&& apto(transportadora, e) && !e.getJaTransportada()) {
				InterfaceEmpresa.message(enc + "\n");
				i++;
			}
		}
		return i;
	}

	/**
	 * Verifica se a empresa pode transportar a encomenda, sendo ela de medicamentos
	 * @param v empresa
	 * @param e encomenda
	 * @return Se pode transportar a encomenda ou não
	 */
	private boolean apto(IEntregas v, IEncomenda e) {
		return !e.getMedicine() || v.aceitoTransporteMedicamentos();
	}

	/**
	 * Informação das entregas de um certo voluntário ou empresa
	 */
	private void entregas() {

		InterfaceEmpresa.message("Pretente voluntário ou empresa transportadora? (V ou E) ");
		String opcao = Input.lerString();

		InterfaceEmpresa.message("Intervalo de tempo [mes1,mes2]:\n mes1: ");
		int mes1 = Input.lerInt();
		InterfaceEmpresa.message(" mes2: ");
		int mes2 = Input.lerInt();

		if(opcao.equals("V")) {
			Map<String, IVoluntario> voluntarios = this.sistema.getVoluntarios();

			InterfaceEmpresa.listaVoluntarios(voluntarios);
			InterfaceEmpresa.message(">> ");
			String v = Input.lerString();

			while (!voluntarios.containsKey(v)) {
				InterfaceEmpresa.message(">> ");
				v = Input.lerString();
			}
			for(IEncomenda e: voluntarios.get(v).getEncomendas().values())
				if(e.getData().getMonthValue() >= mes1 && e.getData().getMonthValue() <= mes2)
					InterfaceEmpresa.printEncomenda(e);


		}
		else if(opcao.equals("E")) {

			Map<String,IEmpresa> empresas = this.sistema.getTransportadoras();

			InterfaceEmpresa.listaEmpresas(empresas);
			InterfaceEmpresa.message(">> ");
			String e = Input.lerString();

			while (!empresas.containsKey(e)) {
				InterfaceEmpresa.message(">> ");
				e = Input.lerString();
			}

			for(IEncomenda enc: empresas.get(e).getEncomendas().values())
				if(enc.getData().getMonthValue() >= mes1 && enc.getData().getMonthValue() <= mes2)
					InterfaceEmpresa.printEncomenda(enc);
		}

	}
}
