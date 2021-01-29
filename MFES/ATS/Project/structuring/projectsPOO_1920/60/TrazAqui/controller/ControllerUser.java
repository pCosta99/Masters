package controller;


import interfaces.*;
import model.*;
import view.InterfaceUtilizador;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Classe que implementa o controller para os utilizadores
 */
public class ControllerUser {
    private final ISistema sistema;
    private final String user;

    /**
     * Construtor de um controller para os utilizadores
     * @param s sistema
     * @param user user
     */
    public ControllerUser(ISistema s, String user) {
        this.sistema = s;
        this.user = user;
    }

    /**
     * Menu user
     */
    public void menu() {

        boolean sair = false;

        while(!sair) {

            InterfaceUtilizador.menu();
            InterfaceUtilizador.message(">> ");
            int opcao = Input.lerInt();

            switch (opcao) {
                case 1:
                    try{
                        fazerEncomenda();
                    } catch (NaoExisteLojaException | NaoExisteProdutoException | ComandoDesconhecidoException e) {
                        e.getMessage();
                    }
                    break;
                case 2:
                    entregas();
                    break;

                case 3:
                    classificar();
                    break;

                case 4:
                    sair = true;
            }
        }

    }

    /**
     * Adiciona o produto à encomenda
     * @param enc encomenda
     * @param listProds lista de produtos
     * @param listLojas lista de lojas
     * @param loja loja
     */
    private void adicionarProduto(IEncomenda enc, Map<String, IProduto> listProds, Map<String, ILoja> listLojas, String loja) throws NaoExisteProdutoException {

        InterfaceUtilizador.produtosLoja(listProds);
        InterfaceUtilizador.message(">> ");
        String p = Input.lerString();

        if (!listProds.containsKey(p)) throw new NaoExisteProdutoException("O produto não existe.\n");

        InterfaceUtilizador.message("Insira a quantidade de produto que deseja comprar: ");
        double qtd = Input.lerDouble();

        IProduto prod = listProds.get(p);

        double valorUnid = prod.getValor(); //o produto que se compra tem os valores unitários
        double pesoUnid = prod.getPeso();
        boolean medicine = prod.getMedicine();

        IProduto newProd = new Produto(prod.getCode(), prod.getProduto(), qtd, valorUnid, pesoUnid * qtd, medicine);

        enc.addProduto(listLojas.get(loja), this.sistema.getUtilizadores().get(user), newProd);

    }

    /**
     * Solicitar encomenda
     */
    private void fazerEncomenda() throws NaoExisteLojaException, ComandoDesconhecidoException, NaoExisteProdutoException {
        boolean add = true;
        boolean maisProds;

        Map<String,ILoja> listLojas = this.sistema.getLojas();

        InterfaceUtilizador.listaLojas(listLojas);
        InterfaceUtilizador.message(">> ");
        String loja = Input.lerString();

        String codigo = RandomGenerator.generateEncomenda();
        while(this.sistema.getEncomendas().containsKey(codigo))
            codigo = RandomGenerator.generateEncomenda();

        IEncomenda enc = new Encomenda(codigo);

        if(!listLojas.containsKey(loja)) throw new NaoExisteLojaException("Loja inválida.\n");

        Map<String, IProduto> listProds = listLojas.get(loja).getProdutos();
        if(listProds.isEmpty()) {
            InterfaceUtilizador.message("Lamentamos, mas esta loja não tem produtos disponíveis de momento\n");
            return;
        }

        while (add) {
            maisProds = true;

            adicionarProduto(enc, listProds, listLojas, loja);

            while (maisProds) {
                InterfaceUtilizador.message("Deseja adicionar mais produtos? (S ou N): ");
                String soun = Input.lerString();

                if (soun.equals("N")) {
                    maisProds = false;
                    add = false;
                } else if(soun.equals("S")) maisProds = false;
                else throw new ComandoDesconhecidoException("Comando inválido.\n");
            }
        }
        this.sistema.getUtilizadores().get(user).addEncomenda(enc); //adiciona encomenda ao user
        this.sistema.addEncomenda(enc); //adiciona encomenda ao sistema

        //esperar pela loja
        ILoja l = listLojas.get(enc.getVendedor());
        if(l.hasFila()) {
            InterfaceUtilizador.message("A loja vai demorar " + l.tempoEspera() + " minutos a ter a encomenda pronta\n");
            this.sistema.getLojas().get(loja).updateFila();
        }

        //associar a voluntario
        if(!aceitouVoluntario(enc,l))
            //se não conseguiu voluntário, associar a transportadora
            if(!aceitouEmpresa(enc,l)) InterfaceUtilizador.message("Desculpe, nenhum voluntário ou empresa se encontra disponível.\n");

    }

    /**
     * Determina se há algum voluntário disponível para fazer a entrega
     * @param enc encomenda
     * @param l loja
     * @return Se há algum voluntário disponível
     */
    private boolean aceitouVoluntario(IEncomenda enc, ILoja l) {
        Map<String, IVoluntario> voluntarios = this.sistema.getVoluntarios();

        List<IVoluntario> voluntariosDisponiveis = voluntarios.values().stream()
                .filter(v -> v.inRaio(l) && v.inRaio(user) && v.isLivre() && apto((Entregas) v,enc))
                .collect(Collectors.toList());

        if(!voluntariosDisponiveis.isEmpty()) { //se encontrou pelo menos um voluntário apto
            IVoluntario selecionado = voluntariosDisponiveis.get(0);
            String code = selecionado.getCode();

            enc.setData(LocalDateTime.now());
            voluntarios.get(code).setLivre(false);
            voluntarios.get(code).addEncomenda(enc);
            InterfaceUtilizador.voluntarioFound(code);

            return true;
        }
        return false;
    }

    /**
     * Determina se há alguma empresa transportadora disponível
     * @param enc encomenda
     * @param l loja
     * @return Se existe alguma empresa transportadora disponível
     */
    private boolean aceitouEmpresa(IEncomenda enc, ILoja l) throws ComandoDesconhecidoException {
        Map<String, IEmpresa> empresas = this.sistema.getTransportadoras();

        List<IEmpresa> empresasDisponiveis = empresas.values().stream()
                .filter(e -> e.inRaio(l) && e.inRaio(user) && e.isLivre() && apto((Entregas) e,enc))
                .collect(Collectors.toList());

        if(!empresasDisponiveis.isEmpty()) { //se encontrou pelo menos uma empresa apta
            IEmpresa selecionada = empresasDisponiveis.get(0);
            String code = selecionada.getCode();

            InterfaceUtilizador.message("Aceita pagar " + selecionada.custoEntrega(sistema,code) + " € pela entrega da sua encomenda? (S ou N) ");
            String soun = Input.lerString();

            if (soun.equals("S")) { //se user aceitou o preço
                enc.setData(LocalDateTime.now());

                empresas.get(code).setLivre(false); //empresa já não está disponível para mais entregas
                empresas.get(code).addEncomenda(enc); //encomenda adicionada à empresa

                InterfaceUtilizador.empresaFound(code);

                return true;
            }
            else if(soun.equals("N"))
                InterfaceUtilizador.message("Lamentamos informar mas a sua encomenda não tem transportadores disponíveis de momento...\n");
            else throw new ComandoDesconhecidoException("Comando inválido.\n");
        }
        return false;
    }

    /**
     * Verifica se a empresa ou voluntário pode transportar a encomenda, sendo ela de medicamentos
     * @param v empresa
     * @param e encomenda
     * @return Se pode transportar a encomenda ou não
     */
    private boolean apto(Entregas v, IEncomenda e) {
        return !e.getMedicine() || v.aceitoTransporteMedicamentos();
    }

    /**
     * Informação das entregas de um certo voluntário ou empresa
     */
    private void entregas() {

        InterfaceUtilizador.message("Pretente voluntário ou empresa transportadora? (V ou E) ");
        String opcao = Input.lerString();

        InterfaceUtilizador.message("Intervalo de tempo [mes1,mes2]:\n mes1: ");
        int mes1 = Input.lerInt();
        InterfaceUtilizador.message(" mes2: ");
        int mes2 = Input.lerInt();

        if(opcao.equals("V")) {
            Map<String, IVoluntario> voluntarios = this.sistema.getVoluntarios();

            InterfaceUtilizador.listaVoluntarios(voluntarios);
            InterfaceUtilizador.message(">> ");
            String v = Input.lerString();

            while (!voluntarios.containsKey(v)) {
                InterfaceUtilizador.message(">> ");
                v = Input.lerString();
            }
            for(IEncomenda e: voluntarios.get(v).getEncomendas().values())
                if(e.getData().getMonthValue() >= mes1 && e.getData().getMonthValue() <= mes2)
                    InterfaceUtilizador.printEncomenda(e);


        }
        else if(opcao.equals("E")) {

            Map<String,IEmpresa> empresas = this.sistema.getTransportadoras();

            InterfaceUtilizador.listaEmpresas(empresas);
            InterfaceUtilizador.message(">> ");
            String e = Input.lerString();

            while (!empresas.containsKey(e)) {
                InterfaceUtilizador.message(">> ");
                e = Input.lerString();
            }

            for(IEncomenda enc: empresas.get(e).getEncomendas().values())
                if(enc.getData().getMonthValue() >= mes1 && enc.getData().getMonthValue() <= mes2)
                    InterfaceUtilizador.printEncomenda(enc);
        }

    }

    /**
     * Classificar um voluntário ou empresa
     */
    private void classificar() {

        InterfaceUtilizador.message("Pretente voluntário ou empresa transportadora? (V ou E) ");
        String opcao = Input.lerString();

        if(opcao.equals("V")) {

            Map<String, IVoluntario> voluntarios = this.sistema.getVoluntarios();

            InterfaceUtilizador.listaVoluntarios(voluntarios);
            InterfaceUtilizador.message(">> ");
            String v = Input.lerString();

            while (!voluntarios.containsKey(v)) {
                InterfaceUtilizador.message(">> ");
                v = Input.lerString();
            }
            InterfaceUtilizador.message("Qual a classificação que pretende dar? (0-10) ");
            double rate = Input.lerDouble();

            voluntarios.get(v).setRating(rate);

            InterfaceUtilizador.message("Obrigado pelo seu feedback!\n");
        }
        else if(opcao.equals("E")) {

            Map<String,IEmpresa> empresas = this.sistema.getTransportadoras();

            InterfaceUtilizador.listaEmpresas(empresas);
            InterfaceUtilizador.message(">> ");
            String e = Input.lerString();

            while (!empresas.containsKey(e)) {
                InterfaceUtilizador.message(">> ");
                e = Input.lerString();
            }
            InterfaceUtilizador.message("Qual a classificação que pretende dar? (0-10) ");
            double rate = Input.lerDouble();

            empresas.get(e).setRating(rate);

            InterfaceUtilizador.message("Obrigado pelo seu feedback!\n");
        }
    }
}
