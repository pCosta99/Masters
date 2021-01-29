package Projeto.Views;
import Projeto.Avisos.AvisoEncomendaEntregue;
import Projeto.Avisos.AvisoOrcamentoRecebido;
import Projeto.Controllers.ControllerUtilizador;
import Projeto.Encomenda.LinhaDeEncomenda;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.ListaVaziaException;
import Projeto.Interfaces.*;
import Projeto.Util.Input;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Classe que implementa a View do Cliente.
 * É onde os dados solicitados do Model são exibidos.
 * É a camada de interface com o usuário.
 * É utilizada para receber a entrada de dados e apresentar visualmente o resultado.
 */
public class ViewUtilizador {
    private final ControllerUtilizador controller;

    /**
     * Construtor parametrizado da ViewCliente.
     */
    public ViewUtilizador(ControllerUtilizador c){
        this.controller = c;
    }

    /**
     * Método de I/O que apresenta ao cliente as varias açoes que ele pode efetuar.
     */
    public void menuUtilizador() {
        int ciclo = 0;
        this.trataNotificacoes();
        try {
            while (ciclo == 0) {
                System.out.println("O que pretende fazer?");
                System.out.println("\t1 -> Criar uma nova Encomenda.");
                System.out.println("\t2 -> Ver o histórico de Encomendas");
                System.out.println("\t3 -> Ver os 10 clientes que mais usaram esta APP");
                System.out.println("\t4 -> Classificar Voluntario/Empresa");
                System.out.println("\t5 -> Definiçoes");
                System.out.println("\t0 -> Logout");
                int opcao = Input.lerInt();
                switch (opcao) {
                    case 1:
                        this.criaEncomenda();
                        this.controller.gravar();
                        break;
                    case 2:
                        this.mostraHistorico();
                        break;
                    case 3:
                        this.mostraTop10();
                        break;
                    case 4:
                        this.classificar();
                    case 5:
                        this.definicoes();
                        break;
                    case 0:
                        ciclo = 1;
                        System.out.println("Volte sempre!");
                        break;
                    default:
                        System.out.println("Ups! Opção Inválida. A opção " + opcao + " não existe!");
                        break;
                }
            }
        } catch (ListaVaziaException | IOException exc) {
            System.out.println("Ups! " + exc.getMessage());
        }
    }

    /**
     * Método que percorre todas as notificações do Cliente e "trata-as", ou seja,
     * para o caso dos AvisoOrcamentoRecebido mostra o orçamento que a empresa fez pela entrega da sua encomenda
     * e para os outros simplesmente printa ao utilizador o que diz na notificação.
     */
    private void trataNotificacoes() {
        Collection<IAviso> avisos = this.controller.getAvisos();
        if(avisos.size() > 0) {
            for (IAviso a : avisos) {
                System.out.println(a.toString());
                if (a instanceof AvisoOrcamentoRecebido) {
                    System.out.println("Deseja aceitar? (true para aceitar, false para recusar)");
                    boolean sn = Input.lerBoolean();
                    this.controller.trataEncomendaEmpresa(a, sn);
                } else if (a instanceof AvisoEncomendaEntregue) {
                    AvisoEncomendaEntregue a2 = (AvisoEncomendaEntregue) a;
                    int avaliacao;
                    do {
                        System.out.println("Avalie a entrega da encomenda " + a2.getIdEncomenda() + " entregue por " + a2.getIdVoluntario() + " (0 a 10)");
                        avaliacao = Input.lerInt();
                        if (avaliacao > 10 || avaliacao < 0) System.out.println("Avaliação inválida!");
                    } while(avaliacao > 10 || avaliacao < 0);
                    this.controller.avalia(a2.getIdVoluntario(), avaliacao);
                }
                System.out.println("Prima enter para continuar a ver as notificações :)");
                Input.lerString();
                this.controller.removeNotificacao(a);
            }
        } else System.out.println("Nao tem notificações!!");
    }

    /**
     * Metodo que cria uma encomenda com um respetivo id, conforme a loja e produtos que o cliente selecionou.
     * A encomenda é depois guardada no registo do utilizador e no registo de encomendas do trazAqui, para que as empresas
     * consigam procurar e oferecer um orçamento.
     * Todos os voluntarios que "possam" fazer a entrega são notificados.
     */
    private void criaEncomenda() {
        Collection<String[]> lojas = this.controller.getLojas();
        System.out.println("Lojas disponiveis: ");
        for (String[] s : lojas) {
            System.out.println("\tID: " + s[0] + " -> " + s[1]);
        }
        System.out.println("Escolha uma loja (escreva o id da loja pretendida)");
        String lojaID = Input.lerString();

        try {
            List<IProduto> c = (List<IProduto>) this.controller.getLojaProds(lojaID);
            int i = 1;
            // Mostrar ao utilizador os produtos disponiveis na loja.
            System.out.println("Produtos disponiveis:");
            for(IProduto p : c) {
                System.out.println(i + " -> " + p.getNome() + " --> " + p.getPreco() + " euros.");
                i++;
            }
            i = 1;
            // O cliente tem de escolher os produtos que quer encomendar
            Collection<LinhaDeEncomenda> list = new ArrayList<>();
            while(i != 0) {
                System.out.println("Digite o numero do produto que deseja comprar (ou 0 para sair)");
                i = Input.lerInt();
                if (i > 0 && i < c.size()) {
                    System.out.println("Digite a quantidade que deseja comprar:");
                    int q = Input.lerInt();
                    if(q > 0){
                        LinhaDeEncomenda l = this.controller.criaLinhaEncomenda(c.get(i-1), q);
                        list.add(l);
                    }
                    else System.out.println("Não pode comprar " + q + " produtos!!");
                }
                else if (i != 0) System.out.println("A opcao " + i + " não é uma opção válida!");
            }
            IEncomenda e = this.controller.criaEncomenda(lojaID, list);
            System.out.println("A encomenda foi feita com sucesso e foi lhe atribuido o id => " + e.getID());
            System.out.println("Prima enter para continuar :)");
            Input.lerString();
        } catch (EntidadeNaoExisteException exc) {
            System.out.println("Ups! " + exc.getMessage());
            this.criaEncomenda();
        }

    }

    /**
     * Método que mostra ao Utilizador o seu histórico de compras.
     * E, caso o Utilizador queira, mostra os detalhes sobre todas as encomendas.
     */
    private void mostraHistorico() throws ListaVaziaException {
        System.out.println("Historico de Encomendas:");
        Collection<String> h = this.controller.historicoEncomendas();
        for (String s : h) {
            System.out.println("\t" + s);
        }
        try {
            System.out.println("Escreva o id de uma das compras para mais detalhes ou 0 para sair");
            String id = Input.lerString();
            if(!id.equals("0")) {
                String encomenda = this.controller.getEncomenda(id);
                System.out.println(encomenda);
            }
        } catch (EntidadeNaoExisteException exc) {
            System.out.println("Ups! " + exc.getMessage());
            this.mostraHistorico();
        }
    }

    /**
     * Método que mostra os 10 clientes que mais encomendas realizaram nesta app.
     */
    private void mostraTop10() {
        Collection<String> top = this.controller.top10Clientes();
        int i = 1;
        for(String s : top) {
            System.out.println(i + " -> " + s);
            i++;
        }
    }

    /**
     * Método que permite, ao Utilizador, alterar os seus dados pessoais.
     */
    private void definicoes() throws IOException {
        int opcao = 1;
        while(opcao != 0) {
            System.out.println("DEFINIÇÕES:");
            System.out.println("\t1 -> Mudar o nome");
            System.out.println("\t2 -> Mudar a localizacao ");
            System.out.println("\t3 -> Mudar o NIF");
            System.out.println("\t4 -> Ver o meu perfil");
            System.out.println("\t5 -> Apagar a conta");
            System.out.println("\t0 -> Voltar ao menu anterior");
            opcao = Input.lerInt();
            switch (opcao) {
                case 1:
                    System.out.println("Insira o seu novo nome:");
                    String nome = Input.lerString();
                    this.controller.changeName(nome);
                    this.controller.gravar();
                    break;
                case 2:
                    float lon = this.getLongitude();
                    float lat = this.getLatitude();
                    this.controller.changeLocal(lat, lon);
                    this.controller.gravar();
                    break;
                case 3:
                    System.out.println("Novo NIF:");
                    String nif = Input.lerString();
                    this.controller.changeNIF(nif);
                    this.controller.gravar();
                    break;
                case 4:
                    IUtilizador u = this.controller.getUtilizador();
                    System.out.println("ID: " + u.getId());
                    System.out.println("Nome: " + u.getNome());
                    System.out.println("NIF: " + u.getNIF());
                    System.out.println(u.getLocalizacao().toString());
                    System.out.println("Prima Enter para continuar");
                    Input.lerString();
                    break;
                case 5:
                    this.removerConta();
                    break;
                case 0:
                    opcao = 0;
                    break;
                default:
                    opcao = -1;
                    System.out.println("Opção inválida!!!");
                    break;
            }
        }
    }

    /**
     * Metodo que permite ao cliente classificar o voluntario ou empresa que efetuou o transporte da sua encomenda.
     */
    private void classificar() throws ListaVaziaException{
        System.out.println("Historico de Encomendas:");
        this.mostraHistorico();
        System.out.println("Escreva o id da encomenda que pretende classificar.");
        String idEnc = Input.lerString();
        System.out.println("Qual a classificaçao que pretende dar a este trabalhador?");
        int classificacao = Input.lerInt();
        this.controller.classificar(idEnc, classificacao);
    }

    /**
     * Método que permite ao Utilizador remover a conta.
     */
    private void removerConta() throws IOException {
        System.out.println("Tem a certeza que quer remover a conta? (true para remover, false para nao remover):");
        boolean remove = Input.lerBoolean();
        if(remove) {
            this.controller.removeConta();
            this.controller.gravar();
            System.out.println("Conta removida com sucesso!");
        }
    }

    /**
     * Método que pede a latitude ao utilizador e verifica se é válida.
     * Uma vez que isto era repetido várias vezes neste código decidimos
     * criar este método para evitar repetição e tornar o código mais perceptivel.
     */
    private float getLatitude() {
        float ret = -200;
        System.out.println("Introduza a sua latitude:");
        while (ret < -90 || ret > 90) {
            ret = Input.lerFloat();
            if(ret < -90 || ret > 90){
                System.out.println("Ups! Valor Inválido! Por favor insira um valor entre -90 e 90:");
            }
        }
        return ret;
    }

    /**
     * Método que pede a longitude ao utilizador e verifica se é válida.
     * Uma vez que isto era repetido várias vezes neste código decidimos
     * criar este método para evitar repetição e tornar o código mais perceptivel.
     */
    private float getLongitude() {
        float ret = -200;
        System.out.println("Introduza a sua longitude:");
        while (ret < -180 || ret > 180) {
            ret = Input.lerFloat();
            if(ret < -180 || ret > 180){
                System.out.println("Ups! Valor Inválido! Por favor insira um valor entre -180 e 180:");
            }
        }
        return ret;
    }
}