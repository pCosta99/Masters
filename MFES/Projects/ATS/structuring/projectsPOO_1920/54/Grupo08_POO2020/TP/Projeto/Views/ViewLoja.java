package Projeto.Views;
import Projeto.Controllers.ControllerLoja;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.ListaVaziaException;
import Projeto.Interfaces.IEncomenda;
import Projeto.Interfaces.IProduto;
import Projeto.Util.Input;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

/**
 * Classe que implementa a View da Loja.
 * É onde os dados solicitados do Model são exibidos.
 * É a camada de interface com o usuário.
 * É utilizada para receber a entrada de dados e apresentar visualmente o resultado.
 */
public class ViewLoja {
    private final ControllerLoja controller;

    /**
     * Construtor parametrizado da ViewLoja.
     */
    public ViewLoja(ControllerLoja c) {
        this.controller = c;
    }

    /**
     * Menu principal que mostra ao utilizador o que pode fazer e como fazer
     */
    public void menuLojas() {
        int ciclo = 0;
        while (ciclo == 0) {
            try {
                System.out.println("O que pretende fazer?");
                System.out.println("\t1 -> Declarar uma encomenda como pronta!");
                System.out.println("\t2 -> Adicionar produtos à lista de produtos");
                System.out.println("\t3 -> Remover produtos da lista de produtos");
                System.out.println("\t4 -> Alterar o tamanho da fila");
                System.out.println("\t5 -> Ver os produtos prontos para entregar");
                System.out.println("\t6 -> Alterar dados");
                System.out.println("\t0 -> Logout");
                int opcao = Input.lerInt();
                switch (opcao) {
                    case 1:
                        this.prontarEnc();
                        break;
                    case 2:
                        this.adicionaProdutos();
                        System.out.println("Produto adicionado!");
                        this.controller.gravar();
                        break;
                    case 3:
                        this.removeProdutos();
                        System.out.println("Produto Removido!");
                        this.controller.gravar();
                        break;
                    case 4:
                        System.out.println("Quantas pessoas estao de momento na fila?");
                        int tamanho_fila = Input.lerInt();
                        this.controller.qtsPessoasAtual(tamanho_fila);
                        this.controller.gravar();
                        break;
                    case 5:
                        System.out.println("Produtos prontos para entregar:");
                        Collection<String> h = this.controller.historicoEncomendas();
                        for (String s : h) {
                            System.out.println("\t" + s);
                        }
                        try {
                            System.out.println("Escreva o id de uma das compras para mais detalhes ou 0 para sair");
                            String id = Input.lerString();
                            String encomenda = this.controller.getEncomenda(id);
                            System.out.println(encomenda);
                        } catch (EntidadeNaoExisteException exc) {
                            System.out.println(exc.getMessage());
                        }
                        break;
                    case 6:
                        boolean sair = this.alteraDadosLoja();
                        if(sair) ciclo = 1;
                        break;
                    case 0:
                        ciclo = 1;
                        break;
                    default:
                        System.out.println("Ups! Opção Inválida. A opção " + opcao + " não existe!");
                        break;
                }
            } catch (ListaVaziaException | IOException exc) {
                System.out.println("Ups! " + exc.getMessage());
            }
        }
    }

    private void prontarEnc() throws IOException {
        int i = 1;
        List<IEncomenda> encs = (List<IEncomenda>) this.controller.getEncomendas();
        for(IEncomenda e : encs) {
            System.out.println(i + " -> " + e.getID());
            i++;
        }
        System.out.println("Qual é a encomenda que já está pronta? (0 para sair)");
        int pronta = Input.lerInt();
        if(pronta > 0 && pronta <= encs.size()) {
            IEncomenda e = encs.get(pronta - 1);
            int total = this.controller.prontarEnc(e);
            System.out.println(total + " voluntarios receberam notificação sobre esta encomenda!");
            this.controller.gravar();
        }
    }

    /**
     * Metodo que adiciona produtos à lista de produtos de uma dada loja.
     */
    private void adicionaProdutos () {
        double peso = 0; float preco = 0;
        System.out.println("Insira o nome produto que pretende adicionar à lista de produtos?");
        String nome = Input.lerString();
        System.out.println("Insira o peso produto que pretende adicionar à lista de produtos?");
        while (peso <= 0) {
            peso = Input.lerDouble();
            if(peso <= 0) System.out.println("Um produto não pode ter peso " + peso +"\n Insira um valor válido:");
        }
        System.out.println("Insira o preço produto que pretende adicionar à lista de produtos?");
        while(preco <= 0) {
            preco = Input.lerFloat();
            if(preco <= 0) System.out.println("Um produto não pode ter preco " + preco +"\n Insira um valor válido:");
        }
        System.out.println("O produto que pretende adicionar é um produto medicinal? (true para sim, false para nao)");
        boolean medicinal = Input.lerBoolean();
        this.controller.adicionaProdutos(nome, peso, preco, medicinal, 0);
    }

    /**
     * Metodo que remove produtos da lista de produtos de uma dada loja.
     */
    private void removeProdutos() {
        Collection<IProduto> prods = this.controller.getProdutos();
        for(IProduto p : prods) System.out.println(" -> " + p.getCodigo() + " " + p.getNome());
        System.out.println("Insira o codigo do produto que pretende remover da lista de produtos?");
        String codigo = Input.lerString();
        // Aqui devia se verificar se o produto existe mas nao sei fazer isso.
        this.controller.removeProdutoControl(codigo);
    }

    /**
     * Metodo que altera os dados da loja, caso esta assim o pretenda.
     */
    private boolean alteraDadosLoja() throws IOException {
        boolean r = false;
        System.out.println("Que dados pretende alterar?");
        System.out.println("1 -> Nome\n2 -> Localizacao\n3 -> Informacao sobre os dados da fila" +
                "\n4 -> Tempo medio de atendimento\n5 -> Apagar a Conta");
        int dados = Input.lerInt();
        switch (dados) {
            case 1:
                System.out.println("Para que nome deseja alterar?");
                String nome = Input.lerString();
                this.controller.setNomeLoja(nome);
                this.controller.gravar();
                break;
            case 2:
                float lon = this.getLongitude();
                float lat = this.getLatitude();
                this.controller.setLocLoja(lat, lon);
                this.controller.gravar();
                break;
            case 3:
                System.out.println("Pretende fornecer dados sobre a fila? (true para sim, false para nao)");
                boolean dadosFila = Input.lerBoolean();
                this.controller.setDFilaControl(dadosFila);
                this.controller.gravar();
                break;
            case 4:
                float tempA = 0;
                System.out.println("Qual é o novo tempo de atendimento medio?");
                while(tempA <= 0) {
                    tempA = Input.lerFloat();
                    if( tempA <= 0) System.out.println("O tempo médio nao pode ser " + tempA + "\nInsira um valor válido:");
                }
                this.controller.setTempMedControl(tempA);
                this.controller.gravar();
                break;
            case 5:
                System.out.println("Tem a certeza que deseja apagar a conta? (true para sim, false para nao)");
                r = Input.lerBoolean();
                if(r) {
                    this.controller.removeConta();
                    System.out.println("Conta apagada com sucesso!");
                }
                this.controller.gravar();
                break;
            default:
                System.out.println("Opcao inválida!");
                break;
        }
        if(!r) {
            System.out.println("Pretende alterar mais algum dado? (true caso sim, false caso não)");
            boolean changeAgain = Input.lerBoolean();
            if (changeAgain) alteraDadosLoja();
        }
        return r;
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