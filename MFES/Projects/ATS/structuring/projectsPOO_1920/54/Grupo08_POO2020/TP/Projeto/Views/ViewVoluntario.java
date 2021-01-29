package Projeto.Views;

import Projeto.Avisos.AvisoVoluntario;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.ListaVaziaException;
import Projeto.Interfaces.IAviso;
import Projeto.Interfaces.IEncomenda;
import Projeto.Interfaces.ILoja;
import Projeto.Interfaces.IUtilizador;
import Projeto.Util.Input;
import Projeto.Controllers.ControllerVoluntario;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

/**
 * Classe que implementa a View do Voluntario.
 * A view provoca interações com o usuário, que interage com o Controller e
 * é onde os dados solicitados do Model são exibidos.
 */
public class ViewVoluntario {
    private final ControllerVoluntario controller;

    /**
     * Construtor parametrizado da ViewVoluntario
     */
    public ViewVoluntario(ControllerVoluntario c) {
        this.controller = c;
    }

    /**
     * Método de I/O que apresenta ao voluntario as varias açoes que ele pode efetuar.
     */
    public void menuVoluntario(){
        int ciclo = 0;
        while (ciclo == 0) {
            try {
                System.out.println("O que pretende fazer?");
                System.out.println("\t1 -> Fazer a entrega de uma encomenda");
                System.out.println("\t2 -> Ver o histórico de encomendas que realizou");
                System.out.println("\t3 -> Alterar dados");
                System.out.println("\t4 -> Mostrar avisos");
                System.out.println("\t0 -> Logout");
                int opcao = Input.lerInt();
                switch (opcao) {
                    case 1:
                        this.fazEntrega();
                        break;
                    case 2:
                        this.mostraHistorico();
                        break;
                    case 3:
                        this.alteraDadosVoluntario();
                        break;
                    case 4:
                        this.avisosVoluntario();
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

    /**
     * Metodo que processa a entrega da encomenda que o voluntario realizará.
     */
    private void fazEntrega() throws IOException {
        System.out.println("Neste momento, esta disponivel para fazer entregas? (true caso sim, false caso não)");
        System.out.println("Enquanto faz a entrega não pode utilizar mais nada na app.");
        boolean dispEntregas = Input.lerBoolean();
        if (dispEntregas) {
            this.controller.changeEstado();
            // Verificamos se a lista de encomendas do utilizador está vazia ou se tem encomendas por entregar
            if(this.controller.estaVaziaControl()) System.out.println("De momento nao existem encomendas a ser entregues!");
            else {
                // Tenho de percorrer a lista de encomendas e o voluntário escolhe qual vai entregar
                List<IEncomenda> encs = (List<IEncomenda>) this.controller.getEncsPorEntregar();
                int i = 1;
                for (IEncomenda s : encs) {
                    System.out.println(i + " -> " + s.getID());
                    i++;
                }
                System.out.println("Escolha qual quer transportar!");
                int enc = Input.lerInt();
                IEncomenda e = encs.get(enc - 1);
                ILoja l = this.controller.getLoja(e.getLojaID());
                System.out.println("A encomenda está na loja " + l.getId() + " cuja localização é \n" +
                        l.getLocalizacao().toString());
                System.out.println("Quando chegar à loja escreva ok!");
                String ok = "";
                while(!ok.equals("ok")) {
                    ok = Input.lerString();
                    if(!ok.equals("ok")) System.out.println("Escreva ok quando chegar (tudo em minusculas)");
                }
                System.out.println("Quanto tempo demorou a chegar à loja (em minutos)");
                float tempChegarLoja = Input.lerFloat();

                // Aqui ele já chegou à loja
                System.out.println("Quando pegar na encomenda escreva ok!"); ok = "";
                while(!ok.equals("ok")) {
                    ok = Input.lerString();
                    if(!ok.equals("ok")) System.out.println("Escreva ok quando tiver a encomenda (tudo em minusculas)");
                }
                IUtilizador u = this.controller.getClienteControl(e.getUserID());
                System.out.println("Agora tem de levar a encomenda para \n" + u.getLocalizacao().toString());
                // Aqui está se a dirigir para a casa do utilizador
                System.out.println("Quando entregar a encomenda escreva ok!"); ok = "";
                while(!ok.equals("ok")) {
                    ok = Input.lerString();
                    if(!ok.equals("ok")) System.out.println("Escreva ok quando entregar a encomenda (tudo em minusculas)");
                }
                System.out.println("Quanto tempo demorou desde a loja até ao local da entrega (em minutos)");
                float tempoLojaCasa = Input.lerFloat();
                float tempo = tempoLojaCasa + tempChegarLoja;
                System.out.println("Demorou " + tempo + " minutos a fazer a entrega");
                float velo = this.controller.calculaVelo(tempo, u, l);
                System.out.println("A sua velocidade foi de " + velo);
                this.controller.insereVelControl(velo);
                this.controller.notifyUser(u, e);
                this.controller.switch1(e);

                this.controller.grava();
            }
        }
    }

    /**
     * Método que mostra o historico de compras ao utilizador.
     */
    private void mostraHistorico() throws ListaVaziaException {
        System.out.println("Historico de Pedidos:");
        Collection<String> h = this.controller.historicoEncomendas();
        for (String s : h) {
            System.out.println("\t" + s);
        }
        try {
            System.out.println("Escreva o id de uma das compras para mais detalhes ou 0 para sair");
            String id = Input.lerString();
            if(!id.equals("0")) {
                IEncomenda encomenda = this.controller.getEncomenda(id);
                System.out.println(encomenda.toString());
            }
        } catch (EntidadeNaoExisteException exc) {
            System.out.println("Ups! " + exc.getMessage());
        }
    }

    /**
     * Metodo que altera os dados do voluntario caso ele assim o pretenda.
     */
    private void alteraDadosVoluntario() throws IOException {
        System.out.println("Que dados pretende alterar?");
        System.out.println("1 -> Nome\n2 -> Localizacao\n3 -> Raio de ação\n4 -> Transporte de medicamentos\n" +
                "5 -> Capacidade de transporte de encomendas\n 6 -> Apagar a conta");
        int dados = Input.lerInt();
        switch (dados) {
            case 1:
                System.out.println("Para que nome deseja alterar?");
                String nome = Input.lerString();
                this.controller.setNomeVoluntario(nome);
                this.controller.grava();
                break;
            case 2:
                float lon = this.getLongitude();
                float lat = this.getLatitude();
                this.controller.setLocVoluntario(lat, lon);
                this.controller.grava();
                break;
            case 3:
                float raio = 0;
                System.out.println("Qual é o novo raio de acao?");
                while(raio <= 0) {
                    raio = Input.lerFloat();
                    if(raio <= 0) System.out.println("O raio nao pode tomar o valor de " + raio + "\nInsira um valor válido!");
                }
                this.controller.setRaioVoluntario(raio);
                this.controller.grava();
                break;
            case 4:
                boolean medic;
                if (!this.controller.getMedicControl()) {
                    System.out.println("Já tem um certificado de transporte de medicamentos? (true caso sim, false caso não)");
                }
                else {
                    System.out.println("Pretende deixar de transportar medicamentos? (true caso sim, false caso não)");
                }
                medic = Input.lerBoolean();
                this.controller.setMedicVoluntario(medic);
                this.controller.grava();
                break;
            case 5:
                int cap = 0;
                System.out.println("Qual é a nova capacidade de transporte de encomendas");
                while(cap <= 0) {
                    cap = Input.lerInt();
                    if(cap <= 0) System.out.println("Nao pode transportar " + cap +"\nInsira um valor válido!");
                }
                this.controller.setCapMaxVoluntario(cap);
                this.controller.grava();
                break;
            case 6:
                this.controller.apagaConta();
                break;
            default:
                System.out.println("Opcao inválida!");
                break;
        }
        System.out.println("Pretende alterar mais algum dado? (true caso sim, false caso não)");
        boolean changeAgain = Input.lerBoolean();
        if (changeAgain) alteraDadosVoluntario();
    }

    /**
     * Método que trata dos avisos do Voluntário.
     */
    private void avisosVoluntario() throws IOException {
        Collection<IAviso> col = this.controller.getAvisosVols();
        if (col.isEmpty()) System.out.println("Não existem avisos de momento.");
        else {
            System.out.println("Lista de Avisos: ");
            for (IAviso aviso : col) {
                AvisoVoluntario a = (AvisoVoluntario) aviso;
                if(this.controller.trataAviso(a)) {
                    System.out.println("Deseja entregar a encomenda " + a.getIdEncomenda() + " ?");
                    boolean aceita = Input.lerBoolean();
                    if(aceita) {
                        this.controller.pegaEncomenda(a);
                    }
                }
                this.controller.remNotificacao(a);
            }
            this.controller.grava();
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
