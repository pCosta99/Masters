package Projeto.Views;
import Projeto.Controllers.ControllerEmpresa;
import Projeto.Interfaces.*;
import Projeto.Util.Input;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Classe que implementa a View da Empresa.
 * A view provoca interações com o usuário, que interage com o Controller e
 * é onde os dados solicitados do Model são exibidos.
 */
public class ViewEmpresa {
    private final ControllerEmpresa controller;

    /**
     * Construtor parametrizado da ViewEmpresa
     */
    public ViewEmpresa(ControllerEmpresa controller) {
        this.controller = controller;
    }

    public void menuEmpresa() {
        int ciclo = 1;
        while (ciclo == 1) {
            try {
                System.out.println("\nBem vindo ao Menu da Empresa. Que tarefa pretende realizar?\n" +
                        "\n1 -> Notificacoes" +
                        "\n2 -> Transportar Encomendas" +
                        "\n3 -> Os N Clientes que mais aceitaram o transporte da empresa" +
                        "\n4 -> Verificar a distância total percorrida (em kms)" +
                        "\n5 -> As N empresas que mais utilizaram a aplicação (em distância percorrida)" +
                        "\n6 -> As N empresas que mais utilizaram a aplicação (em número de encomendas)" +
                        "\n7 -> Encomendas por aceitar" +
                        "\n8 -> Faturacao Periódica" +
                        "\n9 -> Definicoes" +
                        "\n0 -> Logout");
                int num = Input.lerInt();
                switch (num) {
                    case 1:
                        this.menuNotificacoes();
                        break;
                    case 2:
                        this.tranportarEncomenda();
                        break;
                    case 3:
                        System.out.println("Qual o valor para N?");
                        int n = Input.lerInt();
                        this.topNClientsEmpresa(n);
                        break;
                    case 4:
                        System.out.println("A empresa percorreu "
                                + this.controller.getDistTotalEmpresa() + " kms.");
                        System.out.println("Prima Enter para continuar!");
                        Input.lerString();
                        break;
                    case 5:
                        System.out.println("Qual o valor para N?");
                        int N = Input.lerInt();
                        Collection<String> emps = this.controller.getTopNEmpresasDist(N);
                        int i = 1;
                        for (String e : emps) {
                            System.out.println(i + " -> " + e);
                            i++;
                        }
                        break;
                    case 6:
                        System.out.println("Qual o valor para N?");
                        int valor = Input.lerInt();
                        Collection<String> col = this.controller.getTopNEmpresasEnc(valor);
                        int i2 = 1;
                        for (String e : col) {
                            System.out.println(i2 + " -> " + e);
                            i2++;
                        }
                        break;
                    case 7:
                        System.out.println("Lista de encomendas por aceitar");
                        this.encsPorTransportarEmpresa();
                        break;
                    case 8:
                        this.faturacaoPeriodica();
                        break;
                    case 9:
                        this.alteraDadosEmpresa();
                        ciclo = 0;
                        break;
                    case 0:
                        System.out.println("A sair...");
                        ciclo = 0;
                        break;
                    default:
                        System.out.println("Opcao inválida!");
                        break;
                }
            } catch (IOException e) {
                System.out.println("Ups! " + e.getMessage());
            }
        }
    }

    /**
     * Metodo que altera os dados da empresa, caso esta assim o pretenda.
     */
    private void alteraDadosEmpresa() throws IOException {
        System.out.println("Que dados pretende alterar?");
        System.out.println("1 -> Nome\n2 -> Localizacao\n3 -> Raio de acao\n4 -> Transporte de medicamentos\n" +
                "5 -> Capacidade de transporte de encomendas\n6 -> Taxa base\n7 -> NIF\n8 -> Apagar a conta");
        int dados = Input.lerInt();
        switch (dados) {
            case 1:
                System.out.println("Para que nome deseja alterar?");
                String nome = Input.lerString();
                this.controller.setNomeEmpresa(nome);
                this.controller.gravar();
                break;
            case 2:
                float lon = this.getLongitude();
                float lat = this.getLatitude();
                this.controller.setLocEmpresa(lat, lon);
                this.controller.gravar();
                break;
            case 3:
                System.out.println("Qual é o novo raio de acao?");
                float raio = Input.lerFloat();
                this.controller.setRaioEmpresa(raio);
                this.controller.gravar();
                break;
            case 4:
                boolean medic;
                if (!this.controller.getMedicEmpresa())
                    System.out.println("Já tem um certificado de transporte de medicamentos? (true caso sim, false caso não)");
                else System.out.println("Pretende continuar a transportar medicamentos? (true caso sim, false caso não)");
                medic = Input.lerBoolean();
                this.controller.setMedicEmpresa(medic);
                this.controller.gravar();
                break;
            case 5:
                System.out.println("Qual é a nova capacidade de transporte de encomendas");
                int cap = Input.lerInt();
                this.controller.setCapMaxEmpresa(cap);
                this.controller.gravar();
                break;
            case 6:
                System.out.println("Qual é a nova taxa base?");
                float taxa = Input.lerFloat();
                this.controller.setTaxaEmpresa(taxa);
                this.controller.gravar();
                break;

            case 7:
                System.out.println("Qual é o novo NIF?");
                String nif = Input.lerString();
                this.controller.setNifEmpresa(nif);
                this.controller.gravar();
                break;
            case 8:
                this.controller.apagaConta();
                System.out.println("Conta apagada com sucesso");
                break;
            default:
                System.out.println("Opcao inválida!");
                break;
        }
        System.out.println("Pretende alterar mais algum dado? (true caso sim, false caso não)");
        boolean changeAgain = Input.lerBoolean();
        if (changeAgain) alteraDadosEmpresa();
    }

    private void menuNotificacoes() throws IOException {
        int i = 1;
        if (this.controller.avisosEmpresa().isEmpty()) System.out.println("Não existem avisos de momento.");
        else {
            for (IAviso aviso : this.controller.avisosEmpresa()) {
                System.out.println(i + " -> " + aviso.toString());
                i++;
            }
            this.controller.gravar();
        }
    }

    /**
     * Metodo que retorna
     */
    public void topNClientsEmpresa(int n) {
        System.out.println("Lista de Clientes:");
        for(IUtilizador c : this.controller.topNClientesEmpresa(n))
            System.out.println("ID do Cliente: " + c.getId());
    }

    private void encsPorTransportarEmpresa() {
        int i = 1;
        Collection<IEncomenda> encs = this.controller.encsPorTransportar();
        for(IEncomenda enc : encs) {
            System.out.println(i + " -> " + enc.toString());
            i++;
        }
        System.out.println("\nQuais pretende transportar? (Responda p.e 1,2,3)");
        String input = Input.lerString();
        String[] campos = input.split(",");
        int limit;
        Collection<IEncomenda> chosenEncs = new ArrayList<>();
        for(String s : campos) {
            limit = Integer.parseInt(s);
            if (limit > encs.size()) System.out.println("Esse número não é valido!");
            else {
                IEncomenda e = this.getEncPos(limit, encs);
                if (e != null) chosenEncs.add(e);
            }
        }
        System.out.println("Escolheu as seguintes encomendas: ");
        for(IEncomenda e : chosenEncs) {
            System.out.println(e.getID());
            this.controller.userRecebeOrcamento(e);
        }
    }

    public IEncomenda getEncPos(int pos, Collection<IEncomenda> col) {
        Iterator<IEncomenda> it = col.iterator();
        IEncomenda enc = null;
        for (; pos > 0 && it.hasNext(); pos--)
            enc = it.next();
        return enc;
    }

    private void faturacaoPeriodica() {
        System.out.println("Qual das opcoes pretende escolher?" +
                "\n1 -> Faturacao dentro de um intervalo de tempo" +
                "\n2 -> Faturacao num determinado tempo (ano, dia ou mes)");
        int opcao = Input.lerInt();
        switch (opcao) {
            case 1:
                fatIntervalo();
                break;
            case 2:
                fatTempo();
                break;
            default:
                System.out.println("Opcao inválida!");
                break;
        }
    }

    private void fatIntervalo() {
        System.out.println("Introduza o tempo inicial: (Formato: AAAA-MM-DD");
        String tempoI = Input.lerString();
        String[] tInicial = tempoI.split("-");
        int[] ti = new int[3];
        for(int i = 0; i < 3; i++)
            ti[i] = Integer.parseInt(tInicial[i]);
        System.out.println("Introduza o tempo final: (Formato: AAAA-MM-DD");
        String tempoF = Input.lerString();
        String[] tFinal = tempoF.split("-");
        int[] tf = new int[3];
        for(int i = 0; i < 3; i++)
            tf[i] = Integer.parseInt(tFinal[i]);
        float res = this.controller.getTotalFaturadoPeriodo(ti, tf);
        System.out.println("O total faturado nesse intervalo é de " + res);
    }

    public void fatTempo() {
        System.out.println("Introduza o tempo: (Formato: AAAA-MM-DD");
        String tempo = Input.lerString();
        String[] t = tempo.split("-");
        int[] newT = new int[3];
        for(int i = 0; i < 3; i++)
            newT[i] = Integer.parseInt(t[i]);
        float r = this.controller.getTotalFaturadoPeriodo(newT);
        System.out.println("O total faturado nesse intervalo é de " + r);
    }

    /**
     * Método que pede a latitude ao utilizador.
     * Uma vez que isto era repetido várias vezes neste código decidimos
     * criar este método para evitar repetição e tornar o código mais perceptivel.
     */
    private float getLatitude() {
        float ret = -200;
        System.out.println("Introduza a sua latitude:");
        while (ret < -90 || ret > 90) {
            ret = Input.lerFloat();
            if(ret < -90 || ret > 90) System.out.println("Ups! Valor Inválido! Por favor insira um valor entre -90 e 90:");
        }
        return ret;
    }

    /**
     * Método que pede a longitude ao utilizador.
     * Uma vez que isto era repetido várias vezes neste código decidimos
     * criar este método para evitar repetição e tornar o código mais perceptivel.
     */
    private float getLongitude() {
        float ret = -200;
        System.out.println("Introduza a sua longitude:");
        while (ret < -180 || ret > 180) {
            ret = Input.lerFloat();
            if(ret < -180 || ret > 180) System.out.println("Ups! Valor Inválido! Por favor insira um valor entre -180 e 180:");
        }
        return ret;
    }

    private void tranportarEncomenda() throws IOException {
        Collection<IEncomenda> encs = this.controller.getEncomendasPT();
        // Primeiro tenho de ver se ele tem encomendas para entregar.
        if(!encs.isEmpty()) {
            int i = 0;
            for (IEncomenda e : encs) {
                System.out.println(i + " -> " + e.getID());
                i++;
            }
            String escolhas = Input.lerString();
            String[] campos = escolhas.split(",");
            List<IEncomenda> chosenEncs = new ArrayList<>();
            int limit;
            Iterator<IEncomenda> it = encs.iterator();
            for (String s : campos) {
                limit = Integer.parseInt(s);
                IEncomenda encomenda = it.next();
                for (; limit > 0 && it.hasNext(); limit--, it.next())
                    encomenda = it.next();
                chosenEncs.add(encomenda);
            }
            float dist = 0;
            for (IEncomenda e : chosenEncs) {
                dist += this.transportePara(e);
            }
            this.controller.atualizaDist(dist);
            this.controller.gravar();
            System.out.println("Já foram entregues todas a encomendas");
        }  else {
            System.out.println("Não tem encomendas para entregar!");
        }
    }

    private float transportePara(IEncomenda e) {
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
        IUtilizador u = this.controller.getCliente(e.getUserID());
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
        float velo = this.controller.calculaVel(tempo, u, l);
        System.out.println("A sua velocidade foi de " + velo);
        this.controller.insereVelControl(velo);
        this.controller.notifyUser(u, e);
        this.controller.switch1(e);
        return velo * tempo;
    }
}