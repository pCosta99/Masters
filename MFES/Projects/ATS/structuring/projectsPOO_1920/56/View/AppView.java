package View;

import Model.Catalogos.ICatalogoProds;
import Model.Encomendas.Entrega;
import Model.Encomendas.IEncomenda;
import Model.Encomendas.IEntrega;
import Model.Encomendas.ILinhaEncomenda;
import Model.Tipos.Empresa;
import Model.Tipos.ITipo;
import Model.Tipos.Voluntario;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class AppView implements IAppView {

    public void preInicio(){
        System.out.println();
        System.out.println("-----------Bem-Vindo!!-----------");
        System.out.println("1- Ler um ficheiro .txt");
        System.out.println("2- Carregar estado do programa");
        System.out.println("0- Sair do aplicativo");
    }

    public void inicio () {
        System.out.println();
        System.out.println("---------TRAZ AQUI!--------");
        System.out.println("Prima:");
        System.out.println("1- Fazer Registo"); //adiciona ao estado
        System.out.println("2- Fazer Login "); //retoma o estado do aplicativo
        System.out.println("3- Alterar a password");
        System.out.println("4- Guardar estado do programa");
        System.out.println("5- Ver os Top10 Users de acordo com o nº de encomendas feitas");
        System.out.println("6- Ver as Top10Empresas de acordo com a dist. percorrida");
        System.out.println("0- Sair do aplicativo");
    }

    public void login(int x){
        if(x==0) System.out.println("-----------LOGIN!----------");
        if(x==1) System.out.println("Insira o seu email:");
        if(x==3) System.out.println("Password ou email incorreto. Tente novamente.");
        if(x==2) System.out.println("Insira a sua password:");
    }

    public void reg (int x){
        if (x==1){
            System.out.println("Insira o nome ");
        }
        if (x==2){
            System.out.println("Insira a latitude");
        }
        if (x==3){
            System.out.println("Insira a longitude");
        }
        if (x==4) {
            System.out.println("Insira o raio de ação");
        }
        if(x==5) {
            System.out.println("Insira o NIF");
        }
        if(x==6){
            System.out.println("Insira o preço por km");
        }

    }

    public void registo (){
        System.out.println("---------TRAZ AQUI!--------");
        System.out.println("Fazer registo de:");
        System.out.println("1- Utilizador");
        System.out.println("2- Voluntário");
        System.out.println("3- Loja");
        System.out.println("4- Empresa");
        System.out.println("0- Voltar ao Menu Inicial");
    }

    public void Voluntariomode(){
        System.out.println("-----------VOLUNTÁRIO MODE----------");
        System.out.println("1- Encomendas à espera da validação");
        System.out.println("2- Entregar/Recusar Encomenda ao Utilizador");
        System.out.println("3- Histórico de entregas");
        System.out.println("4- Classificações recebidas pelos Users");
        System.out.println("0- Voltar ao Menu de Login");
    }

    public void userMode(){
        System.out.println("-----------USER MODE----------");
        System.out.println("1- Fazer uma encomenda");
        System.out.println("2- Escolher o transporte");
        System.out.println("3- Classificar transporte + Recibo");
        System.out.println("4- Encomendas em espera de Transporte");
        System.out.println("5- Histórico");
        System.out.println("0- Voltar ao Menu de Login");
    }

    public void modeLoja(){
        System.out.println("-----------LOJA MODE----------");
        System.out.println("1- Encomendas em fila de espera");
        System.out.println("2- Validar uma encomenda");
        System.out.println("3- Encomendas já prontas à espera da transportadora");
        System.out.println("0- Voltar ao Menu de Login");
    }

    public void modeEmpresa(){
        System.out.println("-----------EMPRESA MODE----------");
        System.out.println("1- Encomendas à espera da validação");
        System.out.println("2- Entregar/Recusar Encomenda ao Utilizador");
        System.out.println("3- Histórico de entregas");
        System.out.println("4- Classificações recebidas pelos Users");
        System.out.println("5- Total Faturado num período");
        System.out.println("0- Voltar ao Menu de Login");
    }

    public void encomendasEspera(Set<IEncomenda> res){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                ENCOMENDAS NA FILA DE ESPERA");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Utilizador                               Código da Encomenda");
        for(IEncomenda enco : res){
            System.out.printf("%-38s",enco.getUserID());
            System.out.print(enco.getEncomendaID());
            System.out.println();
        }
    }

    public void encomendasFila(Set<IEntrega> res){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                ENCOMENDAS PARA TRANSPORTE");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Código da Encomenda  Medicamento   Congelados   Hora Inicial");
        for(IEntrega enco : res){
            System.out.printf("%-20s",enco.getEncomenda().getEncomendaID());
            System.out.printf("%-20s",enco.getEncomenda().getMedicamentos());
            System.out.printf("%-20s",enco.getEncomenda().getCongelados());
            System.out.print(enco.getEncomenda().getHoraInicial());
            System.out.println();
        }
    }

    public void listagem(IEntrega[] res){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                   HISTÓRICO DE ENTREGAS");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Código     Distância Percorrida       Data           Hora   ");
        for(int i=0; i<res.length; i++){
            System.out.printf("%-20s",res[i].getEncomenda().getEncomendaID());
            System.out.printf("%-20s",res[i].getDistPercorrida());
            System.out.printf("%-20s",res[i].getDataEntrega());
            System.out.print(res[i].getHoraEntrega());
            System.out.println();
        }
    }

    public void prepara(int x){
        if(x==0){
            System.out.println("Aceita transportar medicamentos?");
            System.out.println("(Y) Yes                  (N) No ");
        }
        if(x==1){
            System.out.println("Está a chover ou está trânsito ?");
            System.out.println("(Y) Yes                  (N) No ");
        }
        if(x==2) System.out.println("Quantas encomendas consegue transportar?");
        if(x==3) System.out.println("Qual é a taxa adicional?");
    }


    public void transportes(char opcao, HashSet<ITipo> res){
        int x = 0x2713;
        if(opcao== ' ') {
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            System.out.println("                   OPÇÕES DE TRANSPORTE");
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");

        }
        if(opcao=='V') {
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            System.out.println("VOLUNTÁRIOS       CÓDIGO      RAIO DE AÇÃO           PREÇO  ");
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            for(ITipo tipo : res){
                if(tipo instanceof Voluntario){
                    Voluntario vol = (Voluntario) tipo;
                    System.out.printf("%-15s",vol.getNome());
                    System.out.printf("    %-15s",vol.getId());
                    if(vol.getAvailability()){
                        String check = "✓";
                        System.out.printf("%-15s",check);
                    }
                    else {
                        String no = "x";
                        System.out.printf("%-15s",no);
                    }
                    String preco = "     Grátis";
                    System.out.printf("%-15s",preco);
                    System.out.println();
                }
            }
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            System.out.println("(E) Escolha um meio de transporte");
        }
        else if(opcao =='T'){
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            System.out.println("TRANSPORTADORAS      CÓDIGO     DISPONIBILIDADE        PREÇO");
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            for(ITipo tipo : res){
                if(tipo instanceof Empresa){
                    Empresa emp = (Empresa) tipo;
                    System.out.printf("%-15s",emp.getNome());
                    System.out.printf("%-25s",emp.getId());
                    if(emp.getDisponibilidade()){
                        String check = "✓";
                        System.out.printf("%-15s",check);
                    }
                    else {
                        String no = "x";
                        System.out.printf("%-15s",no);
                    }
                    System.out.printf("%-15s",emp.getPreco());
                    System.out.println();
                }
            }
            System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
            System.out.println("(E) Escolha um meio de transporte");
        }
        else if(opcao == 'M'){
            System.out.println("Obrigada!! Dentro de minutos a encomenda será entregue!\nAté já!");
        }
        //else System.out.println("Opção inválida! Escolha novamente");
        if(opcao != 'E') System.out.println("\n(V) Voluntários       (M) Menu           (T) Transportadoras");
    }


    public void classificacao(){
        System.out.println("-----------CLASSIFICAÇÃO----------");
        System.out.println("1- ★");
        System.out.println("2- ★★");
        System.out.println("3- ★★★");
        System.out.println("4- ★★★★");
        System.out.println("5- ★★★★★");
        System.out.println("0- Voltar ao Menu");
    }


    public void classificacoes(HashMap<String,Integer> res){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                     CLASSIFICAÇÕES");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Código do User                                 Classificação");
        for(String u : res.keySet()){
            System.out.printf("%-45s",u);
            if(res.get(u)==1){
                System.out.print("★");
            }
            if(res.get(u)==2){
                System.out.print("★★");
            }
            if(res.get(u)==3){
                System.out.print("★★★");
            }
            if(res.get(u)==4){
                System.out.print("★★★★");
            }
            if(res.get(u)==5){
                System.out.print("★★★★★");
            }
            System.out.println();
        }
    }

    public void infos(){
        System.out.println("-----------LOJA MODE----------");
        System.out.println("Quanto tempo demora, em média, em minutos, a atender uma pessoa?");
    }


    public void tipodeEncomenda(int x) {
        if (x==1) {
            System.out.println("Algum destes produtos é medicamento?");
            System.out.println("1- Sim");
            System.out.println("2- Não");
        }
        if(x==2) {
            System.out.println("Algum destes produtos é congelado?");
            System.out.println("1- Sim");
            System.out.println("2- Não");
        }
        if(x==3){
            System.out.println("Insira o peso da encomenda: ");
        }
    }

    public void printMensagem(String s){
        System.out.printf("\n%s\n",s);
    }

    public void top (ITipo[] res, Integer[] n){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                      TOP 10 USERS");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Código do User                              Nº de Encomendas");
        for(int i=0; i<10; i++){
            System.out.printf("%-45s",res[i].getId());
            System.out.print(n[i]);
            System.out.println();
        }
    }

    public void top2 (String[] res, Float[] dist){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                      TOP 10 EMPRESAS");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Código da Empresa                       Distancia Percorrida");
        for(int i=0; (i<10)&&(i<res.length); i++){
            System.out.printf("%-45s",res[i]);
            System.out.print(dist[i]);
            System.out.println();
        }
    }

    public void lista (List<IEntrega> res) {
        System.out.println(res);
    }

    public void vendasLoja(Set<IEncomenda> res){
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("                      VENDAS FEITAS");
        System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
        System.out.println("Código da Encomenda   Medicamento   Congelados     Cod.User ");
        for(IEncomenda enco : res){
            System.out.printf("%-20s",enco.getEncomendaID());
            System.out.printf("%-20s",enco.getMedicamentos());
            System.out.printf("%-20s",enco.getCongelados());
            System.out.print(enco.getUserID());
            System.out.println();
        }
    }

}
