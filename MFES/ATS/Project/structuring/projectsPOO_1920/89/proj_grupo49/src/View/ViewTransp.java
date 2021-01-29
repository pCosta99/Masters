package View;

import Model.Encomenda;
import Model.Loja;
import Model.Transportadora;
import Model.Utilizador;

import java.time.LocalDateTime;
import java.util.List;

public class ViewTransp {

    public void menuTransp(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Aceitar Entrega                                                                       |");
        System.out.println("|2  -> Ver historico de encomendas  entregues                                                |");
        System.out.println("|3  -> Ver encomendas por entregar                                                           |");
        System.out.println("|4  -> Alterar dados pessoais                                                                |");
        System.out.println("|5  -> Encomenda entregue                                                                    |");
        System.out.println("|6  -> Ver total faturado entre 2 datas                                                      |");
        System.out.println("|0  -> Exit Program                                                                          |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void printBarraN(){
        System.out.println();
    }
    public void flush(){
        System.out.println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
    }

    public void pressioneEnter(){
        System.out.println("\nPressione enter para continuar");
    }

    public void printExit(){
        System.out.println("\nObrigado por usar GestVendasApp!");
    }

    public void inst(){
        System.out.println("\nEscolha a instrução: ");
    }

    public void printVazia(){System.out.println("Nao existem pedidos de momento.");}

    public void printError(){
        System.out.println("\nNão escreveu nenhuma das duas opções válidas!");
        System.out.print("Por favor tente outra vez: ");
    }

    public void printEntregue(){System.out.println("Insira o código da encomenda que entregou:");}

    public void aceitar(){
        System.out.println("Deseja aceitar entregar a encomenda:");
    }

    public void esperaUaceitar(){
        System.out.println("Necessário aceitação do utilizador. Por favor aguarde!");
    }

    public void opc2(List<Encomenda> e){
        if (e.size() <= 0){
            System.out.println("LISTA VAZIA!");
        }
        else {
            System.out.println("Codigo:");
            for (Encomenda es : e) {
                if (es.getEntregue()) System.out.println(es.getCodenc());
            }
        }
    }

    public void opc3(List<Encomenda> e){
        if (e.size() <= 0){
            System.out.println("LISTA VAZIA!");
        }
        else {
            System.out.println("Codigo:");
            for (Encomenda es : e) {
                if (!es.getEntregue()) System.out.println(es.getCodenc());
            }
        }
    }
    public void empetyList(){
       System.out.println("LISTA VAZIA!");
    }
    public void getEncomendas(){
        System.out.println("Encomendas: ");
    }

    public void op3_aux(Encomenda e , Loja l, Utilizador u){
        System.out.println("\nEncomenda :" + e. getCodenc());
        System.out.println("Localização da loja:\n" + l.getGPS().toString());
        System.out.println("Localização do Utilizador:\n " + u.getGPS().toString());
    }


    public void op1(Encomenda e){
        System.out.println("\nEcomenda:" + e.getCodenc());
        System.out.println("Model.Loja: " +e.getCodloja());
        System.out.println("Model.Utilizador: "+e.getCoduser());
    }

    public void aceite(){
        System.out.println("Model.Encomenda Aceite para entrega!");
    }
    public void rejeite(){System.out.println("Model.Encomenda rejeitada");}

    public void printMenuPedidos(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Aceitar Pedido                                                                        |");
        System.out.println("|2  -> Rejeitar Pedido                                                                       |");
        System.out.println("|____________________________________________________________________________________________|");
    }


    // ===================== Alterar dados ==============================================

    public void printMenuDados(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Alterar Nome                                                                          |");
        System.out.println("|2  -> Alterar Password                                                                      |");
        System.out.println("|3  -> Alterar Localização                                                                   |");
        System.out.println("|4  -> Alterar raio de ação                                                                  |");
        System.out.println("|5  -> Alterar taxa                                                                          |");
        System.out.println("|6  -> Alterar taxa de peso                                                                  |");
        System.out.println("|7  -> Alterar tranporte de Medicamentos                                                     |");
        System.out.println("|0  -> Voltar ao menu Transportadora                                                         |");
        System.out.println("|____________________________________________________________________________________________|");
    }



    public void altNome(){ System.out.println("Insira o nome novo:");
    }

    public void passordAntiga(){
        System.out.println("Insira a password atual:");
    }

    public void passError(){
        System.out.println("Palavra pass incorreta");
    }

    public void passordNova(){
        System.out.println("Insira a password nova:");
    }

    public void altloc(){
        System.out.println("Insira a nova latitude:");
    }

    public void altloclon(){
        System.out.println("Insira a nova longitude:");
    }

    public void altRaio(){ System.out.println("Insira o novo raio de ação:");}

    public void raioSuc(){System.out.println("Raio de ação alterado com sucesso!");}

    public void altT(){System.out.println("Insira a nova taxa:");}

    public void altTP(){System.out.println("Insira a nova taxa de peso:");}

    public void altMedico(){
        System.out.println("Tem aptidões para transporte de medicamentos?");
        System.out.println("1  -> SIM");
        System.out.println("2  -> NÃO");
    }

    public void printDadosAtuais(Transportadora t){
        System.out.println("\nOs seus dados atuais: ");
        System.out.println("Nome:" + t.getNome());
        System.out.println("Codigo:"  + t.getCod());
        System.out.println("Médico: " +t.aceitoTransporteMedicamentos());
        System.out.println("Raio de ação:" + t.getRaio());
        System.out.println("Taxa:" + t.getTaxa());
        System.out.println("Taxa de Peso:" + t.getTaxaPeso());
        System.out.println("Kms percorridos: " + t.getKms());
        System.out.println("A sua Classificação: " + t.getClGeral());
        System.out.println("Localização:\n    " + t.getGPS().toString() + "\n");
    }

    public void printNonE(){
        System.out.println("A encomenda não existe!");
    }

    public void ano(){
        System.out.println("Insira o ano:");
    }

    public void mes(){
        System.out.println("Insira o mes:");
    }

    public void dia(){
        System.out.println("Insira o dia:");
    }
    public void hora(){
        System.out.println("Insira o hora:");
    }
    public void minuto(){
        System.out.println("Insira o minuto:");
    }

    public void print1stDate(){
        System.out.println("Insira a primeira data:");
    }
    public void print2ndDate(){
        System.out.println("Insira a segunda data:");
    }

    public void printFat(LocalDateTime d1, LocalDateTime d2, double f){
        if(f==0) System.out.println("Nao existem entregas entre essas datas!");
       else  System.out.println("O total faturado entre: " + d1 + " e " + d2 +"foi: " + f);
    }

    public void invalidDate(){
        System.out.println("Alguns dos valores são invalidos!");
    }


}
