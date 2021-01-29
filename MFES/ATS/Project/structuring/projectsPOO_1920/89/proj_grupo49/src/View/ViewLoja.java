package View;

import Model.Encomenda;
import Model.Loja;
import Model.Produto;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

public class ViewLoja {

    public void menuLoja(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Sinalizar uma Encomenda pronta para Transporte                                        |");
        System.out.println("|2  -> Adicionar produto                                                                     |");
        System.out.println("|3  -> Ver quantas e quais encomendas estão sinalizadas                                      |");
        System.out.println("|4  -> Ver encomendas não sinalizadas                                                        |");
        System.out.println("|5  -> Ver lista total de encomendas                                                         |");
        System.out.println("|6  -> Consultar stock de produtos                                                           |");
        System.out.println("|7  -> Alterar dados pessoais                                                                |");
        System.out.println("|8  -> Remover Produto                                                                       |");
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

    public void printError(){
        System.out.println("\nNão escreveu nenhuma das duas opções válidas!");
        System.out.print("Por favor tente outra vez: ");
    }

    public void inst(){
        System.out.println("\nEscolha a instrução: ");
    }
    
    public void instExec(){
        System.out.println("\nInstrução executada com sucesso! ");
    }



    public void opc3(List<Encomenda> e){
        if (e.size() <=0){
            System.out.println("A lista está vazia, não tem encomendas sinalizadas!");
        }
        else {
            System.out.println("Codigo");
            for (Encomenda es : e) {
                System.out.println(es.getCodenc());
            }
        }
    }

    public void opc4(List<Encomenda> e, List<Encomenda> prontas){
        if (e.size() <=0){
            System.out.println("A lista esta de encomendas geral esta vazia vazia!");
        }
        else {
            List<Encomenda> aux = new ArrayList<>();
            for (Encomenda em :e) {
                if (!prontas.contains(em) && !em.getAceites()){
                    aux.add(em.clone());
                }
            }
            if (aux.size()<= 0){
                System.out.println("Não há encomendas a sinalizar!");
            }
            else {
                System.out.println("Codigo");
                for (Encomenda es : aux) {
                    System.out.println(es.getCodenc());
                }
            }
        }
    }

    public void opc5(List<Encomenda> e){
        if (e.size() <=0){
            System.out.println("A lista está vazia!");
        }
        else {
          System.out.println("Todas as encomendas:");
            for (Encomenda es : e) {
                System.out.println("\nModel.Encomenda: " + es.getCodenc());
                System.out.println("Aceite " + es.getAceites());
                System.out.println("Entregue " + es.getEntregue());
            }
        }
    }

    public void cunsultaStock(){
        System.out.println("Deseja consultar o stock de produtos :");
    }

    public void pError(){
        System.out.println("O produto inserido não existe!");
    }

    public void remCP(){
        System.out.println("Insira o código do produto a remover:");
        System.out.print("-> ");
    }

//    public void consultaResult(int p){
//        System.out.printf("Existem %d unidades do produto:\n", p);
//    }


    public void op1(){
        System.out.println("Insira a encomenda a sinalizar para entrega:");
    }

    public void op1Error(){
        System.out.println("A encomenda inserida não existe ou já foi sinalizada");
    }



    
    public void addCP(){
        System.out.println("Insira o código do produto a adicionar:");
    }
    
    public void addDesP(){
        System.out.println("Insira a descrição do produto a adicionar:");
    }

    public void addMedico(){
        System.out.println("O pruduto inserido é classificado como médico? (1) se sim (0) caso contrário");
    }
    
    public void addPeso(){
        System.out.println("Insira o peso unitário do produto a adicionar:");
    }

    public void addQP(){
        System.out.println("Insira o preço unitário do produto a adicionar:");
    }

    public void addPError() {
        System.out.println("O código do produto inserido já existe!");
    }



    public void printProd(TreeSet<Produto> produtos){
        if (produtos == null){
            System.out.println("A lista está vazia!");
        }
        else{
            System.out.println("Produtos da loja:");
            for (Produto p : produtos){
                System.out.println("\nCodigo: "+ p.getCod());
                System.out.println("Descrição: "+ p.getNome());
                System.out.println("Médico: " +p.getMedico());
                System.out.println("Peso uni: "+ p.getPeso());
                System.out.println("Preço uni: "+ p.getPreçouni());
            }
        }
    }

    public void printProduto(Produto p){
        System.out.println("\nProduto inserido:");
        System.out.println("Codigo: "+ p.getCod());
        System.out.println("Descrição: "+ p.getNome());
        System.out.println("Médico: " +p.getMedico());
        System.out.println("Peso uni: "+ p.getPeso());
        System.out.println("Preço uni: "+ p.getPreçouni());
    }




    // ===================== Alterar dados ==============================================


    public void printMenuDados(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Alterar Nome                                                                          |");
        System.out.println("|2  -> Alterar Password                                                                      |");
        System.out.println("|3  -> Alterar Localização                                                                   |");
        System.out.println("|0  -> Voltar ao menu Loja                                                                   |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void altNome(){
        System.out.println("Insira o nome novo:");
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

    public void printDadosAtuais(Loja l){
        System.out.println("\nOs seus dados atuais: ");
        System.out.println("Nome:" + l.getNome());
        System.out.println("Codigo:"  + l.getCod());
        System.out.println("Localização:\n    " + l.getGPS().toString() + "\n");
    }


    //?======================================= Loja Fila ===============================================================


        public void menuLojaFila(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Sinalizar uma Encomenda pronta para Transporte                                        |");
        System.out.println("|2  -> Adicionar produto                                                                     |");
        System.out.println("|3  -> Ver quantas e quais encomendas estão sinalizadas                                      |");
        System.out.println("|4  -> Ver encomendas não sinalizadas                                                        |");
        System.out.println("|5  -> Ver lista total de encomendas                                                         |");
        System.out.println("|6  -> Consultar stock de produtos                                                           |");
        System.out.println("|7  -> Alterar dados pessoais                                                                |");
        System.out.println("|8  -> Atualizar Fila de espera                                                              |");
        System.out.println("|0  -> Exit Program                                                                          |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void filaEspera(Loja a){
        System.out.println("Em Fila de Espera: " + a.getFila());
    }

    public void setFilaEspera(){
        System.out.println("A fila atualmente tem quantas encomendas em espera ?");
        System.out.print("->");
    }





}
