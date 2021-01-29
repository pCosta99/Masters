package View;

import Model.*;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ViewUser {


    public void menuUser(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Fazer Encomenda                                                                       |");
        System.out.println("|2  -> Aceitar Entrega                                                                       |");
        System.out.println("|3  -> Ver historico de encomendas                                                           |");
        System.out.println("|4  -> Classificar Serviço de entrega                                                        |");
        System.out.println("|5  -> Alterar dados pessoais                                                                |");
        System.out.println("|0  -> Exit Program                                                                          |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void printBarraN(){
        System.out.println();
    }
    public void flush(){
        System.out.println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
    }
    public void inst(){
        System.out.println("\nEscolha a instrução: ");
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

    public void codEnc(){
        System.out.println("Introduza o código da sua encomenda:");
    }

    public void codloja(){
        System.out.println("Introduza o código da loja que pretende fazer a encomenda:");
    }

    public void quant(){
        System.out.println("Quantos produtos diferentes pretende pedir?");
    }

    public void codProd(){
        System.out.println("Introduza o código de produto:");
    }

    public void quantp(){
        System.out.println("Qual a quantidade do produto?");
    }

    public void succes(){
        System.out.println("Model.Encomenda bem sucedida!\n A Model.Loja irá sinalizar a sua encomenda e ser-lhe-á atribuído um transporte.\n Aguarde por mais informação.");
    }

    public void lojaInv(){ System.out.println("Esta loja não existe, por favor insira uma loja válida!");}

    public void prodInv(){ System.out.println("Este produto nao existe, por favor insira um produto válido!");}

    public void printInv(){
        System.out.println("\nValor Inválido!");
        System.out.print("Por favor tente outra vez: \n");
    }

    public void remProd(){System.out.println("Insira o codigo de produto que pretende remover:");}

    public void encVazia(){
        System.out.println("Ainda não acrescentou nenhum produto!");
    }

    public void printLojas(Lojas l){
        System.out.println("Código           Nome da Model.Loja\n");
        for(Map.Entry<String, Loja> loja : l.getLojas().entrySet()){
            System.out.printf("%4s           %s\n", loja.getKey(), loja.getValue().getNome());
        }
    }

    public void printProdutos(Set<Produto> p){
        System.out.println("Código           Preço           Descrição\n");
        for(Produto pr : p){
            System.out.printf("%4s             %f                %s\n", pr.getCod(), pr.getPreçouni(), pr.getNome());
        }
    }

    public void menuEncomenda(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Acrescentar um Produto                                                                |");
        System.out.println("|2  -> Remover um Produto                                                                    |");
        System.out.println("|3  -> Ver estado de Encomenda                                                               |");
        System.out.println("|4  -> Finalizar Encomenda                                                                   |");
        System.out.println("|0  -> Exit Program                                                                          |");
        System.out.println("|____________________________________________________________________________________________|");
    }


    public void getEstadoEnc(List<LinhaEncomenda> l){
        if (l.size()<=0) System.out.println("Lista Vazia!");
        else{
            for (LinhaEncomenda linha: l){
                System.out.println();
                System.out.println("Model.Produto: " + linha.getCod());
                System.out.println("Descrição: " + linha.getDesc());
                System.out.println("Quantidade: " + linha.getQuant());
                System.out.println("Preço s/transp: " + linha.getPreco());
            }
        }
    }

    public void printEnc(Map<String,List<String>> p){
        System.out.println("Encomendas em espera:");
        for(Map.Entry<String,List<String>> ped : p.entrySet()){
            System.out.println(ped.getKey());
        }
    }

    public void pedTransp(String e ){
        System.out.println("Pedidos de Transportadoras para a encomenda "+e+":\n");
    }

    public void printTransp(Transportadora t, double pr, double[] time){
        System.out.println("\nCodigo: "+t.getCod());
        System.out.println("Nome: "+t.getNome());
        System.out.println("Preço: "+ pr);
        System.out.println("Tempo estimado: " +time[0] +"D " + time[1]+ "H " + time[2] +" min");
    }

    public void codEncC(){
        System.out.println("Introduza o código da encomenda a consultar:");
    }


    public void acceptTransp(){
        System.out.println("Insira o codigo da transportadora a aceitar");
    }

    public void jaAceite(String ec){
        System.out.println("A encomenda "+ec+" já tem trasnportadora!");
    }

    public void printNonT(){
        System.out.println("A transportadora que inseriu não existe. Por favor tente outra vez:");
    }

    public void printNonE(){
        System.out.println("A encomenda que inseriu não existe. Por favor tente outra vez:");
    }

    public void classifica(List<Encomenda> l){
            for(Encomenda e : l){
                    System.out.println("Encomenda:" + e.getCodenc());
            }
            System.out.println("Escolha qual entrega pretende classificar!");
        }

    public void classificacao(){
        System.out.println("Classifique a entrega de 0 a 5");
    }

    public void obg(){
        System.out.println("Obrigado pela sua classificacao!");
    }

    public void histEnc(List<Encomenda> l){
        if (l.size()<=0) System.out.println("Lista Vazia! ");
        else{
            for(Encomenda e : l) {
                System.out.println();
                System.out.println("Ecomenda:" + e.getCodenc());
                System.out.println("Model.Loja: " + e.getCodloja());
                System.out.println("Preço: " + e.getPreco());
                System.out.println("Médica : " +e.getMedica());
                System.out.println("Transporte: " + e.getTransp());
                System.out.println("Aceite: " + e.getAceites());
                System.out.println("Inicio da Entrega:" + e.getDatai());
                System.out.println("Entregue: " + e.getEntregue());
                if (e.getEntregue()) {
                    System.out.println("Fim da Entrega:" + e.getDataf());
                }
                else System.out.println("A encomenda ainda não foi entregue!");
            }
        }
    }

    public void printMenuDados(){
        System.out.println("_____________________________________________________________________________________________");
        System.out.println("|Escolha uma opção:                                                                          |");
        System.out.println("|1  -> Alterar Nome                                                                          |");
        System.out.println("|2  -> Alterar Password                                                                      |");
        System.out.println("|3  -> Alterar Localização                                                                   |");
        System.out.println("|0  -> Voltar ao menu                                                                        |");
        System.out.println("|____________________________________________________________________________________________|");
    }

    public void printDadosAtuais(Utilizador l){
        System.out.println("\nOs seus dados atuais: ");
        System.out.println("Nome:" + l.getNome());
        System.out.println("Codigo:"  + l.getCod());
        System.out.println("Localização:\n    " + l.getGPS().toString() + "\n");
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

    public void vazia(){
        System.out.println("A lista está vazia");
    }

    public void existe(){
        System.out.println("A encomenda que tentou criar já existe!");
    }


}
