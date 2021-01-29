package src.view;

import src.controller.UtilizadorController;
import src.exceptions.*;
import src.model.Encomenda;
import src.model.Entrega;
import src.model.Produto;
import src.model.Utilizador;

import java.time.DateTimeException;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Scanner;

public class UtilizadorView {
    private UtilizadorController controller;
    private Scanner sc;

    public UtilizadorView(UtilizadorController c, Scanner sc){
        this.controller = c;
        this.sc = sc;
    }

    public void run() {
        int x = 1;
        while (x != 0) {
            System.out.println("Escolha uma opção:");
            System.out.println("0 - Sair");
            System.out.println("1 - Verificar Entregas no histórico");
            System.out.println("2 - Adicionar Linha de Encomenda");
            System.out.println("3 - Criar Encomenda");
            System.out.println("4 - Verificar Pedidos de Encomenda");
            System.out.println("5 - Requisitar Entrega");
            System.out.println("6 - Classificar Entrega");
            System.out.println("7 - Verificar lista de produtos disponíveis");
            System.out.println("8 - Verificar Perfil");
            System.out.println("9 - Verificar a lista das 10 Transportadoras que mais usam o sistema (em kms)");
            System.out.println("10 - Verificar lista de lojas disponíveis");
            try{
                x = sc.nextInt();
                sc.nextLine();

                switch (x) {

                    case (1):
                        this.getHistoricoEntregas();
                        break;

                    case (2):
                        this.addLinhaEncomendaSc();
                        break;

                    case (3):
                        this.criarEncomenda();
                        break;

                    case (4):
                        this.verificarPedidosEncomenda();
                        break;

                    case (5):
                        this.requisitaEntrega();
                        break;

                    case (6):
                        this.classificaEntrega();
                        break;

                    case (7):
                        this.showProdutos(this.controller.getProdutos());
                        break;

                    case(8):
                        this.showPerfil(this.controller.getPerfil());
                        break;

                    case(9):
                        this.top10Transportadoras();
                        break;

                    case(10):
                        this.printLojas();
                        break;

                    case (0):
                        System.out.println("Até breve!");
                        break;
                    default:
                        System.out.println("Insira uma opção válida");
                }
            }

            catch (RuntimeException e){
                System.out.println("Não foi possível Realizar o solicitado!");
                System.out.println("Input inválido!");
                sc.nextLine();
            }
        }
    }

    public void showEntregas(Collection<Entrega> printables){
        int r = 0;
        for (Entrega e : printables){
            System.out.println(r + "-" + e);
            r++;
        }
        System.out.println("");
    }




    public void addLinhaEncomendaSc(){
        try{
            String codProd = "";
            double quant;
            System.out.println("Insira o código do produto a encomendar:");
            codProd = sc.nextLine();
            System.out.println("Insira a quantidade a encomendar:");
            quant = sc.nextDouble();
            this.controller.addLinhaEncomenda(codProd,quant);
            System.out.println("Linha adicionada com sucesso!");
        }

        catch(ProdutoInexistenteException e){
            System.out.println("Não foi possível Realizar o solicitado!");
            System.out.println(e.getMessage());
        }

        catch (ValorInvalidoException exc){
            System.out.println("Valor inválido!");
        }

        catch (RuntimeException e){
            System.out.println("Não foi possível Realizar o solicitado!");
            System.out.println("Input inválido!");
            sc.nextLine();
        }
    }




    public void criarEncomenda(){
        try{
            String codLoja;
            System.out.println("Insira o código da loja:");
            codLoja = sc.nextLine();
            this.controller.criaEncomenda(codLoja);
            System.out.println("Encomenda criada com sucesso!");
        }

        catch (LojaInexistenteException e) {
            System.out.println("Não foi possível Realizar o solicitado!");
            System.out.println(e.getMessage());     
        }

        catch (SemLinhasDeEncomendaException e){
            System.out.println("Não foi possível Realizar o solicitado!");
            System.out.println("Parece que você não possui linhas de encomenda registadas");

        }
    }





    public void verificarPedidosEncomenda(){
        int r = 1;
        for(Encomenda e : this.controller.getPedidosEncomenda()){
            System.out.println(r + "-" + e);
            r++;
        }
    }



    public void requisitaEntrega(){
        try{
            String codEnc;
            System.out.println("Insira a encomenda que pretende receber:");
            codEnc = sc.nextLine();
            Optional<Map<String,Double>> res = this.controller.requisitarEntrega(codEnc);

            if(res.isEmpty()){
                System.out.println("Sucesso! A entrega da sua encomenda foi atribuída a um voluntário");
            }

            else{
                System.out.println("Recebeu as seguintes propostas de entrega:");

                for(Map.Entry<String,Double> e : res.get().entrySet()){
                    System.out.println("Empresa: " + e.getKey() + " Preço: " + e.getValue());
                }

                this.aceitaPropostaTransportadora(res.get(), codEnc);

            }
        }

        catch(EncomendaInvalidaException e){
            System.out.println("Processo inválido!");
            System.out.println(e.getMessage());
        }

        catch(SemDistribuidoresException e){
            System.out.println("Parece não haverem Distribuidores disponíveis na sua área, tente novamente mais tarde!");
        }
    }





    public void classificaEntrega(){
        try{
            String codEnc;
            double classificacao;

            System.out.println("Insira a Entrega que pretende classificar:");
            codEnc = sc.nextLine();

            System.out.println("Insira a classificação que pretende atribuir:");
            classificacao = sc.nextDouble();

            this.controller.classificarEntrega(codEnc,classificacao);

            System.out.println("Sucesso! A Entrega " + codEnc + " foi classificada");
        }

        catch(EncomendaInvalidaException e){
            System.out.println("Não foi possível realizar o solicitado;");
            System.out.println(e.getMessage());
        }

        catch (ValorInvalidoException e){
            System.out.println("Não foi possível realizar o solicitado;");
            System.out.println(e.getMessage());
            sc.nextLine();
        }

        catch(RuntimeException e){
            System.out.println("Não foi possível Realizar o solicitado!");
            System.out.println("Input inválido!");
            sc.nextLine();
        }
    }





    public void aceitaPropostaTransportadora(Map<String,Double> r, String codEnc){
        String codDistribuidor;

        System.out.println("Insira o código da Transportadora cuja oferta quer aceitar:");
        codDistribuidor = sc.nextLine();

        if (r.containsKey(codDistribuidor)) {
            this.controller.aceitaPropostaTransportadora(codDistribuidor,codEnc);
        }

        else{
            System.out.println("código inválido, operação cancelada!");
        }  
    }



    public void showProdutos(Collection<Produto> printables){
        int r = 0;
        for (Produto p : printables){
            System.out.println(r + "-" + p);
            r++;
        }
        System.out.println("");
    }

    public void showPerfil(Utilizador atual){
        System.out.println("Código de user:");
        System.out.println(atual.getUsername());
        System.out.println("Nome:");
        System.out.println(atual.getNome());
        System.out.println("Email:");
        System.out.println(atual.getEmail());
        System.out.println("Localização:");
        System.out.println(atual.getLocalizacao());
    }

    public void getHistoricoEntregas(){
        int ano1,mes1,dia1,ano2,mes2,dia2;
        System.out.println("Insira a data a partir da qual deseja ver o valor faturado:");
        System.out.println("Insira o ano:");
        ano1 = sc.nextInt();
        System.out.println("Insira o mês:");
        mes1 = sc.nextInt();
        System.out.println("Insira o dia:");
        dia1 = sc.nextInt();
        System.out.println("Insira a data até qual deseja ver o valor faturado:");
        System.out.println("Insira o ano:");
        ano2 = sc.nextInt();
        System.out.println("Insira o mês:");
        mes2 = sc.nextInt();
        System.out.println("Insira o dia:");
        dia2 = sc.nextInt();

        try {
            this.showEntregas(this.controller.getHistoricoEntregas(ano1, mes1, dia1, ano2, mes2, dia2));
        }

        catch (DateTimeException d){
            System.out.println("Valores Inválidos! Tente novamente!");
        }
    }

    public void top10Transportadoras(){
        System.out.println(this.controller.getTop10TransKms().toString());
    }

    public void printLojas(){
        System.out.println(this.controller.getLojasSis().toString());
    }
}
