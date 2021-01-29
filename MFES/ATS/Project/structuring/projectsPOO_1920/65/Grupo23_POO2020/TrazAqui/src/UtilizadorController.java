import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class UtilizadorController {

    public static void controller(SGE sge, Utilizador user) {
        String op = "";

        do {
            /*Print do menu*/
            View.showUserMenu();

            /*Pedido de opçao? */
            View.showInsercao("uma opçao: ");

            op = Input.lerString();
            op = op.toUpperCase();

            switch (op) {

                /*Utilizador solicita uma encomenda*/
                case "1": {

                    String o = "";

                    /*Lista de prods para a enc*/
                    List<Produto> prods = new ArrayList<>();

                    /*Print das lojas nearby*/
                    View.showLojasUser(sge.getLojas(), user);

                    /*Pedido de loja*/
                    View.showInsercao("uma loja: ");
                    String loja = Input.lerString();

                    /*Validaçao*/
                    while(!sge.getLojas().containsKey(loja)){
                        View.showError("Loja invalida. Tente outra vez.");

                        View.showInsercao("uma loja: ");
                        loja = Input.lerString();
                    }

                    /*Associar a loja a escikga*/
                    Loja l = sge.getLoja(loja);

                    double peso = 0;

                    /*Loop de fazer a encomenda*/
                    while(!o.equals("Q")){
                        View.showEncomendaMaking();

                        View.showInsercao("uma opçao: ");
                        o = Input.lerString();
                        o = o.toUpperCase();

                        /*No caso de querer fazer a encomenda*/
                        if(o.equals("E")){
                            /*Pedido do codprod*/
                            View.showInsercao("um codigo do produto: ");
                            String codProd = Input.lerString();

                            while(!IProduto.isValid(codProd)){
                                View.showError("Codigo invalido. Tente outra vez.");

                                /*Pedido do codprod*/
                                View.showInsercao("um codigo do produto: ");
                                codProd = Input.lerString();
                            }

                            /*Pedido de descriçao de prod*/
                            View.showInsercao("uma descriçao: ");
                            String desc = Input.lerString();

                            View.showInsercao("a quantidade pretendida: ");
                            double quant = Input.lerDouble();

                            peso += quant;

                            double precoUni = RandomDoubles.generateDoubles();

                            prods.add(new Produto(codProd, desc, quant, precoUni));
                        }
                    }

                    /*Pergunta se a encomenda e medica*/
                    View.show("Encomenda e medica? ");
                    boolean em = Input.lerBoolean();

                    String rcodEnc = "e" + RandomDoubles.generateInts();

                    while(sge.getEncomendas().containsKey(rcodEnc) || !IEncomenda.isValid(rcodEnc)){
                        rcodEnc = "e" + RandomDoubles.generateInts();
                    }

                    Encomenda enc = new Encomenda(rcodEnc, user.getCodUtilizador(),
                            l.getCodLoja(), null, peso, false, false, false, em, prods);

                    /*Adiçao da encomenda ao user*/
                    user.addEncomenda(enc);

                    /*Adiçao da encomenda ao sistema*/
                    sge.addEncomenda(enc);

                    /*Adiçao aos registos da loja*/
                    l.addEncsRegistos(enc);

                    /*Adiçao a queue da loja*/
                    l.addEncsQueue(enc);

                    View.show("Encomenda feita. O numero da encomenda é: " + enc.getCodEncomenda());

                    break;
                }

                /*aceitar, ou não, o serviço de entrega proposto por uma empresa transportadora*/
                case "2": {
                    if(user.getOfertasT() == null){
                        View.showError("Nao existem ofertas.");
                        break;
                    }

                    /*Show oferta*/
                    View.showOfertaUtilizador(user.getOfertasT());

                    /*Show opçoes*/
                    View.showInsercao("S ou N: ");

                    /*Pedido*/
                    String escolha = Input.lerString();
                    escolha = escolha.toUpperCase();

                    /*Validaçao*/
                    while(!escolha.equals("S") && !escolha.equals("N")){
                        /*Erro*/
                        View.showError("Opçao Invalida. Tente outra vez.");

                        /*Pedido*/
                        escolha = Input.lerString();
                        escolha = escolha.toUpperCase();
                    }

                    /*Yes*/
                    if(escolha.equals("S")){
                        sge.clienteAceitaTransportadora(sge, user.getOfertasT());
                    }

                    /*No*/
                    else{
                        user.setOfertasTrans(null);
                    }

                    break;
                }

                /*aceder à informação das entregas efectuadas num determinado período e por voluntário ou transportador*/
                case "3": {
                    if(!user.temEncEntregues()){
                        View.showError("O utilizador ainda nao recebeu encomendas.");
                        break;
                    }

                    /*Pedido 1*/
                    View.showInsercao("(no formato AAAA-MM-DDTHH:MM:SS) a primeira data: ");
                    String d1 = Input.lerString();
                    LocalDateTime date1 = LocalDateTime.parse(d1);

                    /*Pedido 2*/
                    View.showInsercao("(no formato AAAA-MM-DDTHH:MM:SS) a segunda data: ");
                    String d2 = Input.lerString();
                    LocalDateTime date2 = LocalDateTime.parse(d2);

                    String escolha = "";

                    do {
                        View.showEncsVT();

                        /*Pedido*/
                        View.showInsercao("uma opçao: ");
                        escolha = Input.lerString();
                        escolha = escolha.toUpperCase();

                        switch(escolha) {

                            /*Voluntarios*/
                            case "1": {
                                /*Lista de encomendas entregues pelos voluntarios*/
                                List<Encomenda> encsV = user.encsEntregueVoluntario(user.encsBetween(date1, date2));

                                if(encsV.size() == 0){
                                    View.showError("Nenhum voluntario entregou uma encomenda a este user.");
                                    break;
                                }

                                /*Print das encomendas*/
                                View.showEncomendas(encsV);

                                /*Pedido*/
                                View.showInsercao("uma encomenda: ");
                                String e = Input.lerString();

                                while (!sge.ListContainsEncomenda(encsV, e)) {
                                    View.showError("Encomenda nao existe. Tente outra vez.");

                                    /*Pedido*/
                                    View.showInsercao("uma encomenda: ");
                                    e = Input.lerString();
                                }

                                Encomenda enc = user.getEncomendas().get(e);

                                View.showInfoEncomenda(enc);

                                break;
                            }

                            /*Transportadora*/
                            case "2": {
                                /*Lista de encomendas entregues pelas transportadoras*/
                                List<Encomenda> encsT = user.encsEntregueTransportadora(user.encsBetween(date1, date2));

                                if(encsT.size() == 0){
                                    View.showError("Nenhuma Transportadora entregou uma encomenda a este user.");
                                    break;
                                }

                                /*Print das encomendas*/
                                View.showEncomendas(encsT);

                                /*Pedido*/
                                View.showInsercao("uma encomenda: ");
                                String e = Input.lerString();

                                while (!sge.ListContainsEncomenda(encsT, e)) {
                                    View.showError("Encomenda nao existe. Tente outra vez.");

                                    /*Pedido*/
                                    View.showInsercao("uma encomenda: ");
                                    e = Input.lerString();
                                }

                                Encomenda enc = user.getEncomendas().get(e);

                                View.showInfoEncomenda(enc);

                                break;
                            }

                            case "Q": {
                                View.show("Exiting...");
                                break;
                            }

                            default: {
                                View.showError("Opçao Invalida.");
                                break;
                            }
                        }

                    } while(!escolha.equals("Q"));

                    break;
                }

                /*Classificar o serviço de entrega*/
                case "4": {

                    /*Verificar se o Utilizador tem encomendas entregues e nao classificadas*/
                    if (!user.temEncEntregues() || user.getEncomendas().size() == 0) {
                        View.showError("Nao ha nenhuma encomenda entregue para classificar.");
                        break;
                    } else {
                        /*Map com as encomendas a apresentar*/
                        Map<String, Encomenda> encomendasSemValidacao = user.getEncsSValid();

                        /*Print das lista de encomendas entregues e sem classificaçao*/
                        View.showEncomendas(encomendasSemValidacao);

                        /*Pedido de uma encomenda*/
                        View.showInsercao("uma encomenda: ");
                        String e = Input.lerString();

                        /*Validaçao da encomenda*/
                        while (!encomendasSemValidacao.containsKey(e)) {
                            View.showError("Essa encomenda nao existe. Tente outra vez");

                            View.showInsercao("uma encomenda: ");
                            e = Input.lerString();
                        }

                        /*Encomenda em especifico*/
                        Encomenda enc = user.getEncomendas().get(e);

                        /*Transportadora*/
                        if (enc.getCodEntregador().charAt(0) == 't') {
                            /*Transportadora*/
                            Transportadora t = sge.getTransportadoras().get(enc.getCodEntregador());

                            /*Pedido de classficaçao*/
                            View.showInsercao("uma classificaçao: ");
                            float classificacao = Input.lerFloat();

                            while(classificacao < 0 || classificacao > 10){
                                View.showError("Valor invalido. Entre 0 e 10.");

                                /*Pedido de classficaçao*/
                                View.showInsercao("uma classificaçao: ");
                                classificacao = Input.lerFloat();
                            }

                            /*Adiçao de classificaçao*/
                            t.getClassificacoes().add(classificacao);

                            /*Mudança da flag*/
                            enc.setClassificada(true);

                            /*Print final*/
                            View.showClassificacaoDada();
                        }

                        /*Voluntario*/
                        else {
                            /*Voluntario*/
                            Voluntario v = sge.getVoluntarios().get(enc.getCodEntregador());

                            /*Pedido de classficaçao*/
                            View.showInsercao("uma classificaçao: ");
                            float classificacao = Input.lerFloat();

                            while(classificacao < 0 || classificacao > 10){
                                View.showError("Valor invalido. Entre 0 e 10.");

                                /*Pedido de classficaçao*/
                                View.showInsercao("uma classificaçao: ");
                                classificacao = Input.lerFloat();
                            }

                            /*Adiçao de classificaçao*/
                            v.getClassificacoes().add(classificacao);

                            /*Mudança da flag*/
                            enc.setClassificada(true);

                            /*Print final*/
                            View.showClassificacaoDada();
                        }
                        break;
                    }
                }

                /*Exit*/
                case "Q": {
                    View.showBB();
                    break;
                }

                /*Opçao Invalida*/
                default: {
                    View.showError("Opcao Invalida.");
                    break;
                }
            }
        } while (!op.equals("Q"));
    }
}

