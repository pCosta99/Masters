import java.io.*;
import java.time.LocalDateTime;
import java.util.*;

public class SGEController {
    SGE sge = new SGE();

    public void start(){
        String opcao = "";

        SGE sge = new SGE();

        Parse.parse(sge);

        do {
            /*Print do menu*/
            View.printMenu();

            /*Pedido de uma opçao*/
            View.showInsercao("uma opçao: ");

            /*Pede o input ao user*/
            opcao = Input.lerString();
            opcao = opcao.toUpperCase();

            switch(opcao) {

                /*registar um utilizador, um voluntário, uma empresa transportadora e uma loja;*/
                case "1": {
                    View.showOpcoesRegisto();

                    View.showInsercao("uma opçao: ");

                    String op = Input.lerString();

                    switch(op) {
                        case "1": {
                            /*Pedido do username*/
                            View.showInsercao("um codigo de utilizador: ");
                            String codUtilizador = Input.lerString();

                            /*Validaçao do Codigo de utilizador*/
                            while (!IUtilizador.isValid(codUtilizador) || sge.getUtilizadores().containsKey(codUtilizador)) {
                                View.showError("O codigo de utilizador nao e valido! Tente outra vez.");

                                View.showInsercao("um codigo de utilizador: ");
                                codUtilizador = Input.lerString();
                            }

                            /*Pedido do nome de utilizador*/
                            View.showInsercao("o nome do Utilizador: ");
                            String nomeUser = Input.lerString();

                            /*Pedido da coordenada x da posiçao do utilizador*/
                            View.showInsercao("a coordenada x: ");
                            double gpsx = Input.lerDouble();

                            /*Pedido da coordenada y da posicao do utilizador*/
                            View.showInsercao("a coordenada y: ");
                            double gpsy = Input.lerDouble();

                            /*1 parametro e null pk o user ainda n fez nenhuma encomenda*/
                            sge.addUtilizador(new Utilizador(new TreeMap<>(), gpsx, gpsy, nomeUser, codUtilizador));

                            break;
                        }

                        case "2": {
                            /*Pedido do id do Voluntario*/
                            View.showInsercao("um codigo de voluntario: ");
                            String codVoluntario = Input.lerString();

                            /*Validaçao do Codigo de Voluntario*/
                            while (!IVoluntario.isValid(codVoluntario) || sge.getVoluntarios().containsKey(codVoluntario)) {
                                View.showError("O codigo do voluntario nao e valido! Tente outra vez.");

                                View.showInsercao("um codigo de voluntario: ");
                                codVoluntario = Input.lerString();
                            }

                            /*Pedido do nome do Voluntario*/
                            View.showInsercao("o nome do voluntario: ");
                            String nomeVoluntario = Input.lerString();

                            /*Pedido da coordenada x da posiçao do Voluntario*/
                            View.showInsercao("a coordenada x: ");
                            double gpsx = Input.lerDouble();

                            /*Pedido da coordenada y da posicao do Voluntario*/
                            View.showInsercao("a coordenada y: ");
                            double gpsy = Input.lerDouble();

                            /*Pedido de um raio*/
                            View.showInsercao("um raio: ");
                            int raio = Input.lerInt();

                            /*Pedido de vm*/
                            View.showInsercao("a velocidade media: ");
                            double vm = Input.lerDouble();

                            /*Pedido é transporte medico?*/
                            View.show("E transporte medico? ");
                            boolean tm = Input.lerBoolean();

                            sge.addVoluntario(new Voluntario(gpsx, gpsy, raio, vm, new TreeMap<>(), codVoluntario, nomeVoluntario, false, false, tm, null, new ArrayList<>()));

                            break;
                        }

                        case "3": {
                            /*Pedido do id da Transportada*/
                            View.showInsercao("um codigo de transportadora: ");
                            String codTransportadora = Input.lerString();

                            /*Validaçao do Codigo de transportadora*/
                            while (!ITransportadora.isValid(codTransportadora) || sge.getTransportadoras().containsKey(codTransportadora)) {
                                View.showError("O codigo da transportadora nao e valido! Tente outra vez.");

                                View.showInsercao("um codigo de transportadora: ");
                                codTransportadora = Input.lerString();
                            }

                            /*Pedido do nome de transportadora*/
                            View.showInsercao("o nome da transportadora: ");
                            String nomeTransportadora = Input.lerString();

                            /*Pedido da coordenada x da posiçao do transportadora*/
                            View.showInsercao("a coordenada x: ");
                            double gpsx = Input.lerDouble();

                            /*Pedido da coordenada y da posicao do transportadora*/
                            View.showInsercao("a coordenada y: ");
                            double gpsy = Input.lerDouble();

                            /*Pedido do NIF*/
                            View.showInsercao("o NIF: ");
                            String nif = Input.lerString();

                            /*Pedido de um raio*/
                            View.showInsercao("um raio: ");
                            int raio = Input.lerInt();

                            /*Pedido do ppkm*/
                            View.showInsercao("o preço por kilometro: ");
                            double ppkm = Input.lerDouble();

                            /*Pedido de vm*/
                            View.showInsercao("a velocidade media: ");
                            double vm = Input.lerDouble();

                            /*Pedido de capacidade de peso*/
                            View.showInsercao("a capacidade total: ");
                            double capPeso = Input.lerDouble();

                            /*Pedido é transporte medico?*/
                            View.show("E transporte medico? ");
                            boolean tm = Input.lerBoolean();


                            sge.addTransportadora(new Transportadora(gpsx, gpsy, codTransportadora, nomeTransportadora, nif, raio, ppkm, 0, vm, capPeso, false, false, tm, new TreeMap<>(), new ArrayList<>(), new ArrayList<>()));

                            break;
                        }

                        case "4": {
                            /*Pedido do id da loja*/
                            View.showInsercao("um codigo de transportadora: ");
                            String codLoja = Input.lerString();

                            /*Validaçao do Codigo da loja*/
                            while (!ILoja.isValid(codLoja) || sge.getLojas().containsKey(codLoja)) {
                                View.showError("O codigo da loja nao e valido! Tente outra vez.");

                                View.showInsercao("um codigo da loja: ");
                                codLoja = Input.lerString();
                            }

                            /*Pedido do nome de loja*/
                            View.showInsercao("o nome da loja: ");
                            String nomeLoja = Input.lerString();

                            /*Pedido da coordenada x da posiçao da loja*/
                            View.showInsercao("a coordenada x: ");
                            double gpsx = Input.lerDouble();

                            /*Pedido da coordenada y da posicao da loja*/
                            View.showInsercao("a coordenada y: ");
                            double gpsy = Input.lerDouble();

                            sge.addLoja(new Loja (codLoja, nomeLoja, gpsx, gpsy, new TreeMap<>(), new ArrayList<>(), new TreeMap<>()));
                        }

                        default: {
                            View.showError("Opcao Invalida");
                            break;
                        }

                    }

                    break;
                }

                /*validar o acesso à aplicação utilizando as credenciais (email e password);*/
                case "2": {
                    /*Print de pedido*/
                    View.showInsercao("o username: ");

                    /*Pedido de username*/
                    String username = Input.lerString();

                    /*Print de pedido*/
                    View.showInsercao("a password: ");

                    /*Pedido de password*/
                    String password = Input.lerString();

                    /*Validaçao*/
                    while (!IUtilizador.isValid(username) || !sge.getUtilizadores().get(username).getPwd().equals(password)) {
                        View.showError("O username ou a password estao incorretos. Tente outra vez.");

                        View.showInsercao("o username: ");

                        username = Input.lerString();

                        View.showInsercao("a password: ");

                        password = Input.lerString();
                    }

                    /*User em concreto*/
                    Utilizador user = sge.getUtilizadores().get(username);

                    /*Controller*/
                    UtilizadorController.controller(sge, user);

                    break;
                }

                /*Voluntarios*/
                case "3": {

                    /*Print de pedido*/
                    View.showInsercao("o codigo de voluntario: ");

                    /*Pedido de input*/
                    String voluntario = Input.lerString();

                    /*Validaçao*/
                    while (!sge.getVoluntarios().containsKey(voluntario)) {
                        View.showError("O codigo e invalido! Tente outra vez.");

                        View.showInsercao("o codigo de voluntario: ");
                        voluntario = Input.lerString();
                    }

                    /*Voluntario em concreto*/
                    Voluntario v = sge.getVoluntarios().get(voluntario);

                    /*Controller do voluntario*/
                    VoluntarioController.controller(sge, v);

                    break;
                }

                /*Transportadoras*/
                case "4": {

                    /*Pedido de transportadora*/
                    View.showInsercao("uma transportadora: ");
                    String t = Input.lerString();

                    while (!sge.getTransportadoras().containsKey(t)) {
                        View.showError("A transportadora nao existe. Tente outra vez.");

                        /*Pedido de loja*/
                        View.showInsercao("uma transportadora: ");
                        t = Input.lerString();
                    }

                    /*Transportadora*/
                    Transportadora transportadora = sge.getTransportadora(t);

                    /*Controller*/
                    TransportadoraController.controller(sge, transportadora);

                    break;
                }

                /*Lojas*/
                case "5": {

                    /*Pedido de loja*/
                    View.showInsercao("uma loja: ");
                    String l = Input.lerString();

                    while (!sge.getLojas().containsKey(l)) {
                        View.showError("A loja nao existe. Tente outra vez.");

                        /*Pedido de loja*/
                        View.showInsercao("uma loja: ");
                        l = Input.lerString();
                    }

                    /*Loja*/
                    Loja loja = sge.getLoja(l);

                    /*Controller*/
                    LojaController.controller(sge, loja);

                    break;

                }

                /*Query que mostra os top 10 utilizadores*/
                case "6": {
                    View.showTopUtilizadores(sge.topUtilizadores());
                    break;
                }

                /*Query que mostra as top 10 transportadoras*/
                case "7": {
                    View.showTopTransportadoras(sge.topTransportadoras());
                    break;
                }

                /*Indicar o total faturado por uma transportadora num determinado periodo de tempo*/
                case "8": {
                    /*Pedido do codigo de empresa*/
                    View.showInsercao(" uma transportadora: ");
                    String codTrans = Input.lerString();

                    /*Validaçao*/
                    while(!sge.getTransportadoras().containsKey(codTrans)){
                        View.showError("Transportadora nao existe. Tente outra vez");

                        /*Pedido do codigo de empresa*/
                        View.showInsercao("");
                        codTrans = Input.lerString();
                    }

                    View.showInsercao("(no formato AAAA-MM-DDTHH:MM:SS) a primeira data: ");

                    String d1 = Input.lerString();

                    LocalDateTime date1 = LocalDateTime.parse(d1);

                    View.showInsercao("(no formato AAAA-MM-DDTHH:MM:SS) a segunda data: ");

                    String d2 = Input.lerString();

                    LocalDateTime date2 = LocalDateTime.parse(d2);

                    View.showPrecoTransporte(sge.totalFatTransportadora(codTrans, date1, date2));

                    break;
                }


                /*gravar o estado da aplicação em ficheiro*/
                case "S": {
                    String filename = "gestEncomendas.dat";
                    try {
                        ObjectOutputStream os = new ObjectOutputStream(new FileOutputStream(filename));
                        os.writeObject(sge);
                        os.close();
                    }
                    catch(IOException e){
                        e.printStackTrace();
                        View.showError(e.getMessage());
                    }
                    break;
                }

                /*Dar load ao estado da aplicaçao*/
                case "L": {
                    String filename = "gestEncomendas.dat";
                    try {
                        ObjectInputStream is = new ObjectInputStream(new FileInputStream(filename));
                        sge = (SGE) is.readObject();
                        is.close();
                    }
                    catch(IOException | ClassNotFoundException e){
                        e.printStackTrace();
                        View.showError(e.getMessage());
                    }
                    break;
                }

                /*Quit*/
                case "Q": {
                    View.showBB();
                    break;
                }

                /*Opçao invalida*/
                default: {
                    View.showError("Opcao Invalida");
                    break;
                }
            }
        } while(!opcao.equals("Q"));
    }
}
