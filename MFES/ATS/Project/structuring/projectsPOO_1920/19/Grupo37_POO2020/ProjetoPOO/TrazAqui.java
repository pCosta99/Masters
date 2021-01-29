import java.io.*;
import java.util.Scanner;
import java.util.Map;
import java.util.InputMismatchException;
import java.util.List;
import java.util.ArrayList;
import java.time.LocalDateTime;
import java.lang.Object;

public class TrazAqui{
    private GerirTrazAqui batatas;
    private Menu menuInicial, menuLogin, menuCliente, menuLoja, menuEmpresa, menuVoluntario;

    public static void main(String[] args) {
        new TrazAqui().run();
    }

    private TrazAqui() {
        String[] opcoes1 = {"Sign Up", "Login", "Ver Top 10 Clientes", "Ver Top 10 Empresas"};
        String[] opcoes2 = {"Utilizador", "Voluntário", "Loja", "Empresa"};
        String[] opcoes3 = {"Ver lojas", "Fazer encomenda", "Ver lista de encomendas"};
        String[] opcoes4 = {"Transportar encomendas", "Mudar medicamentos", "Informar que encomenda está entregue", "Ver informações de encomenda por entregar"};
        String[] opcoes5 = {"Ver catálogo", "Adicionar ao catálogo", "Ver encomendas", "Disponibilizar encomendas"};
        String[] opcoes6 = {"Transportar encomendas", "Mudar medicamentos", "Informar que encomenda está entregue", "Ver informações de encomendas por entregar"};
        this.menuInicial = new Menu(opcoes1);
        this.menuLogin = new Menu(opcoes2);
        this.menuCliente = new Menu(opcoes3);
        this.menuVoluntario = new Menu(opcoes4);
        this.menuLoja = new Menu(opcoes5);
        this.menuEmpresa = new Menu(opcoes6);
        Parse p = new Parse();
        this.batatas = p.parse();
        try {
            this.batatas = GerirTrazAqui.lerObj("estado.obj");
        }
        catch (FileNotFoundException e) {
            System.out.println("Parece que é a primeira utilização");
            this.batatas = new GerirTrazAqui();
        }
        catch (IOException e) {
            System.out.println("Ops! Erro de leitura!");
            this.batatas = new GerirTrazAqui();
        }
        catch (ClassNotFoundException e) {
            System.out.println("Ops! Formato de ficheiro de dados errado!");
            this.batatas = new GerirTrazAqui();
        }
    }

    private void run() {
        //System.out.println(this.logNegocio.toString());
        do {
            menuInicial.executa();
            String email;
            String password;
            switch (menuInicial.getOpcao()) {
                case 1:
                    String nome;
                    GPS gps;
                    int r;
                    boolean meds;
                    double raio;
                    double espera;
                    int aceite;
                    float precokms;
                    double precoTemp;
                    double estimativa;
                    int vel;
                    System.out.println("Escolheu dar sign up");
                    menuLogin.executa();
                    switch (menuLogin.getOpcao()){
                        case 1:
                            System.out.println("Está a dar sign up como Utilizador");
                            do{
                                System.out.println("Insira o seu email:");
                                email = lerNome();
                                if (this.batatas.existeUtilizador("Utilizador", email)){
                                    System.out.println("Esse email já está a ser utilizado");
                                    email = null;
                                }
                            } while(email == null && !email.equals("0"));
                            if (!email.equals("0")){
	                            do{
	                                System.out.println("Insira a sua password (mínimo, de 6 caracteres e máximo de 16):");
	                                password = lerPassword();
	                            } while(password == null);
	                            System.out.println("Insira o seu nome:");
	                            nome = lerNome();
	                            do{
	                                System.out.println("Insira as suas coordenadas (latitude e longitude):");
	                                gps = lerGPS();
	                            } while(gps == null);
	                            this.batatas.addCliente(email, password, nome, gps);
	                        }
                            break;
                        case 2:
                            System.out.println("Está a dar sign up como Voluntário");
                            do{
                                System.out.println("Insira o seu email:");
                                email = lerNome();
                                if (this.batatas.existeUtilizador("Voluntário", email)){
                                    System.out.println("Esse email já está a ser utilizado");
                                    email = null;
                                }
                            } while(email == null && !email.equals("0"));
                            if (!email.equals("0")){
	                            do{
	                                System.out.println("Insira a sua password (mínimo, de 6 caracteres e máximo de 16):");
	                                password = lerPassword();
	                            } while(password == null);
	                            System.out.println("Insira o seu nome:");
	                            nome = lerNome();
	                            do{
	                                System.out.println("Insira as suas coordenadas (latitude e longitude):");
	                                gps = lerGPS();
	                            } while(gps == null);
	                            do{
	                                System.out.println("Insira o raio (em kms) no qual vai querer operar:");
	                                raio = lerEspera();
	                            } while(raio == -1);
	                            do{
	                                System.out.println("Caso esteja certificado para transportar encomendas médicas e pretender transportá-las responda 1, caso contrário responda 2");
	                                System.out.println("(Este campo pode ser alterado mais tarde)");
	                                r = lerBoolean();
	                            } while(r == -1);
	                            if (r == 1) meds = true;
	                            else meds = false;
	                            do{
	                                System.out.println("A que velocidade média se deve deslocar?");
	                                vel = lerInt();
	                            } while(vel == -1);
	                            this.batatas.addVoluntario(email, password, nome, gps, raio, meds, vel);
	                        }
                            break;
                        case 3:
                            System.out.println("Está a dar sign up como Loja");
                            do{
                                System.out.println("Insira o email:");
                                email = lerNome();
                                if (this.batatas.existeUtilizador("Loja", email)){
                                    System.out.println("Esse email já está a ser utilizado");
                                    email = null;
                                }
                            } while(email == null && !email.equals("0"));
                            if (!email.equals("0")){
	                            do{
	                                System.out.println("Insira a password (mínimo, de 6 caracteres e máximo de 16):");
	                                password = lerPassword();
	                            } while(password == null);
	                            System.out.println("Insira o nome da loja:");
	                            nome = lerNome();
	                            do{
	                                System.out.println("Insira as coordenadas (latitude e longitude):");
	                                gps = lerGPS();
	                            } while(gps == null);
	                            do{
	                                System.out.println("Insira o tempo de espera médio de atendimento na loja, caso não deseje partilhar essa informação insira \"0\":");
	                                espera = lerEspera();
	                            }while(espera == -1);
	                            this.batatas.addLoja(email, password, nome, gps, espera);
	                        }
                            break;
                        case 4:
                            System.out.println("Está a dar sign up como Empresa");
                            do{
                                System.out.println("Insira o email:");
                                email = lerNome();
                                if (this.batatas.existeUtilizador("Empresa", email)){
                                    System.out.println("Esse email já está a ser utilizado");
                                    email = null;
                                }
                            } while(email == null && !email.equals("0"));
                            if (!email.equals("0")){
	                            do{
	                                System.out.println("Insira a password (mínimo, de 6 caracteres e máximo de 16):");
	                                password = lerPassword();
	                            } while(password == null);
	                            System.out.println("Insira o nome da empresa:");
	                            nome = lerNome();
	                            do{
	                                System.out.println("Insira as coordenadas (latitude e longitude):");
	                                gps = lerGPS();
	                            } while(gps == null);
	                            do{
	                                System.out.println("Insira o raio (em kms) no qual vai querer operar:");
	                                raio = lerEspera();
	                            } while(raio == -1);
	                            do{
	                                System.out.println("Caso esteja certificado para transportar encomendas médicas e pretender transportá-las responda 1, caso contrário responda 2:");
	                                System.out.println("(Este campo pode ser alterado mais tarde)");
	                                r = lerBoolean();
	                            } while(r == -1);
	                            if (r == 1) meds = true;
	                            else meds = false;
	                            do{
	                                System.out.println("Por favor diga quantas encomendas poderá transportar simultaneamente:");
	                                aceite = lerInt();
	                            } while(aceite == -1);
	                            do{
	                                System.out.println("Qual será o preço por km que vai fazer?");
	                                precokms = lerRaio();
	                            } while(precokms == -1);
	                            do{
	                                System.out.println("Qual será o preço por tempo?");
	                                precoTemp = lerEspera();
	                            } while(precoTemp == -1);
	                            do{
	                                System.out.println("Qual a velocidade média a que se vai deslocar?");
	                                vel = lerInt();
	                            } while(vel == -1);
	                            this.batatas.addEmpresa(email, password, nome, gps, raio, meds, aceite, precokms, precoTemp, vel);
	                        }
                            break;
                    }
                    break;
                case 2:
                    menuLogin.executa();
                    boolean login = false;
                    Lojas l;
                    int esc;
                    String nomeL;
                    Produto p;
                    int classificacao;
                    switch (menuLogin.getOpcao()){
                        case 1:
                            Cliente c;
                            do{
                                System.out.println("Está a dar login como Utilizador");
                                do{
                                    System.out.println("Insira o seu email:");
                                    email = lerNome();
                                } while(email == null);
                                if (!email.equals("0")){
                                    do{
                                        System.out.println("Insira a sua password (mínimo, de 6 caracteres e maximo de 16):");
                                        password = lerPassword();
                                    } while(password == null);
                                    login = login(email, password, "Utilizador");
                                }
                            } while(login == false && !email.equals("0"));
                            if (!email.equals("0")){
                                System.out.println("Efetuou login como Utilizador com sucesso");
                                c = this.batatas.getCliente().get(email);
                                while (c.getTransporte().size() > 0){
                                    System.out.println("Aceita que a sua encomenda " + c.getTransporte().get(0) + " seja entregue por: " + c.getTransporte().get(0).getPreco() + "€");
                                    do{
                                        System.out.println("1 - Sim");
                                        System.out.println("2 - Não");
                                        esc = lerBoolean();
                                    } while (esc == -1);
                                    if (esc == 1){
                                        this.batatas.getEmp(c.getTransporte().get(0).getCodBusca()).addEncomenda(c.getTransporte().get(0));
                                        c.getTransporte().remove(0);
                                    }
                                    else{
                                        this.batatas.getL(c.getTransporte().get(0).getCodLoja()).getEnc(c.getTransporte().get(0).getCodEnc());
                                        this.batatas.getEmp(c.getTransporte().get(0).getCodBusca()).setAceite(this.batatas.getEmp(c.getTransporte().get(0).getCodBusca()).getAceite() + 1);
                                        c.getTransporte().remove(0);
                                    }
                                }
                                while (c.getClassificaVoluntarios().size() > 0){
                                    System.out.println("Parece que a sua encomenda já foi entregue");
                                    System.out.println("Por favor classifique a entrega de 0 a 10");
                                    do{
                                        classificacao = lerInt();
                                        if (classificacao < 0 || classificacao > 10){
                                            System.out.println("Valor inválido");
                                            classificacao = -1;
                                        }
                                    } while (classificacao == -1);
                                    this.batatas.classificarVoluntario(c.getClassificaVoluntarios().get(0), classificacao);
                                }
                                do{
                                    menuCliente.executa();
                                    String prod;
                                    int quant;
                                    Produto novo;
                                    Encomendas e;
                                    String codU = c.getCodigo();
                                    int ndE = c.getNdeEnc();
                                    switch (menuCliente.getOpcao()){
                                        case 1:
                                            verLojas();
                                            break;
                                        case 2:
                                            do{
                                                System.out.println("Escolha qual a loja da qual deseja fazer a encomenda");
                                                System.out.println("Insira o nome da loja");
                                                verLojas();
                                                nomeL = lerNome();
                                                l = this.batatas.getLoja(nomeL);
                                            } while (l == null);
                                            e = new Encomendas(l.getCodigo(), codU, 0, "e" + codU + ndE, false, new ArrayList<>(), 0, null, null, null, false);
                                            do{
                                                System.out.println("Escolha os produtos");
                                                System.out.println("Caso deseje sair insira 0");
                                                System.out.println(l.getCatalogo().toString());
                                                prod = lerNome();
                                                if (!prod.equals("0")){
                                                    p = l.getProduto(prod);
                                                    if (p != null){
                                                        do{
                                                            System.out.println("Qual a quantidade do produto desejado?");
                                                            quant = lerInt();
                                                            if (quant < 0 || quant > p.getQuantidade()){
                                                               System.out.println("Quantidade inválida");
                                                               quant = -1; 
                                                            }
                                                        } while (quant == -1);
                                                        novo = new Produto(p.getReferencia(), p.getDescricao(), p.getPreco(), quant, p.getPeso());
                                                        do{
                                                            System.out.println("Pretende adicionar este produto" + novo.toString() + " à encomenda?");
                                                            System.out.println("1 - Sim");
                                                            System.out.println("2 - Não");
                                                            esc = lerBoolean();
                                                        } while (esc == -1);
                                                        if (esc == 1) {
                                                            c.setNdeEnc(c.getNdeEnc() + 1);
                                                            e.setTmpIni(LocalDateTime.now());
                                                            e.addProduto(novo);
                                                            e.setPreco(e.calculaValorTotal());
                                                            e.setPeso(e.calculaPesoTotal());
                                                            for (Produto pr: e.getProduto()){
                                                                l.removeItem(pr.getDescricao(), pr.getQuantidade());
                                                            }
                                                        }
                                                    }
                                                }
                                            } while (!prod.equals("0"));
                                            do{
                                                System.out.println("Pretende fazer a encomenda: " + e.toString());
                                                System.out.println("1 - Sim");
                                                System.out.println("2 - Não");
                                                esc = lerBoolean();
                                            } while (esc == -1);
                                            if (esc == 1){
                                                l.addEncomenda(e);
                                                c.addEncomenda(e);
                                            }
                                            break;
                                        case 3:
                                            verEncomendasU(c);
                                            break;
                                    }
                                } while(menuCliente.getOpcao() != 0);
                            }
                            break;
                        case 2:
                            Voluntario v;
                            System.out.println("Está a dar login como Voluntário");
                            do{
                                do{
                                    System.out.println("Insira o seu email:");
                                    email = lerNome();
                                } while(email == null);
                                if (!email.equals("0")){
                                    do{
                                        System.out.println("Insira a sua password (mínimo, de 6 caracteres e máximo de 16):");
                                        password = lerPassword();
                                    } while(password == null);
                                    login = login(email, password, "Voluntário");
                                }
                            } while(login == false && !email.equals("0"));
                            if (!email.equals("0")){
                                System.out.println("Efetuou login como Voluntário com sucesso");
                                v = this.batatas.getVoluntario().get(email);
                                do{
                                    menuVoluntario.executa();
                                    switch (menuVoluntario.getOpcao()){
                                        case 1:
                                            verEncomendasV(v);
                                            System.out.println("Qual a loja de que deseja transportar a encomenda?");
                                            do{
                                                nomeL = lerNome();
                                                l = this.batatas.getLoja(nomeL);
                                                if (!l.getLocalizacao().inRangeV(v)){
                                                    System.out.println("Essa loja está fora do seu alcance");
                                                    l = null;
                                                }
                                            } while (!l.equals("0") && l == null);
                                            esc = -1;
                                            if (!l.equals("0")){
                                                do{
                                                    System.out.println("Qual a encomenda?");
                                                    int a = verEncomendasVL(l, v);
                                                    esc = lerInt();
                                                    if (esc < 0 || esc > a || !this.batatas.getUtil(l.getEncomenda().get(esc - 1).getCodUtil()).getLocalizacao().inRangeV(v) || !(!l.getEncomenda().get(esc - 1).getMedicamentos() || v.aceitoTransporteMedicamentos())){
                                                        System.out.println("Encomenda inválida");
                                                        esc = -1;
                                                    }
                                                    if (esc == 0) break;
                                                } while(esc < 0);
                                            }
                                            if (!l.equals("0") && esc > 0){
                                                l.getEncomenda().get(esc - 1).setDisponivel(false);
                                                l.getEncomenda().get(esc - 1).setCodBusca(v.getCodigoV());
                                                v.setDisponivel(false);
                                                v.setEncomenda(l.getEncomenda().get(esc - 1));
                                            }
                                            break;
                                        case 2:
                                            do{
                                                System.out.println("Pretende transportar ou não medicamentos?");
                                                System.out.println("1 - Sim");
                                                System.out.println("2 - Não");
                                                esc = lerBoolean();
                                            } while(esc == -1);
                                            break;
                                        case 3:
                                            System.out.println("Confirmou a entrega da sua encomenda");
                                            this.batatas.getL(v.getEncomenda().getCodLoja()).removeEncomenda(v.getEncomenda());
                                            this.batatas.entregaV(v);
                                            this.batatas.getUtil(v.getEncomenda().getCodUtil()).addCVol(v.getCodigoV());
                                            break;
                                        case 4:
                                            if (v.getEncomenda() == null){
                                                System.out.println("Não tem nenhuma encomenda por entregar de momento");
                                            }
                                            else{
                                                System.out.println(v.getEncomenda());
                                            }
                                            break;
                                    }
                                } while(menuVoluntario.getOpcao() != 0);
                            }
                            break;
                        case 3:
                            String nomeP;
                            float preco;
                            int quantidade;
                            float peso;
                            String ref;
                            int escolha;
                            System.out.println("Está a dar login como Loja");
                            do{
                                do{
                                    System.out.println("Insira o email:");
                                    email = lerNome();
                                } while(email == null);
                                if (!email.equals("0")){
                                    do{
                                        System.out.println("Insira a password (mínimo, de 6 caracteres e máximo de 16):");
                                        password = lerPassword();
                                    } while(password == null);
                                    login = login(email, password, "Loja");
                                }
                            } while(login == false && !email.equals("0"));
                            if (!email.equals("0")){
                                l = this.batatas.getLojas().get(email);
                                System.out.println("Efetuou login como Loja com sucesso");
                                do{
                                    menuLoja.executa();
                                    switch (menuLoja.getOpcao()){
                                        case 1:
                                            System.out.println(l.getCatalogo());
                                            break;
                                        case 2:
                                            System.out.println("Escolheu adicionar ao catálogo");
                                            do{
                                                System.out.println("Qual é o produto que deseja adicionar?");
                                                nomeP = lerNome();
                                            } while(nomeP == null);
                                            do{
                                                System.out.println("Qual o preço do produto?");
                                                preco = lerRaio();
                                                if (preco < 0) {
                                                    System.out.println("Preço inválido");
                                                    preco = -1;
                                                }
                                            } while (preco == -1);
                                            do{
                                                System.out.println("Qual a quantidade do produto?");
                                                quantidade = lerInt();
                                            } while(quantidade == -1);
                                            do{
                                                System.out.println("Qual o peso do produto?");
                                                peso = lerRaio();
                                                if (peso < 0) {
                                                    System.out.println("Peso inválido");
                                                    peso = -1;
                                                }
                                            } while(peso == -1);
                                            do{
                                                System.out.println("Qual é a referência do produto?");
                                                ref = lerNome();
                                            } while(ref == null);
                                            Produto produto = new Produto(ref, nomeP, preco, quantidade, peso);
                                            do{
                                                System.out.println("Pretende adicionar ao catálogo: " + produto.toString());
                                                System.out.println("1 - Sim");
                                                System.out.println("2 - Não");
                                                esc = lerBoolean();
                                            } while (esc == -1);
                                            if (esc == 1){
                                                l.addCatalogo(produto);
                                            }
                                            break;
                                        case 3:
                                            verEncomendas(l);
                                            break;
                                        case 4:
                                            System.out.println("Qual das encomendas já se encontra disponível para recolha?");
                                            verEncomendas(l);
                                            do{
                                                esc = lerInt();
                                                if (esc == 0) break;
                                                if (esc < 0 || esc > l.getEncomenda().size()){
                                                    System.out.println("Opção inválida");
                                                    esc = -1;
                                                }
                                                else{
                                                    if (l.getEncomenda().get(esc - 1).isDisponivel()){
                                                        System.out.println("Encomenda já se encontra disponível");
                                                        esc = -1;
                                                    }
                                                }
                                            } while(esc == -1);
                                            l.getEncomenda().get(esc - 1).setDisponivel(true);
                                            do{
                                                System.out.println("A encomenda transporta ou não medicamentos?");
                                                System.out.println("1 - Sim");
                                                System.out.println("2 - Não");
                                                escolha = lerBoolean();
                                            } while(escolha == -1);
                                            if (escolha == 1){
                                                l.getEncomenda().get(esc - 1).setMedicamentos(true);
                                            }
                                            break;
                                    }
                                } while(menuLoja.getOpcao() != 0);
                            }
                            break;
                        case 4:
                            Transportadora t;
                            System.out.println("Está a dar login como Empresa");
                            do{
                                do{
                                    System.out.println("Insira o email:");
                                    email = lerNome();
                                } while(email == null);
                                if (!email.equals("0")){
                                    do{
                                        System.out.println("Insira a password (mínimo, de 6 caracteres e máximo de 16):");
                                        password = lerPassword();
                                    } while(password == null);
                                    login = login(email, password, "Empresa");
                                }
                            } while(login == false && !email.equals("0"));
                            if (!email.equals("0")){
                                System.out.println("Efetuou login como Empresa com sucesso");
                                t = this.batatas.getEmpresas().get(email);
                                do{
                                    menuEmpresa.executa();
                                    switch (menuEmpresa.getOpcao()){
                                        case 1:
                                            if (t.getAceite() > 0){
                                                verEncomendasT(t);
                                                System.out.println("Qual a loja de que deseja transportar a encomenda?");
                                                do{
                                                    nomeL = lerNome();
                                                    l = this.batatas.getLoja(nomeL);
                                                    if (l != null && !l.getLocalizacao().inRangeT(t)){
                                                        System.out.println("Essa loja está fora do seu alcance");
                                                        l = null;
                                                    }
                                                } while (l == null && !nomeL.equals("0"));
                                                esc = 0;
                                                if (!nomeL.equals("0") && l != null){
                                                    do{
                                                        if (l.getEncomenda().size() > 0){
                                                            int a = verEncomendasTL(l, t);
                                                            esc = lerInt();
                                                            if (esc == 0) break;
                                                            if (esc < 0 || esc > a || !this.batatas.getUtil(l.getEncomenda().get(esc - 1).getCodUtil()).getLocalizacao().inRangeT(t) || !(!l.getEncomenda().get(esc - 1).getMedicamentos() || t.aceitoTransporteMedicamentos())){
                                                                System.out.println("Encomenda inválida");
                                                                esc = -1;
                                                            }
                                                        }
                                                        else{
                                                            System.out.println("Essa loja não tem encomendas disponíveis");
                                                        }
                                                    } while(esc < 0);
                                                }
                                                if (!nomeL.equals("0") && esc > 0){
                                                    Encomendas e = new Encomendas(l.getEncomenda().get(esc - 1));
                                                    e.setPreco(Math.round((e.getPreco() + t.calculaPreco(l, this.batatas.getUtil(e.getCodUtil()))) * 100) / 100);
                                                    e.setCodBusca(t.getCodigoT());
                                                    this.batatas.getUtil(e.getCodUtil()).addTransporte(e);
                                                    l.getEncomenda().get(esc - 1).setDisponivel(false);
                                                    t.setAceite(t.getAceite() - 1);
                                                }
                                            }
                                            break;
                                        case 2:
                                            do{
                                                System.out.println("Pretende transportar ou não medicamentos?");
                                                System.out.println("1 - Sim");
                                                System.out.println("2 - Não");
                                                esc = lerBoolean();
                                            } while(esc == -1);
                                            break;
                                        case 3:
                                            if (t.getEncAtuais().size() > 0){
                                                System.out.println("Qual das encomendas ja foi entregue?");
                                                verEncT(t);
                                                do{
                                                    esc = lerInt();
                                                    if (esc == 0) break;
                                                    if (esc < 0 || esc > t.getEncAtuais().size()){
                                                        System.out.println("Encomenda inválida");
                                                        esc = -1;
                                                    }
                                                } while (esc == -1);
                                                if (esc > 0){
                                                    t.setKms(t.getKms() + this.batatas.getUtil(t.getEncAtuais().get(esc - 1).getCodUtil()).getLocalizacao().distEntre(t) + this.batatas.getL(t.getEncAtuais().get(esc - 1).getCodLoja()).getLocalizacao().distEntre(t));
                                                    t.removeEncomenda(t.getEncAtuais().get(esc - 1));
                                                    t.setAceite(t.getAceite() + 1);
                                                }
                                            }
                                            else{
                                                System.out.println("Não tem encomendas por entregar");
                                            }
                                            break;
                                        case 4:
                                            if (t.getEncAtuais().size() == 0){
                                                System.out.println("Não tem nenhuma encomenda por entregar de momento");
                                            }
                                            else{
                                                verEncT(t);
                                            }
                                            break;
                                    }
                                } while(menuEmpresa.getOpcao() != 0);
                            }
                    }
                    break;
                case 3:
                    verTopClientes();
                    break;
                case 4:
                    verTopEmpresas();
                    break;
            }
        } while (menuInicial.getOpcao() != 0); // A opção 0 é usada para sair do menu.
        try {
            this.batatas.gravarObj("estado.obj");
        }
        catch (IOException e) {
            System.out.println("Ops! Não consegui gravar os dados!");
        }
        System.out.println("Até breve!...");
    }

    public String lerPassword(){
        Scanner s = new Scanner(System.in);
        String password = s.nextLine();
        if (password.length() < 6){
            System.out.println("Número insuficiente de caracteres");
            return null;
        }
        if (password.length() > 16){
            System.out.println("Excedeu o limite máximo de caracteres");
            return null;
        }
        return password;
    }

    public String lerNome(){
        Scanner s = new Scanner(System.in);
        String nome = s.nextLine();
        return nome;
    }

    public GPS lerGPS(){
        Scanner s = new Scanner(System.in);
        GPS r = new GPS();
        try{
            r.setLatitude(s.nextDouble());
            r.setLongitude(s.nextDouble());
        }
        catch(InputMismatchException e){
            System.out.println("As coordenadas precisam de ser números");
            return null;
        }
        if (r.getLatitude() < -85 || r.getLatitude() > 85 || r.getLongitude() < -180 || r.getLongitude() > 180){
            System.out.println("Coordenadas invalidas");
            return null;
        }
        return r;
    }

    public float lerRaio(){
        Scanner s = new Scanner(System.in);
        float r;
        try{
            r = s.nextFloat();
        }
        catch(InputMismatchException e){
            System.out.println("Convem ser um numero filhinho");
            return -1;
        }
        if (r < 0){
            System.out.println("Número inválido");
            r = -1;
        }
        return r;
    }

    public int lerBoolean(){
        Scanner s = new Scanner(System.in);
        int r;
        try{
            r = s.nextInt();
        }
        catch (InputMismatchException e) {;
            r = -1;
        }
        if (r != 1 && r != 2){
            System.out.println("Não foi escolhida uma das opções");
            r = -1;
        }
        return r;
    }

    public double lerEspera(){
        Scanner s = new Scanner(System.in);
        double r;
        try{
            r = s.nextDouble();
        }
        catch (InputMismatchException e) {
            r = -1;
        }
        return r;
    }

    public int lerInt(){
        Scanner s = new Scanner(System.in);
        int r;
        try{
            r = s.nextInt();
        }
        catch (InputMismatchException e){
            System.out.println("Por favor insira em modo numérico");
            r = -1;
        }
        return r;
    }

    public boolean login(String email, String password, String cod){
        if (!batatas.existeUtilizador(cod, email)){
            System.out.println("Esse email não está registado");
            return false;
        }
        if (password.equals(batatas.getUtilizador(cod, email).getPassword())){
            return true;
        }
        System.out.println("A password está errada");
        return false;
    }

    public void verLojas(){
        int i = 1;
        for(Lojas l: this.batatas.getLojas().values()){
            System.out.print(i);
            System.out.print(" - ");
            System.out.println(l.toString());
            i++;
        }
    }

    public int escLoja(){
        Scanner s = new Scanner(System.in);
        int op = -1;
        verLojas();
        try{
            op = s.nextInt();
        }
        catch(InputMismatchException e){
            op = -1;
        }
        if (op < 0 || op > this.batatas.getLojas().size()){
            System.out.println("Opção inválida");
            op = -1;
        }
        return op;
    }

    public void verEncomendas(Lojas l){
        int i = 1;
        for(Encomendas e: l.getEncomenda()){
            System.out.print(i);
            System.out.print(" - ");
            System.out.println(e.toString());
            System.out.println(e.isDisponivel());
            i++;
        }
    }

    public void verEncomendasU(Cliente c){
        int i = 1;
        for(Encomendas e: c.getEncomendas()){
            System.out.print(i);
            System.out.print(" - ");
            System.out.println(e.toString());
        }
    }

    public void verEncomendasV(Voluntario v){
        for (Lojas l: this.batatas.getLojas().values()){
            if (l.getLocalizacao().inRangeV(v)){
                System.out.println(l.toString());
                for (Encomendas e: l.getNaoAceite()){
                    if (this.batatas.getUtil(e.getCodUtil()).getLocalizacao().inRangeV(v) && (!e.getMedicamentos() || e.getMedicamentos() == v.aceitoTransporteMedicamentos())){
                        System.out.println(e.toString());
                    }
                }
            }
        }
    }

    public int verEncomendasVL(Lojas l, Voluntario v){
        int r = 0;
        for (Encomendas e: l.getEncomenda()){
            if (this.batatas.getUtil(e.getCodUtil()).getLocalizacao().inRangeV(v) && (!e.getMedicamentos() || e.getMedicamentos() == v.aceitoTransporteMedicamentos())){
                System.out.print(r + 1);
                System.out.print(" - ");
                System.out.println(e.toString());
            }
            r += 1;
        }
        return r;
    }

    public void verEncomendasT(Transportadora t){
        for (Lojas l: this.batatas.getLojas().values()){
            if (l.getLocalizacao().inRangeT(t)){
                System.out.println(l.toString());
                for (Encomendas e: l.getNaoAceite()){
                    if (this.batatas.getUtil(e.getCodUtil()).getLocalizacao().inRangeT(t) && (!e.getMedicamentos() || e.getMedicamentos() == t.aceitoTransporteMedicamentos())){
                        System.out.println(e.toString());
                    }
                }
            }
        }
    }

    public int verEncomendasTL(Lojas l, Transportadora t){
        int r = 0;
        for (Encomendas e: l.getEncomenda()){
            if (this.batatas.getUtil(e.getCodUtil()).getLocalizacao().inRangeT(t) && (!e.getMedicamentos() || e.getMedicamentos() == t.aceitoTransporteMedicamentos())){
                System.out.print(r + 1);
                System.out.print(" - ");
                System.out.println(e.toString());
            }
            r += 1;
        }
        return r;
    }

    public void verTopClientes(){
        List<Cliente> l = this.batatas.ordenarCliente(this.batatas.getComparadores());
        for (int i = 0; i < 10 && i < l.size(); i++){
            System.out.print(i + 1);
            System.out.print(" - ");
            System.out.println(l.get(i));
        }
    }

    public void verTopEmpresas(){
        List<Transportadora> l = this.batatas.ordenarTransportadora(this.batatas.getComparadoresT());
        for (int i = 0; i < 10 && i < l.size(); i++){
            System.out.print(i + 1);
            System.out.print(" - ");
            System.out.println(l.get(i));
        }
    }

    public void verEncT(Transportadora t){
        int i = 1;
        for (Encomendas e: t.getEncAtuais()){
            System.out.print(i + 1);
            System.out.print(" - ");
            System.out.println(e);
            i++;
        }
    }
}