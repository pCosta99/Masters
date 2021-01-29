
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.TreeMap;
import java.util.Random;
import java.util.TreeSet;

public class Controller
{


    public static void principal() {
        Menu mp, mu,mv,mt,ml,me,mev,met;
        char voltar = ' ';
        SistemaGestao sg = new SistemaGestao();
        Random gerador = new Random();
        int opp,opu,opv,opt,opl,ope,opev,opet;
        Output.clearScreen();
        mp = Output.menuPrincipal();
        mp.executa();
        opp = mp.getOpcao();
        while(opp!=0){
            voltar = ' ';
            switch (opp) {
                case 1 : {
                    Output.clearScreen();
                    Output.registaEnt();
                    me = Output.menuEntidade();
                    me.executa();
                    ope = me.getOpcao();
                    while (ope!=0){
                        voltar = ' ';
                        Output.clearScreen();
                        switch (ope) {
                            case 1: {
                                Output.clearScreen();
                                Output.inserirCampos();
                                Output.indicaNome();
                                String nome = Input.lerString();
                                String random = "u" + String.valueOf(gerador.nextInt(10000));
                                while (sg.codigoValido(random)) {
                                    random = "u" + String.valueOf(gerador.nextInt(10000));
                                }
                                Output.indicaCoord();
                                double x = Input.lerDouble();
                                double y = Input.lerDouble();
                                Output.indicaEmail();
                                String email = Input.lerString();
                                Output.indicaPass();
                                String passe = Input.lerString();
                                sg.registaUtilizador(nome, random, x, y,email,passe);
                                Output.voltar();
                                while(Character.toUpperCase(voltar) != 'V') {
                                    voltar = Input.lerString().charAt(0);
                                }
                                Output.clearScreen();
                                me.executa();
                                ope = me.getOpcao();
                                break;
                            }

                            case 2: {
                                Output.clearScreen();
                                Output.inserirCampos();
                                Output.indicaNome();
                                String nome = Input.lerString();
                                String random = "v" + String.valueOf(gerador.nextInt(10000));
                                while (sg.codigoValido(random)) {
                                    random = "v" + String.valueOf(gerador.nextInt(10000));
                                }
                                Output.indicaCoord();
                                double x = Input.lerDouble();
                                double y = Input.lerDouble();
                                Output.indicaRaio();
                                double raio = Input.lerDouble();
                                Output.indicaMed();
                                boolean aceitaMed;
                                String auxMed = Input.lerString();
                                aceitaMed = auxMed.equals("s");
                                Output.indicaEmail();
                                String email = Input.lerString();
                                Output.indicaPass();
                                String passe = Input.lerString();
                                sg.registaVoluntario(nome, random, x, y, raio, aceitaMed,email,passe);
                                Output.voltar();
                                while(Character.toUpperCase(voltar) != 'V') {
                                    voltar = Input.lerString().charAt(0);
                                }
                                Output.clearScreen();
                                me.executa();
                                ope = me.getOpcao();
                                break;
                            }

                            case 3: {
                                Output.clearScreen();
                                Output.inserirCampos();
                                Output.indicaNome();
                                String nome = Input.lerString();
                                String random = "l" + String.valueOf(gerador.nextInt(10000));
                                while (sg.codigoValido(random)) {
                                    random = "l" + String.valueOf(gerador.nextInt(10000));
                                }
                                Output.indicaCoord();
                                double x = Input.lerDouble();
                                double y = Input.lerDouble();
                                Output.indicaEmail();
                                String email = Input.lerString();
                                Output.indicaPass();
                                String passe = Input.lerString();
                                sg.registaLoja(random, nome, x, y,email,passe);
                                Output.voltar();
                                while(Character.toUpperCase(voltar) != 'V') {
                                    voltar = Input.lerString().charAt(0);
                                }
                                Output.clearScreen();
                                me.executa();
                                ope = me.getOpcao();
                                break;
                            }

                            case 4: {
                                Output.clearScreen();
                                Output.inserirCampos();
                                Output.indicaNome();
                                String nome = Input.lerString();
                                String random = "t" + String.valueOf(gerador.nextInt(10000));
                                while (sg.codigoValido(random)) {
                                    random = "t" + String.valueOf(gerador.nextInt(10000));
                                }
                                Output.indicaCoord();
                                double x = Input.lerDouble();
                                double y = Input.lerDouble();
                                Output.indicaNif();
                                String nif = Input.lerString();
                                Output.indicaTaxa();
                                double taxa = Input.lerDouble();
                                Output.indicaRaio();
                                double raio = Input.lerDouble();
                                Output.indicaCap();
                                int capacidade = Input.lerInt();
                                Output.indicaMed();
                                boolean aceitaMed;
                                String auxMed = Input.lerString();
                                aceitaMed = auxMed.equals("s");
                                Output.indicaEmail();
                                String email = Input.lerString();
                                Output.indicaPass();
                                String passe = Input.lerString();
                                sg.registaTransportadora(random, nome, nif, x, y, taxa, raio, capacidade, aceitaMed,email,passe);
                                Output.voltar();
                                while(Character.toUpperCase(voltar) != 'V') {
                                    voltar = Input.lerString().charAt(0);
                                }
                                Output.clearScreen();
                                me.executa();
                                ope = me.getOpcao();
                                break;

                            }

                            default: {
                                Output.clearScreen();
                                Output.opcaoInvalida();
                                me.executa();
                                ope = me.getOpcao();
                                break;
                            }
                        }
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;

                }

                case 2 : {
                    boolean b=true;
                    Output.clearScreen();
                    Output.credenciais();
                    String email = Input.lerString();
                    String passe = Input.lerString();
                    String codigo = sg.validaCredenciais(email, passe);
                    if(codigo.equals("")){
                        b = false;
                        while (codigo.equals("")&&!b) {
                            Output.credenciaisInvalidos();
                            Output.tentarNovo();
                            String c = Input.lerString();
                            b = c.equals("s") ? true : false;
                            if(b){
                                email = Input.lerString();
                                passe = Input.lerString();
                                codigo = sg.validaCredenciais(email, passe);
                            }
                            else break;
                        }
                    }
                    if(b){
                        Output.credenciaisValidos();
                        try{
                            Entidade e = sg.getUtilizadorSistema(codigo);
                            String tipo = e.getClass().getSimpleName();

                            switch (tipo) {
                                case "Utilizador": {
                                    Output.clearScreen();
                                    Utilizador u = (Utilizador) e;
                                    mu = Output.menuU();
                                    mu.executa();
                                    opu = mu.getOpcao();
                                    while(opu!=0){
                                        voltar = ' ';
                                        switch (opu) {

                                            case 1 : {
                                                Output.clearScreen();
                                                Output.carrinho(u);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mu.executa();
                                                opu = mu.getOpcao();
                                                break;
                                            }

                                            case 2: {
                                                Output.clearScreen();
                                                Output.lojasDisponiveis();
                                                try {
                                                    Output.lojas(sg);
                                                    Output.digiteLoja();
                                                    String codLoja = Input.lerString();
                                                    Loja l = (Loja) sg.getUtilizadorSistema(codLoja);
                                                    String random = "e" + String.valueOf(gerador.nextInt(10000));
                                                    while (sg.codigoValido(random)) {
                                                        random = String.valueOf(gerador.nextInt(10000));
                                                    }
                                                    Encomenda en = new Encomenda(e.getCodigo(), l.getCodigo(), 0, new TreeMap<>(), random,0,0,0,0,0,0);
                                                    Output.produtosDisponiveis();
                                                    Output.catalogo(sg,l);
                                                    Output.produtosAcomprar();
                                                    String cont = new String();
                                                    while (!cont.equals("q")) {
                                                        Output.novoProd();
                                                        Output.indicaCod();
                                                        String codp = Input.lerString();
                                                        try {
                                                            LinhaEnc le1 = l.getLinhaEncomenda(codp);
                                                            Output.indicaQuant();
                                                            double quant = Input.lerDouble();
                                                            String desc = le1.getDescricao();
                                                            double preco = le1.getValorUni();
                                                            double peso = le1.getPesoUni();
                                                            boolean prodMed = le1.getProdMed();
                                                            LinhaEnc le = new LinhaEnc(codp, desc, quant, preco, peso, prodMed);
                                                            en.insereLinhaEnc(le);
                                                        } catch (CodigoInvalidoException exception) {
                                                            Output.excCodInv(exception.getMessage());
                                                        }
                                                        cont = Input.lerString();
                                                    }
                                                    Output.transpDisponiveis();
                                                    Output.filtraTransportadoras(sg,en,u,l);
                                                    Output.selecionaTrans();
                                                    String codT = Input.lerString();
                                                    try{
                                                        EntidadeEntrega tr = (EntidadeEntrega) sg.getUtilizadorSistema(codT);
                                                        en.alteraParametros(u.getXGPS(),u.getYGPS(),l.getXGPS(),l.getYGPS(),tr.getXGPS(),tr.getYGPS());

                                                        if (tr instanceof Voluntario) {
                                                            Voluntario v = (Voluntario) tr;
                                                            v.aceitaEncV();
                                                            v.setCodEncATransportar(en.getCodEncomenda());
                                                            v.setEncomendaAtransportar(en);
                                                            sg.addUtilizadorSistema(v);
                                                        } else {
                                                            Transportadora t = (Transportadora) tr;
                                                            t.aceitaEncT(en);
                                                            t.insereEncAChegar(en);
                                                            sg.addUtilizadorSistema(t);
                                                        }
                                                        u.insereCarrinho(en);
                                                        l.insereEncEntregar(en);
                                                        l.haEncEntregar();
                                                        sg.addUtilizadorSistema(u);
                                                        sg.addUtilizadorSistema(l);
                                                        sg.addEncomendaSistema(en);
                                                       
                                                        Output.sucessoEnc();
                                                    } catch (ConjuntoVazioException exception) {
                                                        Output.traNaoExiste();
                                                    }
                                                } catch (ConjuntoVazioException exception) {
                                                    Output.lojNaoExiste();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mu.executa();
                                                opu = mu.getOpcao();
                                                break;
                                            }

                                            case 3: {
                                                Output.clearScreen();
                                                EntidadeEntrega ee = (EntidadeEntrega) sg.getUtilizadorSistema(u.getCodUltimoTransportador());
                                                Output.introduzClass();
                                                double c = Input.lerDouble();
                                                try {
                                                    ee.adicionaClassificacao(c);
                                                    sg.addUtilizadorSistema(ee);
                                                } catch (ValoresNaoValidosException | NaoHaServicosException exception) {
                                                    Output.classificacaoInvalida();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mu.executa();
                                                opu = mu.getOpcao();
                                                break;


                                            }

                                            case 4: {
                                                Output.clearScreen();
                                                Output.registos(sg,codigo);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mu.executa();
                                                opu = mu.getOpcao();
                                                break;
                                            }

                                            case 5: {
                                                Output.clearScreen();
                                                try {
                                                    Output.codEncs(u);
                                                } catch (ConjuntoVazioException exception) {
                                                    Output.traNaoExiste();
                                                }
                                                Output.inserirCodEnc();
                                                String codEnc = Input.lerString();
                                                try {
                                                    Encomenda enc = sg.getEncomendaSistema(codEnc);
                                                    Loja l = (Loja) sg.getUtilizadorSistema(enc.getVendedor());
                                                    Output.transpDisponiveis();
                                                    Output.filtraTransportadoras(sg,enc,u,l);
                                                    Output.selecionaTrans();
                                                    String codT = Input.lerString();
                                                    try{
                                                        EntidadeEntrega tr = (EntidadeEntrega) sg.getUtilizadorSistema(codT);
                                                        enc.alteraParametros(u.getXGPS(),u.getYGPS(),l.getXGPS(),l.getYGPS(),tr.getXGPS(),tr.getYGPS());

                                                        if (tr instanceof Voluntario) {
                                                            Voluntario v = (Voluntario) tr;
                                                            v.aceitaEncV();
                                                            v.setCodEncATransportar(enc.getCodEncomenda());
                                                            v.setEncomendaAtransportar(enc);
                                                            sg.addUtilizadorSistema(v);
                                                        } else {
                                                            Transportadora t = (Transportadora) tr;
                                                            t.aceitaEncT(enc);
                                                            t.insereEncAChegar(enc);
                                                            sg.addUtilizadorSistema(t);
                                                        }
                                                        l.insereEncEntregar(enc);
                                                        l.haEncEntregar();
                                                        u.insereCarrinho(enc);
                                                        u.removeEncAceitesLogs(enc);
                                                        sg.addUtilizadorSistema(l);
                                                        sg.addUtilizadorSistema(u);
                                                        sg.addEncomendaSistema(enc);
                                                        
                                                        Output.sucessoEnc();
                                                    } catch (ConjuntoVazioException exception){
                                                        Output.entNaoExiste();
                                                    }
                                                } catch (ConjuntoVazioException exception) {
                                                    Output.encNaoExiste();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mu.executa();
                                                opu = mu.getOpcao();
                                                break;
                                            }
                                            default:{
                                                Output.clearScreen();
                                                Output.opcaoInvalida();
                                                mu.executa();
                                                opu = mu.getOpcao();
                                                break;
                                            }
                                        }
                                    }
                                    sg.addUtilizadorSistema(u);
                                    break;
                                }

                                case "Voluntario": {
                                    Output.clearScreen();
                                    Voluntario v = (Voluntario) e;
                                    mv = Output.menuV();
                                    mv.executa();
                                    opv = mv.getOpcao();
                                    while(opv!=0) {
                                        voltar = ' ';
                                        switch (opv) {

                                            case 1: { 
                                                Output.clearScreen();
                                                Output.volDispo();
                                                String disposicao = Input.lerString();
                                                v.setDisposicao(disposicao.equals("d") || disposicao.equals("D") ? true : false);
                                                sg.addUtilizadorSistema(v);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;
                                            }

                                            case 2: {
                                                Output.clearScreen();
                                                Output.pretende();
                                                mev = Output.menuEncV();
                                                mev.executa();
                                                opev = mev.getOpcao();
                                                while (opev != 0) {
                                                    voltar = ' ';
                                                    switch (opev) {

                                                        case 1: {
                                                            Output.clearScreen();
                                                            try {
                                                                Encomenda enc = sg.getEncomendaSistema(v.getCodEncATransportar());
                                                                Utilizador u = (Utilizador) sg.getUtilizadorSistema(enc.getDestinatario());
                                                                Loja l = (Loja) sg.getUtilizadorSistema(enc.getVendedor());
                                                                v.acabaEnc(v, enc, u.getXGPS(), u.getYGPS(), l.getXGPS(), l.getYGPS());
                                                                u.insereEnc(enc);
                                                                u.removeCarrinho(enc);
                                                                l.insereEnc(enc);
                                                                enc.setInicio(v.getInicio());
                                                                enc.setFim(v.getFim());
                                                                enc.setTempoEntrega(v.getTempoEntrega());
                                                                v.resetTempo();
                                                                v.setEncomendaAtransportar(new Encomenda());
                                                                double kms = v.calculaDistancia(u.getXGPS(), u.getYGPS(), l.getXGPS(), l.getYGPS());
                                                                double acc = v.getKmTotais();
                                                                int nrServ = v.getQuantServicos();
                                                                enc.setKmDeslocacao(kms);
                                                                v.setKmTotais(acc+kms);
                                                                v.setQuantServicos(nrServ+1);
                                                                u.setCodUltimoTransportador(v.getCodigo());
                                                                l.removeEnc(enc.getCodEncomenda());
                                                                l.haEncEntregar();
                                                                sg.addUtilizadorSistema(u);
                                                                sg.addUtilizadorSistema(v);
                                                                sg.addUtilizadorSistema(l);
                                                                sg.addEncomendaSistema(enc);
                                                                Output.encomendaFeita();
                                                            } catch (ConjuntoVazioException exception) {
                                                                Output.encNaoExiste();
                                                            }
                                                            Output.voltar();
                                                            while(Character.toUpperCase(voltar) != 'V') {
                                                                voltar = Input.lerString().charAt(0);
                                                            }
                                                            Output.clearScreen();
                                                            mev.executa();
                                                            opev = mev.getOpcao();
                                                            break;

                                                        }

                                                        case 2: {
                                                            Output.clearScreen();
                                                            try {
                                                                Encomenda enc = sg.getEncomendaSistema(v.getCodEncATransportar());
                                                                Output.imprimeCoord(sg,enc);
                                                            } catch (ConjuntoVazioException exception) {
                                                                Output.encNaoExiste();
                                                            }
                                                            Output.voltar();
                                                            while(Character.toUpperCase(voltar) != 'V') {
                                                                voltar = Input.lerString().charAt(0);
                                                            }
                                                            Output.clearScreen();
                                                            mev.executa();
                                                            opev = mev.getOpcao();
                                                            break;
                                                        }
                                                        default:
                                                            Output.clearScreen();
                                                            Output.opcaoInvalida();
                                                            mev.executa();
                                                            opev = mev.getOpcao();
                                                            break;
                                                    }
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;

                                            }

                                            case 3: {
                                                Output.clearScreen();
                                                Output.encomendaTempos();
                                                try {
                                                    Output.codEncs(sg,v);
                                                    String codEnc = Input.lerString();
                                                    try {
                                                        Output.tempos(sg,v,codEnc);
                                                    } catch (CodigoInvalidoException exception) {
                                                        Output.printException(exception.getMessage());
                                                    }
                                                } catch (ConjuntoVazioException exception) {
                                                    Output.traSemEnc();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;
                                            }

                                            case 4: {
                                                Output.clearScreen();
                                                Output.utilizadorSistema(sg,v);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;
                                            }

                                            case 5: {
                                                Output.clearScreen();
                                                Output.parametros(sg,v);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;
                                            }

                                            case 6: {
                                                Output.clearScreen();
                                                Output.indicaMed();
                                                String c = Input.lerString();
                                                v.aceitaMedicamentos(c.equals("s") ? true : false);
                                                sg.addUtilizadorSistema(v);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;
                                            }
                                            default:
                                                Output.clearScreen();
                                                Output.opcaoInvalida();
                                                mv.executa();
                                                opv = mv.getOpcao();
                                                break;
                                        }
                                    }
                                    sg.addUtilizadorSistema(v);
                                    break;

                                }

                                case "Transportadora": {
                                    Output.clearScreen();
                                    Transportadora t = (Transportadora) e;
                                    mt = Output.menuT();
                                    mt.executa();
                                    opt = mt.getOpcao();
                                    while(opt!=0) {
                                        voltar = ' ';
                                        switch (opt) {

                                            case 1: {
                                                Output.clearScreen();
                                                Output.pretende();
                                                met = Output.menuEncT();
                                                met.executa();
                                                opet = met.getOpcao();
                                                while(opet!=0){
                                                    voltar = ' ';
                                                    switch (opet) {

                                                        case 1: {
                                                            Output.clearScreen();
                                                            TreeSet<Encomenda> ts = t.ordenaPorDistancia();
                                                            try {
                                                                Encomenda enc = t.encAfazer(ts);
                                                                Utilizador u = (Utilizador) sg.getUtilizadorSistema(enc.getDestinatario());
                                                                Loja l = (Loja) sg.getUtilizadorSistema(enc.getVendedor());
                                                                enc.setKmDeslocacao(t.calculaDistancia(u.getXGPS(), u.getYGPS(), l.getXGPS(), l.getYGPS()));
                                                                l.insereEnc(enc);
                                                                u.insereEnc(enc);
                                                                u.removeCarrinho(enc);
                                                                u.setCodUltimoTransportador(t.getCodigo());
                                                                l.removeEnc(enc.getCodEncomenda());
                                                                t.removeEnc(enc.getCodEncomenda());
                                                                t.remEncAfazer(ts);
                                                                l.haEncEntregar();
                                                                t.acabaEnc(t, enc, u.getXGPS(), u.getYGPS(), l.getXGPS(), l.getYGPS());
                                                                sg.addUtilizadorSistema(t);
                                                                sg.addUtilizadorSistema(l);
                                                                sg.addUtilizadorSistema(u);
                                                                sg.addEncomendaSistema(enc);
                                                                Output.encomendaFeita();
                                                                Output.encFeita(enc);
                                                            } catch (ConjuntoVazioException exception) {
                                                                Output.naoHaEnc();
                                                            }
                                                            Output.voltar();
                                                            while(Character.toUpperCase(voltar) != 'V') {
                                                                voltar = Input.lerString().charAt(0);
                                                            }
                                                            Output.clearScreen();
                                                            met.executa();
                                                            opet = met.getOpcao();
                                                            break;
                                                        }

                                                        case 2: {
                                                            Output.clearScreen();
                                                            TreeSet<Encomenda> ts = t.ordenaPorDistancia();
                                                            try {
                                                                Encomenda enc = t.encAfazer(ts);
                                                                Output.imprimeCoord(sg,enc);
                                                            } catch (ConjuntoVazioException exception) {
                                                                Output.naoHaEnc();
                                                            }
                                                            Output.voltar();
                                                            while(Character.toUpperCase(voltar) != 'V') {
                                                                voltar = Input.lerString().charAt(0);
                                                            }
                                                            Output.clearScreen();
                                                            met.executa();
                                                            opet = met.getOpcao();
                                                            break;
                                                        }
                                                        default:
                                                            Output.clearScreen();
                                                            Output.opcaoInvalida();
                                                            met.executa();
                                                            opet = met.getOpcao();
                                                            break;
                                                    }
                                                }
                                                Output.clearScreen();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                            }

                                            case 2: {
                                                Output.clearScreen();
                                                Output.encomendaTempos();
                                                try {
                                                    Output.codEncs(sg,t);
                                                    String codEnc = Input.lerString();
                                                    try {
                                                        Output.tempos(sg,t,codEnc);
                                                    } catch (CodigoInvalidoException exception) {
                                                        Output.printException(exception.getMessage());
                                                    }
                                                } catch (ConjuntoVazioException exception) {
                                                    Output.traSemEnc();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                            }

                                            case 3: {
                                                Output.clearScreen();
                                                Output.utilizadorSistema(sg,t);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                            }

                                            case 4: {
                                                Output.clearScreen();
                                                Output.parametros(sg,t);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                            }

                                            case 5: {
                                                Output.clearScreen();
                                                Output.indicaMed();
                                                String c = Input.lerString();
                                                t.aceitaMedicamentos(c.equals("s") ? true : false );
                                                sg.addUtilizadorSistema(t);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                            }

                                            case 6: {
                                                Output.clearScreen();
                                                Output.dataInicio();
                                                Output.dia();
                                                try {
                                                    int diaI = Input.lerInt();
                                                    Output.mes();
                                                    int mesI = Input.lerInt();
                                                    Output.ano();
                                                    int anoI = Input.lerInt();
                                                    Output.hora();
                                                    int horaI = Input.lerInt();
                                                    Output.minutos();
                                                    int minI = Input.lerInt();
                                                    LocalDateTime inicio = LocalDateTime.of(anoI, mesI, diaI, horaI, minI);
                                                    Output.dataFim();
                                                    Output.dia();
                                                    int diaF = Input.lerInt();
                                                    Output.mes();
                                                    int mesF = Input.lerInt();
                                                    Output.ano();
                                                    int anoF = Input.lerInt();
                                                    Output.hora();
                                                    int horaF = Input.lerInt();
                                                    Output.minutos();
                                                    int minF = Input.lerInt();
                                                    LocalDateTime fim = LocalDateTime.of(anoF, mesF, diaF, horaF, minF);
                                                    Output.faturacaoTransp(sg, inicio, fim, t);
                                                } catch (DateTimeException exception){
                                                     Output.dataInvalida();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                            }
                                            default:
                                                Output.clearScreen();
                                                Output.opcaoInvalida();
                                                mt.executa();
                                                opt = mt.getOpcao();
                                                break;
                                        }
                                    }
                                    sg.addUtilizadorSistema(t);
                                    break;
                                }

                                case "Loja": {
                                    Output.clearScreen();
                                    Loja l = (Loja) e;
                                    ml = Output.menuL();
                                    ml.executa();
                                    opl = ml.getOpcao();
                                    while (opl != 0) {
                                        voltar = ' ';
                                        switch (opl) {

                                            case 1: {
                                                Output.clearScreen();
                                                String c = new String();
                                                while (!c.equals("n")) {
                                                    Output.criaProduto();
                                                    Output.indicaDesc();
                                                    String desc = Input.lerString();
                                                    Output.indicaPreco();
                                                    double preco = Input.lerDouble();
                                                    Output.indicaPeso();
                                                    double peso = Input.lerDouble();
                                                    String random = String.valueOf(gerador.nextInt(10000));
                                                    while (sg.
                                                            codigoValido(random)) {
                                                        random = String.valueOf(gerador.nextInt(10000));
                                                    }
                                                    Output.prodMed();
                                                    boolean prodMed;
                                                    String auxMed = Input.lerString();
                                                    prodMed = auxMed.equals("s");
                                                    try {
                                                        l.insereNovoProduto(random, desc, preco, peso, prodMed);

                                                    } catch (ValoresNaoValidosException exception) {
                                                        Output.pesoPreco();
                                                    }
                                                    Output.maisProd();
                                                    c = Input.lerString();
                                                }
                                                sg.addUtilizadorSistema(l);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                ml.executa();
                                                opl = ml.getOpcao();
                                                break;
                                            }

                                            case 2: {
                                                Output.clearScreen();
                                                Output.encomendas(l);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                ml.executa();
                                                opl = ml.getOpcao();
                                                break;
                                            }

                                            case 3: {
                                                Output.clearScreen();
                                                Output.utilizadorSistema(sg,codigo);
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                ml.executa();
                                                opl = ml.getOpcao();
                                                break;
                                            }

                                            case 4: {
                                                Output.clearScreen();
                                                Output.catalogo();
                                                try {
                                                    Output.catalogo(sg,l);
                                                    Output.removeProduto();
                                                    String codProd = Input.lerString();
                                                    try {
                                                        sg.removeProd(l.getCodigo(), codProd);
                                                    } catch (CodigoInvalidoException exception) {
                                                        Output.printException(exception.getMessage());
                                                    }
                                                } catch (ConjuntoVazioException exception) {
                                                    Output.catSemProd();
                                                }
                                                Output.voltar();
                                                while(Character.toUpperCase(voltar) != 'V') {
                                                    voltar = Input.lerString().charAt(0);
                                                }
                                                Output.clearScreen();
                                                ml.executa();
                                                opl = ml.getOpcao();
                                                break;
                                            }
                                            default:
                                                Output.clearScreen();
                                                Output.opcaoInvalida();
                                                ml.executa();
                                                opl = ml.getOpcao();
                                                break;
                                        }
                                    }
                                    sg.addUtilizadorSistema(l);
                                    break;
                                }
                                default:
                                    break;

                            }

                        } catch (ConjuntoVazioException exception){
                            Output.entNaoExiste();
                        }
                    }
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                case 3 : {
                    Output.clearScreen();
                    Output.sg(sg);
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                case 4 : {
                    Output.clearScreen();
                    try {
                        Output.utiMaisUsados(sg);
                    } catch (ConjuntoVazioException exception) {
                        Output.semUtiException();
                    }
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                case 5 : {
                    Output.clearScreen();
                    try {
                        Output.traMaisUsadas(sg);
                    } catch (ConjuntoVazioException exception) {
                        Output.semTraException();
                    }
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                case 6 : {
                    Output.clearScreen();
                    String path = new String();
                    Output.insiraPath();
                    path = Input.lerString();
                    Output.clearScreen();
                    try{
                        sg.lerCSV(path);
                        Output.carregado();
                    } catch (IOException exception){
                        Output.ficheiroException();
                    }
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                case 7 : {
                    Output.clearScreen();
                    try{
                        sg.guardaEstado("logsOBJ.obj");
                        Output.guardado();
                    }catch (IOException exception) {
                        Output.guardarException();
                    }
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                case 8 : {
                    Output.clearScreen();
                    try{
                        sg = sg.carregaEstado("logsOBJ.obj");
                        Output.carregado();
                    }catch (IOException | ClassNotFoundException exception) {
                        Output.guardarException();
                    }
                    Output.voltar();
                    while(Character.toUpperCase(voltar) != 'V') {
                        voltar = Input.lerString().charAt(0);
                    }
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }

                default : {
                    Output.clearScreen();
                    Output.opcaoInvalida();
                    Output.clearScreen();
                    mp.executa();
                    opp = mp.getOpcao();
                    break;
                }
            }
        }
    }
}