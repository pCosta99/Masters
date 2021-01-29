/**
 *
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;
import java.util.Date;
import java.util.Scanner; 
import java.text.SimpleDateFormat; 
import java.text.ParseException; 
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class GestaoApp implements Serializable {
    
    /* 
     * 
     * variaveis de instancia
     */
    private GestaoEntidades gestEntidades;
    private GestaoEncomendas gestEncomendas;
    
    /* 
     * 
     * construtores
     */
    public GestaoApp() {
        this.gestEntidades = new GestaoEntidades();
        this.gestEncomendas = new GestaoEncomendas();
    }
    
    public GestaoApp(GestaoEntidades gestEntidades, GestaoEncomendas gestEncomendas) {
        this.gestEntidades = gestEntidades.clone();
        this.gestEncomendas = gestEncomendas.clone();
    }
    
    public GestaoApp(GestaoApp gestApp) {
        this.gestEntidades = gestApp.getGestEntidades();
        this.gestEncomendas = gestApp.getGestEncomendas();
    }
    
    /* 
     * 
     * metodos de instancia
     */
    public GestaoEntidades getGestEntidades() {
        return this.gestEntidades.clone();
    }
    
    public Entidade getEntidade(String codigoEntidade) {
        return this.gestEntidades.getEntidade(codigoEntidade);
    }
    
    public void addEntidade(Entidade e) {
        this.gestEntidades.addEntidade(e);
    }
    
    public List<Entidade> getClassificaveis() {
        return this.gestEntidades.getClassificaveis();
    }
    
    public GestaoEncomendas getGestEncomendas() {
        return this.gestEncomendas.clone();
    }
    
    public void addEncomenda(Encomenda e) {
        this.gestEncomendas.addEncomenda(e);
    }
    
    public void removeEncomenda(String codEncomenda) {
        this.gestEncomendas.removeEncomenda(codEncomenda);
    }
    
    public Encomenda getEncomenda(String codEncomenda) {
         return this.gestEncomendas.getEncomenda(codEncomenda); 
    }
    
    public void setEstadoEncomenda(String codEnc, EstadoEncomenda estadoEncomenda) {
        this.gestEncomendas.setEstadoEncomenda(codEnc, estadoEncomenda);
    }
    
    public void setDistanciaEncomenda(String codEncomenda, double distancia) {
        this.gestEncomendas.setDistanciaEncomenda(codEncomenda, distancia);
    }

    public void setEstadoEncomendas(List<String> codigosEncs, EstadoEncomenda estadoEncomenda) {
         this.gestEncomendas.setEstadoEncomendas(codigosEncs, estadoEncomenda);
    }
    
    public List<Encomenda> getEncomendasPendentesUtilizador(String codUtilizador) {
         return gestEncomendas.getEncomendasPendentesUtilizador(codUtilizador);
    }
     
    public List<String> getCodEncomendasPendentesUtilizador(String codUtilizador) {
         return gestEncomendas.getCodEncomendasPendentesUtilizador(codUtilizador);
    }
    
    public List<Encomenda> getEncomendasEntreguesUtilizador(String codUtilizador) {
         return gestEncomendas.getEncomendasEntreguesUtilizador(codUtilizador);
    }
    
    public void classificarTransportador(String codigoEncomenda, Integer classificacao) {
        String codigoTransportador = this.gestEncomendas.getEncomenda(codigoEncomenda).getCodigoTransportador();
        this.gestEntidades.classificarTransportador(codigoTransportador, classificacao);
    }
    
    public List<String> getCodEncomendasPendentesLoja(String codLoja) {
        return this.gestEncomendas.getCodEncomendasPendentesLoja(codLoja);
    }
    
    public List<Encomenda> getEncomendasPendentesLevantamento() {
        return this.gestEncomendas.getEncomendasPendentesLevantamento();
    }
    
    public List<String> getCodEncomendasPendentesLevantamento() {
        return this.gestEncomendas.getCodEncomendasPendentesLevantamento();
    }
    
    public void orcamentarTransporte(String codEncomenda, String codTransportadora, double precoTransporte) {
        this.gestEncomendas.orcamentarTransporte(codEncomenda, codTransportadora, precoTransporte);
    }
    
    public List<Encomenda> getEncomendasPendentesLevantamentoTransportadora(String codTransportadora) {
        return this.gestEncomendas.getEncomendasPendentesLevantamentoTransportadora(codTransportadora);
    }
    
    public List<String> getCodEncomendasPendentesLevantamentoTransportadora(String codTransportadora) {
        return this.gestEncomendas.getCodEncomendasPendentesLevantamentoTransportadora(codTransportadora);
    }
    
    public List<String> getCodEncomendasPendentesTransporte(String codigoTransportador) {
        return this.gestEncomendas.getCodEncomendasPendentesTransporte(codigoTransportador);
    }
    
    public void levantarEncomenda(String codEncomenda, String codTransportador) {
        this.gestEncomendas.levantarEncomenda(codEncomenda, codTransportador);
    }
    
    public void transportarEncomenda(String codEncomenda, String codTransportador) {
        this.gestEncomendas.transportarEncomenda(codEncomenda, codTransportador);
    }
    
    public List<Encomenda> getEncomendasUtilizador(String codUtilizador, Date dataIncico, Date dataFim) {
        return this.gestEncomendas.getEncomendasUtilizador(codUtilizador, dataIncico, dataFim);
    }
    
    public List<Encomenda> getEncomendasTransportador(String codTransportador, Date dataIncico, Date dataFim) {
        return this.gestEncomendas.getEncomendasTransportador(codTransportador, dataIncico, dataFim);
    }
    
    public List<Encomenda> getEncomendasLoja(String codLoja, Date dataIncico, Date dataFim) {
        return this.gestEncomendas.getEncomendasLoja(codLoja, dataIncico, dataFim);
    }
    
    public double totalFaturado(String codTransportadora, Date dataIncicio, Date dataFim) {
        return this.gestEncomendas.totalFaturado(codTransportadora, dataIncicio, dataFim);
    }
    
    public List<String>top10Utilizadores() {
        return this.gestEncomendas.top10Utilizadores();
    }
    
    public List<String>top10Transportadores() {
        return this.gestEncomendas.top10Transportadores();
    }
    
    public void registo() {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Tipo de entidade: Utilizador(U), Loja(L), Voluntario(V) ou Transportadora(T)");
        
        String tipo = scanner.nextLine();

        System.out.println("Codigo: ");
        scanner = new Scanner(System.in);
        String codigo = scanner.nextLine();

        System.out.println("Nome: ");
        scanner = new Scanner(System.in);
        String nome = scanner.nextLine();

        System.out.println("Coordendas: ");
        System.out.println("CoordX: ");
        scanner = new Scanner(System.in);
        double coordX  = scanner.nextDouble();

        System.out.println("CoordY: ");
        scanner = new Scanner(System.in);
        double coordY  = scanner.nextDouble();
        
        scanner.nextLine(); 

        System.out.println("Email: ");
        scanner = new Scanner(System.in);
        String email = scanner.nextLine();

        System.out.println("Password: ");
        scanner = new Scanner(System.in);
        String password = scanner.nextLine();

        double raio = 0;
        if(tipo.toLowerCase().equals("voluntario") || tipo.toLowerCase().equals("v") 
                                                   || tipo.toLowerCase().equals("transportadora") 
                                                   || tipo.toLowerCase().equals("t")) {
            System.out.println("Raio de acção: ");
            scanner = new Scanner(System.in);
            raio = scanner.nextDouble();
        }

        double precoKm = 0;
        double precoHora = 0;
        if(tipo.toLowerCase().equals("transportadora") || tipo.toLowerCase().equals("t")) {
            System.out.println("Preço por km: ");
            scanner = new Scanner(System.in);
            precoKm = scanner.nextDouble();

            System.out.println("Preço por hora: ");
            scanner = new Scanner(System.in);
            precoHora = scanner.nextDouble();
        }

        switch(tipo.toLowerCase()) {
            case "utilizador":
            case "u":
                Utilizador u = new Utilizador(codigo, nome, new GPS(coordX, coordY), email, password);
                addEntidade(u);
                
                saveData();
                break;
            case "loja":
            case "l":
                Loja l = new Loja(codigo, nome, new GPS(coordX, coordY), email, password);
                addEntidade(l);
                
                saveData();
                break;
            case "voluntario":
            case "v":
                Voluntario v = new Voluntario(codigo, nome, new GPS(coordX, coordY), email, password, raio);
                addEntidade(v);
                
                saveData();
                break;
            case "transportadora":
            case "t":
                Transportadora t = new Transportadora(codigo, nome, new GPS(coordX, coordY), email, password, raio, precoKm, precoHora);
                addEntidade(t);
                
                saveData();
                break;
        }
    }
    
    public void login() {

        System.out.println("Email: ");
        Scanner scanner = new Scanner(System.in);
        String email = scanner.nextLine();

        System.out.println("Password: ");
        scanner = new Scanner(System.in);
        String password = scanner.nextLine();

        Entidade entidade = getGestEntidades().login(email, password);

        if(entidade != null) {
            System.out.println("Sucesso");
            System.out.println(entidade.getClass());

            if(entidade.getClass() == Utilizador.class) {
                String[] opcoes = new String[] { "Solicitar a entrega",
                                                 "Aceitar", "Informação das entregas efectuadas", 
                                                 "Classificar", 
                                                 "Consultar encomendas"};
                Menu menu = new Menu(opcoes, "Menu: " + entidade.getClass() + " | " + entidade.getCodigo());
                do {
                    menu.executa();
                    int op = menu.getOpcao();
                    switch(op) {
                        case 1:
                            // Solicitar a entrega
                            solicitarEncomenda(entidade);
                            break;
                        case 2:
                            // Aceitar encomendas
                            aceitarEncomendas(entidade);
                            break;
                        case 3:
                            // Informação das entregas efectuadas
                            System.out.println(getEncomendasEntreguesUtilizador(entidade.getCodigo()).toString() + "\n\n");
                            break;
                        case 4:
                            // Classificar
                            classificarTransportadores(entidade);
                            break;
                        case 5:
                            // Consultar encomendas
                            consultarEncomendas(entidade);
                            break;
                    }
                } while (menu.getOpcao() != 0); 
            } else if(entidade.getClass() == Voluntario.class) {
                String[] opcoes = new String[] { "Sinalizar disponibilidade",
                                                 "Levantar encomenda do utilizador na loja",
                                                 "Transportar encomenda ao destino",
                                                 "Consultar encomendas"};
                Menu menu = new Menu(opcoes, "Menu: " + entidade.getClass() + " | " + entidade.getCodigo());
                do {
                    menu.executa();
                    int op = menu.getOpcao();
                    switch(op) {
                        case 1:
                            // Sinalizar disponibilidade
                            sinalizarDisponibilidade(entidade);
                            break;
                        case 2:
                            // Levantar encomenda do utilizador na loja
                            levantarEncomendaLojaVoluntario(entidade);
                            break;
                        case 3:
                            // Transportar encomenda ao destino
                            transportarEncomendaTransportador(entidade);
                            break;
                         case 4:
                            // Consultar encomendas
                            consultarEncomendas(entidade);
                            break;
                    }
                } while (menu.getOpcao() != 0); 
            } else if(entidade.getClass() == Transportadora.class) {
                String[] opcoes = new String[] { "Sinalizar disponibilidade", 
                                                 "Determinar o preço do transporte", 
                                                 "Transportar encomenda", 
                                                 "Consultar encomendas",
                                                 "Consultar total faturado"};
                Menu menu = new Menu(opcoes, "Menu: " + entidade.getClass() + " | " + entidade.getCodigo());
                do {
                    menu.executa();
                    int op = menu.getOpcao();
                    switch(op) {
                        case 1:
                            // Sinalizar disponibilidade
                            sinalizarDisponibilidade(entidade);
                            break;
                        case 2:
                            // Determinar o preço do transporte
                            determinarPrecoTransporte(entidade);
                            break;
                        case 3:
                            // Transportar encomenda ao destino e registar o preço e o tempo do transporte
                            transportarEncomendaTransportador(entidade);
                            break;
                        case 4:
                            // Consultar encomendas
                            consultarEncomendas(entidade);
                            break;
                        case 5:
                            // Consultar total faturado
                            totalFaturadoTransportadora(entidade);
                            break;
                    }
                } while (menu.getOpcao() != 0); 
            } else if(entidade.getClass() == Loja.class) {
                String[] opcoes = new String[] { "Sinalizar encomenda para entrega", 
                                                 "Mostrar quantidade de pessoas na fila", 
                                                 "Consultar encomendas"};
                Menu menu = new Menu(opcoes, "Menu: " + entidade.getClass() + " | " + entidade.getCodigo());
                do {
                    menu.executa();
                    int op = menu.getOpcao();
                    switch(op) {
                        case 1:
                            // Sinalizar encomenda
                            sinalizarEncomendaLoja(entidade);
                            break;
                        case 2:
                            // Mostar fila
                            System.out.println(getCodEncomendasPendentesLoja(entidade.getCodigo()));
                            break;
                         case 3:
                            // Consultar encomendas
                            consultarEncomendas(entidade);
                            break;
                    }
                } while (menu.getOpcao() != 0); 
            }
        } else {
            System.out.println("Não é possível efectuar login, contacte o administardor do sistema");
        }
    }
    
    public void totalFaturadoTransportadora(Entidade entidade) {
        Scanner scanner = new Scanner(System.in);
        
        List<Encomenda> encs = new ArrayList<>();
        
        System.out.println("Data de inicio no formato dd/MM/yyyy");
        String dataInicioStr = scanner.nextLine();
        
        System.out.println("Data de fim no formato dd/MM/yyyy");
        String dataFimStr = scanner.nextLine();
        
        SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
        Date dataInicio = new Date();
        Date dataFim = new Date();
        try {  
            dataInicio = format.parse(dataInicioStr);
            dataFim = format.parse(dataFimStr);
        } catch (ParseException e) {
            e.printStackTrace();
        }  
        double total = totalFaturado(entidade.getCodigo(), dataInicio, dataFim);
        
        System.out.println("Total faturado: " + total);
    }
                            
    public void saveData() {
        try {
            FileOutputStream f = new FileOutputStream(new File("data.txt"));
            ObjectOutputStream o = new ObjectOutputStream(f);
            o.writeObject(this);
            o.close();
            f.close();
            
        } catch (FileNotFoundException e) {
            System.out.println("Ficheiro nao encontrado");
        } catch (IOException e) {
            System.out.println("Erro ao incilializar stream");
        }   
    
    }
    
    public GestaoApp readData() {
        
        try {
            FileInputStream fi = new FileInputStream(new File("data.txt"));
            ObjectInputStream oi = new ObjectInputStream(fi);
            GestaoApp gestApp = (GestaoApp) oi.readObject();
            //System.out.println(gestApp.toString());
            oi.close();
            fi.close();
            return gestApp.clone();
            
        } catch (FileNotFoundException e) {
            System.out.println("Ficheiro nao encontrado");
        } catch (IOException e) {
            System.out.println("Erro ao incilializar stream");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        
        return new GestaoApp();
    }
    
    public boolean equals(Object o) {
        if(this == o) return true;
        if((this == null) || this.getClass() != o.getClass()) return false;
       
        GestaoApp ga = (GestaoApp) o;
        return this.gestEntidades.equals(ga.getGestEntidades()) && this.gestEncomendas.equals(ga.getGestEncomendas());
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.gestEntidades.toString()).append("\n");
        sb.append(this.gestEncomendas.toString());
        return sb.toString();
    }
    
    public GestaoApp clone() {
        return new GestaoApp(this);
    }
    
    /* 
     * 
     * metodos privados
     */
    private void solicitarEncomenda(Entidade entidade) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Codigo da encomenda: ");
        scanner = new Scanner(System.in);
        String codigoEncomenda = scanner.nextLine();

        System.out.println("Codigo loja: ");
        scanner = new Scanner(System.in);
        String codigoLoja = scanner.nextLine();

        System.out.println("Peso: ");
        scanner = new Scanner(System.in);
        double peso = scanner.nextDouble();
        scanner.nextLine(); 

        List<Produto> produtos = new ArrayList<>();

        String op = "s";
        do {
            System.out.println("Código do produto: ");
            String codigoProduto = scanner.nextLine();

            System.out.println("Nome do produto: ");
            String descrProduto = scanner.nextLine();

            System.out.println("Quantidade: ");
            double quantidade = scanner.nextDouble();

            System.out.println("Valor unitário: ");
            double valUnitario = scanner.nextDouble();
            
            scanner.nextLine(); 

            Produto p = new Produto(codigoProduto, descrProduto, quantidade, valUnitario);
            produtos.add(p);

            System.out.println("Adicionar outro produto (s/n) ?");
            op = scanner.nextLine();
        } while (op.equals("s"));

        EncomendaNormal enc = new EncomendaNormal(codigoEncomenda, entidade.getCodigo(), codigoLoja, peso, produtos, EstadoEncomenda.SOLICITADO);
        addEncomenda(enc);
        System.out.println(enc.toString() + "\n\n");
        saveData();
    }

    private void aceitarEncomendas(Entidade entidade) {
        Scanner scanner = new Scanner(System.in);
        
        List<String> codEncomendas = getCodEncomendasPendentesUtilizador(entidade.getCodigo());
        System.out.println(getEncomendasPendentesUtilizador(entidade.getCodigo()).toString() + "\n\n");
        if (codEncomendas.size() > 0) {
            System.out.println("Aceitar: (s/n) ?");
            String resp = scanner.nextLine();
            if(resp.toLowerCase().equals("s")) {
                setEstadoEncomendas(codEncomendas, EstadoEncomenda.ACEITE_UTILIZADOR);
                saveData();
            } 
        } else {
            System.out.println("De momento não tem encomendas para aceitar");
        }
    }

    private void classificarTransportadores(Entidade entidade) {
        Scanner scanner = new Scanner(System.in);
        
        List<Encomenda> encsEntregues = getEncomendasEntreguesUtilizador(entidade.getCodigo());
        if (encsEntregues.size() > 0) {
            System.out.println(encsEntregues.toString());
            System.out.println("Classifique o transportador: ");
            System.out.println("Codigo encomenda: ");
            String codigoEncomenda = scanner.nextLine();
            System.out.println("Classificação de 1 a 5: ");
            Integer classificacao = scanner.nextInt();
            classificarTransportador(codigoEncomenda, classificacao);
            //System.out.println(getClassificaveis().toString());
            saveData();
        } else {
            System.out.println("Não existem encomendas");
        }
    }
    
    private void consultarEncomendas(Entidade entidade) {
        Scanner scanner = new Scanner(System.in);
        
        List<Encomenda> encs = new ArrayList<>();
        
        System.out.println("Data de inicio no formato dd/MM/yyyy");
        String dataInicioStr = scanner.nextLine();
        
        System.out.println("Data de fim no formato dd/MM/yyyy");
        String dataFimStr = scanner.nextLine();
        
        SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
        Date dataInicio = new Date();
        Date dataFim = new Date();
        try {  
            dataInicio = format.parse(dataInicioStr);
            dataFim = format.parse(dataFimStr);
        } catch (ParseException e) {
            e.printStackTrace();
        }  
        
        if (entidade.getClass() == Utilizador.class) {
            encs = getEncomendasUtilizador(entidade.getCodigo(), dataInicio, dataFim);
        } else if (entidade.getClass() == Loja.class) {
            encs = getEncomendasTransportador(entidade.getCodigo(), dataInicio, dataFim);
        } else if (entidade.getClass() == Voluntario.class) {
            encs = getEncomendasTransportador(entidade.getCodigo(), dataInicio, dataFim);
        } else if (entidade.getClass() == Transportadora.class) {
            encs = getEncomendasLoja(entidade.getCodigo(), dataInicio, dataFim);
        }
        System.out.println(encs.toString());
    }
    
    private void sinalizarDisponibilidade(Entidade entidade) {
        ((Transportador)entidade).setDisponibilidade(true);
        System.out.println("Disponivel para transporte: " + ((Transportador)entidade).disponivelParaTransporte());
        saveData();
    }
    
    private void levantarEncomendaLojaVoluntario(Entidade entidade) {
        if(((Transportador)entidade).disponivelParaTransporte()) {
            Scanner scanner = new Scanner(System.in);
            List<String> codEncs;
            String codEnc;
            codEncs = getCodEncomendasPendentesLevantamento();
            if(codEncs.size() > 0) {
                System.out.println(codEncs.toString());
                System.out.println("Digite o codigo da encomenda que pretende levantar na loja ");
                codEnc = scanner.nextLine();
                
                if(getEncomenda(codEnc) != null) {
                    double distanciaLojaEncomenda = getEncomenda(codEnc).getDistancia();
                    Loja loja = (Loja)getEntidade(getEncomenda(codEnc).getCodigoLoja());
                    double distanciaAteLoja = entidade.calculaDistancia(loja.getGPS());
                                        
                    if(((Voluntario)entidade).getRaio() <= distanciaLojaEncomenda && 
                        ((Voluntario)entidade).getRaio() <= distanciaAteLoja) {
                        levantarEncomenda(codEnc, entidade.getCodigo());                  
                        saveData();  
                    } else {
                        System.out.println("Encomenda está fora do raio de acção!");
                    }
                } else {
                    System.out.println("Verifique se digitou o codigo correcto!");
                }
            } else {
                System.out.println("Não existem encomendas pendentes para levantamento");
            }
        } else {
            System.out.println("Neste momento não está disponível para transporte!");
        }
    }
    
    private void transportarEncomendaTransportador(Entidade entidade) {
        if(((Transportador)entidade).disponivelParaTransporte()) {
            Scanner scanner = new Scanner(System.in);
            List<String> codEncs;
            String codEnc;
            codEncs = getCodEncomendasPendentesTransporte(entidade.getCodigo());
            if(codEncs.size() > 0) {
                System.out.println(codEncs.toString());
                System.out.println("Digite o codigo da encomenda que pretende transportar: ");
                codEnc = scanner.nextLine();
                transportarEncomenda(codEnc, entidade.getCodigo());              
                saveData();
            } else {
                System.out.println("Não existem encomendas pendentes para transporte");
            }
        } else {
            System.out.println("Neste momento não está disponível para transporte!");
        }
    }
      
    private void determinarPrecoTransporte(Entidade entidade) {
        if(((Transportador)entidade).disponivelParaTransporte()) {
            if(getEncomendasPendentesLevantamento().size() > 0) {
                Scanner scanner = new Scanner(System.in);
                List<String> codEncs;
                String codEnc;
                
                System.out.println(getEncomendasPendentesLevantamento().toString());
                System.out.println("\n\n");
                codEncs = getCodEncomendasPendentesLevantamento();
                System.out.println(codEncs.toString());
                System.out.println("Digite o codigo da encomenda que pretende levantar na loja ");
                codEnc = scanner.nextLine();
                
                if(getEncomenda(codEnc) != null) {
                    double distanciaLojaEncomenda = getEncomenda(codEnc).getDistancia();
                    Loja loja = (Loja)getEntidade(getEncomenda(codEnc).getCodigoLoja());
                    double distanciaAteLoja = entidade.calculaDistancia(loja.getGPS());
                    double duracao = getEncomenda(codEnc).getDuracaoEntrega();
                
                    if(((Transportadora)entidade).getRaio() <= distanciaLojaEncomenda && 
                        ((Transportadora)entidade).getRaio() <= distanciaAteLoja) {
                        double precoTransporte = ((Transportador)entidade).calculaCustoTransporte(distanciaLojaEncomenda, duracao);
                        orcamentarTransporte(codEnc, entidade.getCodigo(), precoTransporte);                  
                        saveData();
                    } else {
                        System.out.println("Encomenda está fora do raio de acção!");
                    }
                } else {
                    System.out.println("Verifique se digitou o codigo correcto!");
                }
            } else {
                System.out.println("Não existem encomendas pendentes");
            }
        } else {
            System.out.println("Neste momento não está disponível para transporte!");
        }
    }
    
    private void sinalizarEncomendaLoja(Entidade entidade) {
        Scanner scanner = new Scanner(System.in);
        String codEnc;
        System.out.println(getCodEncomendasPendentesLoja(entidade.getCodigo()));
        if (getCodEncomendasPendentesLoja(entidade.getCodigo()).size() > 0) {
            System.out.println("Digite o codigo da encomenda para sinalizar como disponivel para transporte: ");
            codEnc = scanner.nextLine();
            if(getEncomenda(codEnc) != null) {
                 Entidade cliente = getEntidade(getEncomenda(codEnc).getCodigoUtilizador());
                 double distancia = entidade.calculaDistancia(cliente.getGPS());
                 setDistanciaEncomenda(codEnc, distancia);
                 setEstadoEncomenda(codEnc, EstadoEncomenda.DISPONIBILIZADO_LOJA);             
                 saveData();
            } else {
                System.out.println("Verifique se digitou o codigo correcto!");
            }
        } else {
           System.out.println("De momento não tem encomendas para disponibilizar");
        }
    }
}
