    import java.io.IOException;
    import java.nio.charset.StandardCharsets;
    import java.nio.file.Files;
    import java.nio.file.Paths;
    import java.time.LocalDate;
    import java.util.*;
    import java.util.stream.Collectors;

    public class Parse {
        Map<String,Product> products;
        Map<String,Order> orders;
        public trazAqui parse(){
            try{
                Map<String,Transporter> transporters = new HashMap<>();
                Map<String,Volunteer> volunteers = new HashMap<>();
                Map<String,Store> stores = new HashMap<>();
                orders = new HashMap<>();
                Map<String,orderReady> orderReady = new HashMap<>();
                products = new HashMap<>();
                Map<String,User> user = new HashMap<>();
                Transporter trazAquiTransporter = beforeParse();
                transporters.put(trazAquiTransporter.getCode(),trazAquiTransporter);
                List<String> linhas = lerFicheiro("logs.txt"); //alterar nome do ficheiro
                String[] linhaPartida;
                for (String linha : linhas) {
                    linhaPartida = linha.split(":", 2);
                    switch(linhaPartida[0]){
                        case "Utilizador":
                            User u = parseUtilizador(linhaPartida[1]);
                            user.put(u.getCod(),u);
                            break;
                        case "Loja":
                            Store l = parseLoja(linhaPartida[1]);
                            stores.put(l.getCod(),l);
                            break;
                        case "Voluntario":
                            Volunteer volunteer = parseVolunteer(linhaPartida[1]);
                            volunteers.put(volunteer.getCode(),volunteer);
                            break;
                        case "Transportadora":
                            Transporter transporter = parseTransporter(linhaPartida[1]);
                            transporters.put(transporter.getCode(),transporter);
                            break;
                        case "Aceite":
                            orderReady orderR = parseOrderReady(linhaPartida[1]);
                            orderReady.put(orderR.getCode(),orderR);
                            break;
                        case "Encomenda":
                            Order order = parseOrder(linhaPartida[1]);
                            orders.put(order.getCode(),order);
                            break;
                        default:
                            System.out.println("Linha invalida.");
                            break;
                    }

                }
                return new trazAqui(transporters,volunteers,stores,orders,orderReady,products,user,new ArrayList<>());
            }catch (Exception e){System.out.println("Ocorreu um erro a iniciar os logs.");}
                return new trazAqui();
            }

            public Transporter beforeParse(){
                List<String> string = lerFicheiro("empresa.txt");
                String[] linhaPartida = string.get(0).split(":");
                return parseTransporter(linhaPartida[1]);
            }

            public User parseUtilizador(String input){
                String[] campos = input.split(",");
                String nome = campos[1];
                String codUtilizador = campos[0];
                double gpsx = Double.parseDouble(campos[2]);
                double gpsy = Double.parseDouble(campos[3]);
                Coordinates gps = new Coordinates(gpsx,gpsy);
                return new User(codUtilizador,nome,gps);
            }
            public Store parseLoja(String input) {
            readProducts("products.txt");
            String[] campos = input.split(",");
            String codLoja = campos[0];
            String nomeLoja = campos[1];
            double gpsx = Double.parseDouble(campos[2]);
            double gpsy = Double.parseDouble(campos[3]);
            Coordinates gps = new Coordinates(gpsx, gpsy);
            return new Store(codLoja, nomeLoja, 1, 2, gps, new HashSet<>(products.values()));
        }
            public Volunteer parseVolunteer(String input){
                String[] campos = input.split(",");
                String codUtilizador = campos[0];
                String nome = campos[1];
                double gpsx = Double.parseDouble(campos[2]);
                double gpsy = Double.parseDouble(campos[3]);
                double geographicRadius = Double.parseDouble(campos[4]);
                Map<String,Order> order=new HashMap<>();
                Coordinates gps = new Coordinates(gpsx,gpsy);
                return new Volunteer(codUtilizador,nome,gps,geographicRadius,true,1,10,5,false,order);
            }
            public Transporter parseTransporter(String input){
            String[] campos = input.split(",");
            String codUtilizador = campos[0];
            String nome = campos[1];
            double gpsx = Double.parseDouble(campos[2]);
            double gpsy = Double.parseDouble(campos[3]);
            int nif = Integer.parseInt(campos[4]);
            double geographicRadius = Double.parseDouble(campos[5]);
            double price = Double.parseDouble(campos[6]);
            Coordinates gps = new Coordinates(gpsx,gpsy);
            return new Transporter(codUtilizador,nome,gps,geographicRadius,true,1,20,0,true,new HashMap<>(),price,1,nif);
        }
            public Product parseProduct(String input){
                String[] campos = input.split(",");
                String nome = campos[1];
                String cod = campos[0];
                int weight = Integer.parseInt(campos[2]);
                double price = Double.parseDouble(campos[3]);
                return new Product(cod,nome,weight,price);
            }
            public orderReady parseOrderReady(String input){
                String[] campos = input.split(",");
                String cod = campos[0];
                return new orderReady(cod);
            }
            public Order parseOrder(String input){
                randomOrderTime randomOrderTime = new randomOrderTime(0,15);
                String[] campos = input.split(",");
                String cod = campos[0];
                String codUser = campos[2];
                String codStore = campos[1];
                double weight = Double.parseDouble(campos[3]);
                int i=4; double price=0;
                Set<Product> prodLinha = new HashSet<>();
                while (i<campos.length){
                    String name = campos[i]; i++;
                    String description = campos[i]; i++;
                    double qt = Double.parseDouble(campos[i]); i++;
                    double priceperUni = Double.parseDouble(campos[i]);i++;
                    price +=qt*priceperUni;
                    Product p = new Product(name,description,qt,priceperUni);
                    products.putIfAbsent(p.getCode(),p.clone());
                    prodLinha.add(p);
                }
                return new Order(cod,codUser,codStore,"t50",LocalDate.now(),weight,1,12,price,randomOrderTime.getOrderProcessedTime(),0,false,prodLinha);
            }

            public List<String> lerFicheiro(String nomeFich) {
                List<String> lines = new ArrayList<>();
                try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
                catch(IOException exc) { System.out.println(exc.getMessage()); }
                return lines;
        }
            public void readProducts(String nomeFich) {
            List<String> lines = new ArrayList<>();
            try {
                lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
            } catch (IOException exc) {
                System.out.println(exc.getMessage());
            }
            String[] linhaPartida;
            for (String linha : lines) {
                linhaPartida = linha.split(":", 2);
                switch (linhaPartida[0]) {
                    case "Produto":
                        Product p = parseProduct(linhaPartida[1]);
                        products.put(p.getCode(),p);
                        break;
                }
            }
        }
    }

