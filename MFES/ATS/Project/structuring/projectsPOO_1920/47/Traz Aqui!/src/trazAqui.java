import org.json.simple.parser.ParseException;

import java.io.*;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

public class trazAqui implements Serializable{
    Map<String,Transporter> transporters;
    Map<String,Volunteer> volunteers;
    Map<String,Store> stores;
    Map<String,Order> orders;
    Map<String,orderReady> orderReady;
    Map<String,User> users;
    Map<String,Product> products;
    List<String> orderDeliveryComplete;

    //Métodos de Construção da Classe por omissão, passagem de argumentos e passagem de Objeto.
    public trazAqui(){
        transporters = new HashMap<>();
        volunteers   = new HashMap<>();
        stores       = new HashMap<>();
        orders       = new HashMap<>();
        orderReady   = new HashMap<>();
        products     = new HashMap<>();
        users         = new HashMap<>();
        orderDeliveryComplete = new ArrayList<>();
    }

    public trazAqui(Map<String, Transporter> transporters, Map<String, Volunteer> volunteers, Map<String, Store> stores, Map<String, Order> orders,
                    Map<String, orderReady> orderReady, Map<String, Product> products, Map<String, User> users,List<String> orderDeliveryComplete) {
        setOrders(orders);
        setOrderReady(orderReady);
        setStores(stores);
        setTransporters(transporters);
        setVolunteers(volunteers);
        setUsers(users);
        setProducts(products);
        setOrderDeliveryComplete(orderDeliveryComplete);

    }

    public trazAqui(trazAqui tAq){
        setStores(tAq.getStores());
        setVolunteers(tAq.getVolunteers());
        setTransporters(tAq.getTransporters());
        setOrders(tAq.getOrders());
        setProducts(tAq.getProducts());
        setUsers(tAq.getUsers());
        setOrderReady(tAq.getOrderReady());
        setOrderDeliveryComplete(tAq.getOrderDeliveryComplete());
    }

    //Getters & Setters
    public Map<String, Transporter> getTransporters() {
        Map<String, Transporter> newTransporters = new HashMap<>();
        transporters.forEach((key,value)->newTransporters.put(key,value.clone()));
        return newTransporters;
    }
    public void setTransporters(Map<String, Transporter> oldTransporters) {
        transporters=new HashMap<>();
        oldTransporters.forEach((key,value)->transporters.put(key,value.clone()));
    }
    public Map<String, Volunteer> getVolunteers() {
        Map<String, Volunteer> newVolunteers = new HashMap<>();
        volunteers.forEach((key,value)->newVolunteers.put(key,value.clone()));
        return newVolunteers;
    }
    public void setVolunteers(Map<String, Volunteer> oldVolunteers) {
        volunteers = new HashMap<>();
        oldVolunteers.forEach((key,value)->volunteers.put(key,value.clone()));
    }
    public Map<String, Store> getStores() {
        Map<String, Store> newStores = new HashMap<>();
        stores.forEach((key,value)->newStores.put(key,value.clone()));
        return newStores;
    }
    public void setStores(Map<String, Store> oldStores) {
        stores=new HashMap<>();
        oldStores.forEach((key,value)->stores.put(key,value.clone()));
    }
    public Map<String,Order> getOrders() {
        Map<String,Order> newOrders = new HashMap<>();
        orders.forEach((key,value)->newOrders.put(key,value.clone()));
        return newOrders;
    }
    public void setOrders(Map<String,Order> oldOrders) {
        orders = new HashMap<>();
        oldOrders.forEach((key,value)->orders.put(key,value.clone()));
    }
    public Map<String, orderReady> getOrderReady() {
        Map<String,orderReady> newOrders = new HashMap<>();
        orderReady.forEach((key,value)->newOrders.put(key,value.clone()));
        return newOrders;
    }
    public void setOrderReady(Map<String, orderReady> newOrderReady) {
        this.orderReady = new HashMap<>();
        newOrderReady.forEach((key,value)->orderReady.put(key,value.clone()));
    }
    public Map<String, User> getUsers() {
        Map<String,User> newUsers = new HashMap<>();
        users.forEach((key,value)->newUsers.put(key,value.clone()));
        return newUsers;
    }
    public void setUsers(Map<String, User> newUsers) {
        users=new HashMap<>();
        newUsers.forEach((key,value)->users.put(key,value.clone()));
    }
    public Map<String, Product> getProducts() {
        Map<String,Product> newProducts = new HashMap<>();
        products.forEach((key,value)->newProducts.put(key,value.clone()));
        return newProducts;
    }
    public void setProducts(Map<String, Product> oldProducts) {
        products = new HashMap<>();
        oldProducts.forEach((key,value)->products.put(key,value.clone()));
    }
    public List<String> getOrderDeliveryComplete() {
        return new ArrayList<>(orderDeliveryComplete);
    }
    public void setOrderDeliveryComplete(List<String> orderDeliveryComplete) {
        this.orderDeliveryComplete = new ArrayList<>(orderDeliveryComplete);
    }

    public void addTransporter(Transporter transporter){
        transporters.remove(transporter.getCode());
        transporters.put(transporter.getCode(),transporter.clone());
    }
    public void addVolunteer(Volunteer volunteer){
        volunteers.remove(volunteer.getCode());
        volunteers.put(volunteer.getCode(),volunteer.clone());
    }


    //Metodo Clone
    protected trazAqui clone(){return new trazAqui(this);}

    /*Metodos para o registo de Clientes do programa, sendo eles Loja, Voluntário, Transportadoras e Utilizadores.
    Cada método recebe como argumento um array de Strings que contem a informação necessária. */
    public String createStore(String[] input) throws ContaExistente {
        Store newStore;
        if(stores.values().stream().noneMatch(e->input[0].equals(e.getName()))){
            String code = randomCodeGenerator(1);
            Set<Product> product = createProductforStore(input,5);
            Coordinates Gps = new Coordinates(Double.parseDouble(input[3]),Double.parseDouble(input[4]));
            newStore = new Store(code,input[0],Integer.parseInt(input[1]),Integer.parseInt(input[2]),Gps,product);
            stores.put(code,newStore);
            return code;
        }else throw new ContaExistente("Loja já existe. Efetue Login");
    }

    public String createTransporter(String[] input) throws ContaExistente {
        Transporter transporter;
        if(transporters.values().stream().noneMatch(e->input[0].equals(e.getName()))){
            String code = randomCodeGenerator(2);
            Coordinates Gps = new Coordinates(Double.parseDouble(input[1]),Double.parseDouble(input[2]));
            Map<String,Order> deliveryRegist = new HashMap<>();
            transporter=new Transporter(code,input[0],Gps,Double.parseDouble(input[3]),Boolean.parseBoolean(input[4]),
                    Integer.parseInt(input[5]),Double.parseDouble(input[6]),0,Boolean.parseBoolean(input[7]),deliveryRegist,Double.parseDouble(input[8]),
                    Integer.parseInt(input[9]),Integer.parseInt(input[10]));
            transporters.put(code,transporter);
            return code;
        }else throw new ContaExistente("Transportador já existe. Efetue Login");
    }

    public String createVolunteer(String[] input) throws ContaExistente {
        Volunteer newVol;
        if(volunteers.values().stream().noneMatch(e->input[0].equals(e.getName() ))){
            String code = randomCodeGenerator(3);
            Coordinates Gps = new Coordinates(Double.parseDouble(input[1]),Double.parseDouble(input[2]));
            Map<String,Order> deliveryRegist = new HashMap<>();
            newVol=new Volunteer(code,input[0],Gps,Double.parseDouble(input[3]),Boolean.parseBoolean(input[4]),
                    Integer.parseInt(input[5]),Double.parseDouble(input[6]),0,Boolean.parseBoolean(input[7]),deliveryRegist);
            volunteers.put(code,newVol);
            return code;
        }else throw new ContaExistente("Voluntário já existe. Efetue Login");
    }

    public String createUser(String[] input) throws ContaExistente {
        if(users.values().stream().noneMatch(e->e.getName().equals(input[0]))){
            String code = randomCodeGenerator(4);
            User newUser = new User(code,input[0],new Coordinates(Double.parseDouble(input[1]),Double.parseDouble(input[2])));
            users.put(code,newUser);
            return code;
        }
        else throw new ContaExistente("Utilizador já existe. Efetue Login");
    }

    public Set<Product> createProductforStore(String[] input,int start){
        Set<Product> product = new HashSet<>();
        try{
        int i = start;
        while (i<input.length){
            String[] campos = input[i].split("-");
            String code="";
                code = randomCodeGenerator(5);
                Product newProduct = new Product (code,campos[0],Double.parseDouble(campos[1]),Double.parseDouble(campos[2]));
                product.add(newProduct);
                products.put(code,newProduct);
                i++;
        }}
       catch (Exception e){System.out.println("Erro a criar produto.");}
            return product;
    }

    public String createEncomenda(String codeUser, String codeStore, boolean str, String codeVolunteerTransporter, List<String> codeProducts) throws ErroACriar {
        randomOrderTime randomizer = new randomOrderTime(stores.get(codeStore).getQueue(),stores.get(codeStore).getAvgServiceTime());
       if(codeProducts.size()>0){
        String codeEncomenda = randomCodeGenerator(6);
        double precoDeTransporte=0;
        if(str) precoDeTransporte = getPriceT(codeVolunteerTransporter,codeStore,codeProducts);
        Set<Product> produtos = new HashSet<>(); for(String code : codeProducts){
            produtos.add(products.get(code));
        }
        double weight = produtos.stream().mapToDouble(Product::getWeight).sum();
        double precoEnc = produtos.stream().mapToDouble(Product::getPrice).sum();
        Order newOrder = new Order(codeEncomenda,codeStore,codeUser,codeVolunteerTransporter,LocalDate.now(),weight,3,precoDeTransporte,precoEnc,randomizer.getOrderProcessedTime(),0,false,produtos);
        orders.put(codeEncomenda,newOrder);
        if(str) transporters.get(codeVolunteerTransporter).addOrder(newOrder);
        else volunteers.get(codeVolunteerTransporter).addOrder(newOrder);
        return codeEncomenda;}
        else throw new ErroACriar("Encomenda não registada.");
    }

    public void createOrderReady(String code){
        orderReady.put(code,new orderReady(code));
    }

    public LocalDate createDeliveryExpectedDate(String codeVolunteerTransporter,boolean str){
        LocalDate Data = LocalDate.now();
        LocalDate novaData = LocalDate.now();
        int size=0;
        for(String order : orderReady.keySet()){
            size+= (int) getOrders().values().stream().filter(e->e.getDeliveryCode().equals(codeVolunteerTransporter) && e.getCode().equals(order)).count();
        }
            if (size <= 5) novaData = Data.plusDays(3);
            if (size > 5 && size <=10) novaData = Data.plusDays(5);
            if (size > 10) novaData = Data.plusDays(10);
        return novaData;
    }


    //Verificar credenciais de login
    /*Como argumento recebe o tipo de conta, bem como uma string do formaro Nome#Codigo, que posteriormente é divida e passada por argumento
    * na chamada à validateLogin existente em cada classe Store,User,Volunteer*/
    public String validaLogin(int type,String nome_code) throws ContaNãoExistente {
        String result = ""; String[] splitter = nome_code.split("#");
        String nome = splitter[0]; String code = splitter[1];
        switch (type){
            case 1: if(stores.containsKey(code)){
                Store store = stores.get(code).clone();
                result = store.validateLogin(nome,code);
            }else throw new ContaNãoExistente("Conta pretendida não existe.");
            break;
            case 2: if(transporters.containsKey(code)){
                Transporter transporter = transporters.get(code).clone();
                result = transporter.validateLogin(nome,code);
            }else throw new ContaNãoExistente("Conta pretendida não existe."); break;
            case 3: if(volunteers.containsKey(code)){
                Volunteer vol = volunteers.get(code).clone();
                result = vol.validateLogin(nome,code);
                System.out.println(result);
            }else throw new ContaNãoExistente("Conta pretendida não existe."); break;
            case 4: if(users.containsKey(code)){
                User user = users.get(code).clone();
                result = user.validateLogin(nome,code);
            }else throw new ContaNãoExistente("Conta pretendida não existe.");

                break;
        }
        return result;
    }


    //Métodos para gerar um código. Maximo é 3000, valor arbitrario. Limita o numero de contas de cada tipo a 3000 bem como encomendas.
    public String randomCodeGenerator(int type){
        String code = "";
        Random randomizer = new Random();
        int no4Code = randomizer.nextInt(Integer.MAX_VALUE);
        switch (type){
            case 1: code = "RL" + no4Code; break;
            case 2: code = "RT" + no4Code; break;
            case 3: code = "RV" + no4Code; break;
            case 4: code = "RC" + no4Code; break;
            case 5: code = "RP" + no4Code; break;
            case 6: code = "RO" + no4Code; break;
        }
        while (codeInUse(type,code)){
           no4Code = randomizer.nextInt(Integer.MAX_VALUE);
           switch (type){
               case 1: code = "RL" + no4Code; break;
               case 2: code = "RT" + no4Code; break;
               case 3: code = "RV" + no4Code; break;
               case 4: code = "RC" + no4Code; break;
               case 5: code = "RP" + no4Code; break;
               case 6: code = "RO" + no4Code; break;
           }
       }
        return code;
    }

    //Este metodo recebe o tipo de codigo e o novo codigo, verificando se o mesmo ja esta em uso.
    public boolean codeInUse(int type, String code){
        boolean inUse=false;
        switch (type){
            case 1: inUse=stores.containsKey(code); break;
            case 2: inUse=transporters.containsKey(code); break;
            case 3: inUse=volunteers.containsKey(code); break;
            case 4: inUse=users.containsKey(code); break;
            case 5: inUse=products.containsKey(code); break;
            case 6: inUse=orders.containsKey(code); break;
        }
        return inUse;
    }



    //Encontrar Transportadora mais perto de uma loja. Excluindo transportadoras não desejadas.
    public String getCloserTransporterToStore(String codeStore,ArrayList<String> transporterCodes) throws ContaNãoExistente {
        double distance = Double.MAX_VALUE; String code = "";
        for(Transporter t : transporters.values()){
            if(calculateDistance(t.getCoordinates(),stores.get(codeStore).getCoordinates())<distance &&
            calculateDistance(t.getCoordinates(),stores.get(codeStore).getCoordinates())<t.getGeographicRadius()){
                if(!transporterCodes.contains(t.getCode()) && t.getAvailable()) code = t.getCode();
            }
        }
        if(code.isEmpty()) throw new ContaNãoExistente("Não é possivel encontrar transportadora. Será encaminhado para um voluntário.");
        return code;
    }

    //Encontrar Voluntario mais perto de uma loja.
    public String getCloserVolunteerToStore(String codeStore) throws ContaNãoExistente {
        double distance = Double.MAX_VALUE; String code = "";
        for(Volunteer V : volunteers.values()){
           if(V.getAvailable()){
            if(calculateDistance(V.getCoordinates(),stores.get(codeStore).getCoordinates())<distance &&
                    calculateDistance(V.getCoordinates(),stores.get(codeStore).getCoordinates())<V.getGeographicRadius()){
                code = V.getCode();
            }
        }
        }
        if(code.isEmpty()) throw new ContaNãoExistente("Não é possivel encontrar Voluntário.");
        return code;
    }

    //Encontrar entregas anteriores. 1 -> loja, 2->transportadora, 3->voluntario, 4->client
    public Set<Order> getPastOrders(String code, int type){
        Set<Order> pastOrder = new TreeSet<>(new Comparator<Order>() {
            @Override
            public int compare(Order o1, Order o2) {
                return o1.getDate().compareTo(o2.getDate());
            }
        });
        switch (type){
            case 1: pastOrder = getOrders().values().stream().filter(e->e.getSellerCode().equals(code)).collect(Collectors.toSet()); break;
            case 2: pastOrder = new HashSet<>(transporters.get(code).getDeliveryRegist().values()); break;
            case 3: pastOrder = new HashSet<>(volunteers.get(code).getDeliveryRegist().values()); break;
            case 4: pastOrder = getOrders().values().stream().filter(e->e.getBuyerCode().equals(code)).collect(Collectors.toSet()); break;
        }
        return pastOrder;
    }

    //Metodo que determina a distancia entre uma transportadora/Voluntario e loja desejada.
    public double calculateDistance(Coordinates trans,Coordinates store){
        double x = Math.pow(store.getLatitude()-trans.getLatitude(),2);
        double y = Math.pow(store.getLongitude()-trans.getLongitude(),2);
        return Math.sqrt(x+y);
    }

    //Metodo que calcula o preço de entrega de uma encomenda consoante o preço e a distancia.
    public double getPriceT(String trans, String store, List<String> product){
        double weight = product.stream().mapToDouble(e->products.get(e).getWeight()).count();
        double distance = calculateDistance(transporters.get(trans).getCoordinates(),stores.get(store).getCoordinates());
        return (weight/1000 + distance)*transporters.get(trans).getPrice();
    }

    //Indicar o total facturado por uma empresa transportadora num determinado período;
    public double totalFaturadoEmpresa (String code, LocalDate after,LocalDate before){
            Map<String,Order> regist = transporters.get(code).clone().getDeliveryRegist();
            List<Order> orders = regist.values().stream()
                    .filter(e->e.getDate().isAfter(after) && e.getDate().isBefore(before))
                    .collect(Collectors.toList());
            return orders.stream().mapToDouble(Order::getOrderPrice).count();
    }

    //determinar a listagens das 10 empresas transportadoras que mais utilizam o sistema;
    public Set<Transporter> listaEmpresasMaisUsam(){
        Set<Transporter> Empresas = new TreeSet<>(new Comparator<Transporter>() {
            public int compare(Transporter o1, Transporter o2) {
                if(o1.getKmPercorridos()>o2.getKmPercorridos()) return 1;
                if(o1.getKmPercorridos()<o2.getKmPercorridos()) return -1;
                return o1.compareTo(o2);
            }
        });
        if(transporters!=null){
            transporters.values().stream().limit(10).forEach(e->Empresas.add(e.clone()));
        }
        return Empresas;
    }

    //determinar a listagens dos 10 utilizadores que mais utilizam o sistema;
    public Set<User> listaUtilizadoresMaisUsam(){
        Set<User> user = new TreeSet<>(new Comparator<User>() {
            public int compare(User o1, User o2) {
                if(getPastOrders(o1.getCod(),4).size()>getPastOrders(o2.getCod(),4).size()) return 1;
                if(getPastOrders(o1.getCod(),4).size()<getPastOrders(o2.getCod(),4).size()) return -1;
                return o1.compareTo(o2);
            }
        });
        if(users!=null){
            users.values().stream().limit(10).forEach(e->user.add(e.clone()));
        }
        return user;
    }

    //Metodos Funcionalidades Loja.
    public void setQueueStore(String code,int queue){ stores.get(code).setQueue(queue); }
    public void setAvgServiceTimeStore(String code,int avg){ stores.get(code).setAvgServiceTime(avg);}
    public void addProductToStore(String code,Set<Product> products){
        try {
            stores.get(code).addProducts(products);
        }catch (Exception e){ e.getMessage();}
    }
    public void removeProductFromStore(String code,List<String> codigoProdutos) throws IOException, ParseException {
        try {
            stores.get(code).removeProducts(codigoProdutos);
        }catch (Exception e){ e.getMessage();}
    }

    public Set<Order> getOrdersToSetFromStore(String code){
        Set<Order> ordersFromStores = new HashSet<>();
        ordersFromStores=getOrders().values().stream().filter(e->e.getSellerCode().equals(code)).collect(Collectors.toSet());
        for(String codigo : orderReady.keySet()){
            ordersFromStores.removeIf(e->e.getCode().equals(codigo));
        }
        return ordersFromStores;
    }

    //Metodos Funcionalidades Voluntário/Empresa
    public boolean alterarDisponibilidade(String code,int type) throws ContaNãoExistente {
        boolean choosen;
        if(type==1){
            Transporter trans = getTransporters().get(code).clone();
            if(trans!=null) {
                if (getTransporters().get(code).getAvailable()) {
                    trans.setAvailable(false);
                    choosen = false;
                } else {
                    trans.setAvailable(true);
                    choosen = true;
                }
                addTransporter(trans);
            }else throw new ContaNãoExistente("Conta desejada não existe.");
        }
        else {
            Volunteer volunteer = volunteers.get(code).clone();
            if(volunteer!=null){
                if(volunteer.getAvailable()){
                     volunteer.setAvailable(false);
                    choosen=false;}
                else{
                    volunteer.setAvailable(true);
                    choosen=true;}
                System.out.println(volunteer.toString());
                addVolunteer(volunteer);
            }else throw new ContaNãoExistente("Conta desejada não existe.");
        }
        return choosen;
    }

    public void atualizarEmpregados(String code,int num){
        Transporter transporter = transporters.get(code).clone();
        transporter.setDeliveryCapacity(num);
        addTransporter(transporter);
    }

    public Set<Order> getOrdersToDelivery(String code){
        Set<Order> ordersToDelivery = new HashSet<>();
        for(String ordeReadyCode : orderReady.keySet()) {
            for (Order order : orders.values()) {
                if (order.getCode().equals(ordeReadyCode) && order.getDeliveryCode().equals(code))
                    ordersToDelivery.add(orders.get(ordeReadyCode).clone());
            }
        }
        return ordersToDelivery;
    }

    public void setDeliveryTimeAndFinished(String codeOrder,double tempo,String code,int type){
        if(type==1){
            Order newOrder = orders.get(codeOrder).clone();
            double tempoPLUSdelivery = newOrder.getOrderDeliveryTime() + tempo;
            newOrder.setOrderDeliveryTime(tempoPLUSdelivery);
            orders.remove(codeOrder); orders.put(codeOrder,newOrder.clone());
            transporters.get(code).replaceOrder(newOrder.clone());
            orderDeliveryComplete.add(codeOrder); orderReady.remove(codeOrder);
        }
        else {
            Order newOrder = orders.get(codeOrder).clone();
            double tempoPLUSdelivery = newOrder.getOrderDeliveryTime() + tempo;
            newOrder.setOrderDeliveryTime(tempoPLUSdelivery);
            orders.remove(codeOrder); orders.put(codeOrder,newOrder.clone());
            volunteers.get(code).replaceOrder(newOrder.clone());
            orderDeliveryComplete.add(codeOrder); orderReady.remove(codeOrder);
        }
    }

    //Metodos para um cliente classificar transportadora.
    public Set<Order> ordersToClassificate(String codeUser){
        Set<Order> ordersToClassificate =new HashSet<>();
        for(String orderDeliveryCode :orderDeliveryComplete){
            for (Order order : orders.values()) {
                if (order.getCode().equals(orderDeliveryCode) && order.getBuyerCode().equals(codeUser) && !order.isClassificated())
                    ordersToClassificate.add(orders.get(orderDeliveryCode).clone());
            }
        }
        return ordersToClassificate;
    }

    public void classificateOrder(String codeOrder,double classification){
        Order orderToClassificate = orders.get(codeOrder).clone();
        Transporter transporterToClassificate = transporters.get(orderToClassificate.getDeliveryCode());
        Volunteer volunteerToClassificate = volunteers.get(orderToClassificate.getDeliveryCode());
        orderToClassificate.setDeliveryClassification(classification); orderToClassificate.setClassificated(true);
        orders.replace(codeOrder,orderToClassificate.clone());
        if(volunteerToClassificate==null){
            transporterToClassificate.replaceOrder(orderToClassificate);
            transporterToClassificate.atualizarClass();
        }
        else {
            volunteerToClassificate.replaceOrder(orderToClassificate);
            volunteerToClassificate.atualizarClass();
        }
    }

    @Override
    public String toString() {
        return "trazAqui{" +
                "transporters=" + transporters +
                ", volunteers=" + volunteers +
                ", stores=" + stores +
                ", orders=" + orders +
                ", orderReady=" + orderReady +
                ", users=" + users +
                ", products=" + products +
                ", orderDeliveryComplete=" + orderDeliveryComplete +
                '}';
    }
}
