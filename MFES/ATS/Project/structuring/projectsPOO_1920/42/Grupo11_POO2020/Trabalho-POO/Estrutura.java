import java.io.*;
import java.time.LocalDate;
import java.util.*;

public class Estrutura
{
    private DadosUtilizadores utilizadores;
    private DadosVoluntarios voluntarios;
    private DadosEmpresas empresas;
    private DadosLojas lojas;
    private DadosEncomendas encomendas;
    private String idLogged;


    public Estrutura(){
        this.utilizadores = new DadosUtilizadores();
        this.empresas = new DadosEmpresas();
        this.voluntarios = new DadosVoluntarios();
        this.lojas = new DadosLojas();
        this.encomendas = new DadosEncomendas();
        this.idLogged = "";
    }


    public Estrutura(DadosUtilizadores u, DadosVoluntarios v, DadosEmpresas e, DadosLojas l, DadosEncomendas enc, String log){
        this.utilizadores = u.clone();
        this.voluntarios = v.clone();
        this.empresas = e.clone();
        this.lojas = l.clone();
        this.encomendas = enc.clone();
        this.idLogged = log;
    }


    public Estrutura(Estrutura e){
        this.utilizadores = e.getUtilizadores();
        this.empresas = e.getEmpresas();
        this.lojas = e.getLojas();
        this.voluntarios = e.getVoluntarios();
        this.encomendas = e.getEncomendas();
        this.idLogged = e.getIdLogged();
    }

    public DadosUtilizadores getUtilizadores(){
        return this.utilizadores.clone();
    }

    public DadosEmpresas getEmpresas(){
        return this.empresas.clone();
    }

    public DadosLojas getLojas(){
        return this.lojas.clone();
    }

    public DadosVoluntarios getVoluntarios(){
        return this.voluntarios.clone();
    }

    public DadosEncomendas getEncomendas(){
        return this.encomendas.clone();
    }

    public String getIdLogged(){return this.idLogged;}

    public void setIdLogged(String id){ this.idLogged = id;}



    //o estado da aplicação deverá estar pré-carregado com um conjunto de dados significativos,
    //que permita testar toda a aplicação no dia da entrega.
    public void carregaEstado(String nome) throws FileNotFoundException, IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream(nome);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Estrutura e = (Estrutura) ois.readObject();
        ois.close();
        this.empresas = e.getEmpresas();
        this.utilizadores = e.getUtilizadores();
        this.voluntarios = e.getVoluntarios();
        this.lojas = e.getLojas();
        this.encomendas = e.getEncomendas();
    }



    public void carregaLogs(String fich) throws FileNotFoundException, IOException{
        List<String> dadosS =lerLogs(fich);
        dadosS.forEach(s -> {
            try {
                this.adicionaLinha(s);
            } catch (InvalidValueException e) {
                e.printStackTrace();
            } catch (IdNotFoundException e) {
                e.printStackTrace();
            }
        });
    }

    private static List<String> lerLogs(String fich) throws FileNotFoundException, IOException{
        List<String> linhas = new ArrayList<>();
        BufferedReader br = new BufferedReader(new FileReader(fich));
        String linha;
        int i = 0;
        while((linha = br.readLine()) != null && (linha.equals("Dados de LOGS:")) == false);
        while((linha = br.readLine()) !=null && i<2) i++;
        while((linha = br.readLine()) != null && (linha.equals("\n")) == false){
            linhas.add(linha);
        }
        br.close();
        return linhas;
    }

    private void adicionaLinha(String csv) throws InvalidValueException, IdNotFoundException {
        String[] divisor;
        String tipo, aux, nomeU, idU, nomeV, idV, nomeT, idT, nomeL, idL;
        float latU, longiU, latV, longiV, latT, longiT, latL, longiL;
        int nif;
        double cx, cy, velocidade, preco, consumo, autonomia;

        divisor = csv.split(":");
        tipo = divisor[0];
        aux = divisor[1];

        switch(tipo){
            case "Utilizador": divisor = aux.split(",");
                idU = divisor[0];
                nomeU = divisor[1];
                latU = Float.parseFloat(divisor[2]);
                longiU = Float.parseFloat(divisor[3]);
                Utilizador u = new Utilizador(idU, nomeU, latU, longiU);
                utilizadores.addUtilizador(u);
                break;

            case "Voluntario": divisor = aux.split(",");
                idV = divisor[0];
                nomeV = divisor[1];
                latV = Float.parseFloat(divisor[2]);
                longiV = Float.parseFloat(divisor[3]);
                Voluntario v = new Voluntario(idV, nomeV, latV, longiV);
                voluntarios.addVoluntario(v);
                break;

            case "Transportadora" : divisor = aux.split(",");
                idT = divisor[0];
                nomeT = divisor[1];
                latT = Float.parseFloat(divisor[2]);
                longiT = Float.parseFloat(divisor[3]);
                Empresa e = new Empresa(idT, nomeT, latT, longiT);
                empresas.addEmpresa(e);
                break;


            case "Loja" : divisor = aux.split(",");
                idL = divisor[0];
                nomeL = divisor[1];
                latL = Float.parseFloat(divisor[2]);
                longiL = Float.parseFloat(divisor[3]);
                Loja l = new Loja(idL, nomeL, latL, longiL);
                lojas.addLoja(l);

                break;

            case "Encomenda" : divisor = aux.split(",");
                int s = divisor.length;
                String idE, idUti, idLoja;
                float peso;
                String idLinha, descricao;
                float quantidade, pre;
                idE = divisor[0];
                idUti= divisor[1];
                idLoja = divisor[2];
                peso = Float.parseFloat(divisor[3]);
                int i = 4;
                Encomenda enc = new Encomenda(idE, idUti, idLoja, peso);
                while(i<s){
                    idLinha = divisor[i];
                    descricao = divisor[i+1];
                    quantidade = Float.parseFloat(divisor[i+2]);
                    preco = Float.parseFloat(divisor[i+3]);
                    LinhaEncomenda le = new LinhaEncomenda(idLinha, descricao, quantidade, preco);
                    enc.addLinhaEncomenda(le);
                    i+=4;
                }
                inserePedido(enc);

                break;

            case "Aceite":
                Encomenda encom = encomendas.getEncomenda(aux);
                encom.setAce(true);

        }
    }

    //método que faz inserir pedidos de encomendas a uma loja, por parte de um utilizador;
    //menu pedir os dados todos
    public void inserePedido(Encomenda encomenda) throws IdNotFoundException, InvalidValueException {
        try{
            Utilizador u = this.utilizadores.getUtilizador(encomenda.getIdComprador());
            u.getEncomendas().add(encomenda);
        } catch(IdNotFoundException ex){
            System.out.println(ex.getMessage());
        }
        try{
            Loja l = this.lojas.getLoja(encomenda.getIdLoja());
            l.getEnc().put(encomenda.getIdComprador(),encomenda);
            encomendas.addEncomenda(encomenda);

        } catch(IdNotFoundException | InvalidValueException ex){
            System.out.println(ex.getMessage());
        }

    }

    // método que insere informação de encomenda pronta a ser entregue, por parte das lojas
    //menu das lojas
    public void encomendaPronta(String e) throws IdNotFoundException {
        try {
            encomendas.getEncomenda(e).setEncPronta(true);
        } catch (IdNotFoundException ex){
            System.out.println(ex.getMessage());
        }
    }


    // indicar que se pretende transportar a encomenda. No caso de ser uma empresa transportadora
    //o utilizador que solicitou o serviço terá de aprovar que aceita o preço. Tanto voluntários como
    //empresas apenas podem fazer transporte de encomendas de acordo com o raio de acção que
    //tiverem definido;


public void aceitaTransporte(String idTrans, String idEnc) throws IdNotFoundException {

        Encomenda aux = encomendas.getEncomenda(idEnc);
        aux.setIdTransportador(idTrans);
        aux.setAce(true);
    if(!empresas.getEmpresas().containsKey(idTrans)){
        aux.setAceComprador(true);
        aux.setData(LocalDate.now());
    }



}

public double distancia(float x1, float y1, float x2, float y2, float x3, float y3){

        return Math.sqrt(Math.pow(x1+x2,2)+Math.pow(y1+y2,2)) + Math.sqrt(Math.pow(x2+x3,2)+Math.pow(y2+y3,2));
}

public double custoTransporte (Empresa e, Loja l, Utilizador u){

        return e.getCusto() + e.getTaxa() * distancia(e.getLongitude(),e.getLatitude(),l.getLong(),l.getLat(), u.getLatitude(),u.getLongitude());

}


public void aceitaComp(String idEnc) throws IdNotFoundException, InvalidValueException {
    Scanner scan = new Scanner(System.in);
    Encomenda aux = encomendas.getEncomenda(idEnc);
    String idTrans = aux.getIdTransportador();

    if(empresas.getEmpresas().containsKey(idTrans)){
        Empresa e = this.empresas.getEmpresa(idTrans);
        Loja auxLoja = this.lojas.getLoja(aux.getIdLoja());
        Utilizador u = this.utilizadores.getUtilizador(aux.getIdComprador());
        System.out.println("Custo:" + custoTransporte(e,auxLoja,u));
        System.out.println("Aceitar encomenda? (0-não, 1-sim):");
        int aceita = scan.nextInt();
        if(aceita == 0){
            aux.setAce(false);
            aux.setIdTransportador("");
        } else if(aceita == 1){
            aux.setAceComprador(true);
            aux.setData(LocalDate.now());
        } else{
            throw new InvalidValueException("Opção inválida.");
        }

    }

}



    //• classificar, por parte do utilizador, o serviço de entrega;



    public void classificaEntrega(Encomenda e, int classificacao) throws IdNotFoundException {
        String transportador = e.getIdTransportador();

        if(voluntarios.getVoluntarios().containsKey(transportador)){
            try {
                Voluntario v = voluntarios.getVoluntario(transportador);
                float c = v.getClassificacao()* (totalEntregasVoluntario(v)-1) + classificacao;
                float b = (float) totalEntregasVoluntario(v);
                float x = c/b;
                v.setClassificacao(x);
            } catch (IdNotFoundException ex){
                System.out.println(ex.getMessage());
            }


        } else if(empresas.getEmpresas().containsKey(transportador)){

            try {
                Empresa em = empresas.getEmpresa(transportador);
                float  a = em.getClassificacao()* (totalEntregasEmpresa(em)-1) + classificacao;
                float d = (float) totalEntregasEmpresa(em);
                float f = a/d;
                em.setClassificacao(f);

            }catch (IdNotFoundException ex){
            System.out.println(ex.getMessage());
        }

        }

    }

    public int totalEntregasVoluntario(Voluntario v){
        String id = v.getId();
        int total = 0;
        for(Encomenda e : encomendas.getEncomendas().values()){
            if(e.getIdTransportador().equals(id) && e.getaceC()){
                total++;
            }
        }
        return total;

    }

    public int totalEntregasEmpresa(Empresa e){
        String id = e.getId();
        int total = 0;
        for(Encomenda en : encomendas.getEncomendas().values()){
            if(en.getIdTransportador().equals(id) && en.getaceC()){
                total++;
            }
        }
        return total;

    }



    //ter acesso no perfil de cada uma das entidades à informação sobre as encomendas transportadas (em função de data/hora de transporte);


    //indicar o total facturado por uma empresa transportadora num determinado período;
    public float totalFaturado(Empresa e, LocalDate inicio, LocalDate fim) throws IdNotFoundException {
        float total = 0;
        for(Encomenda enc: encomendas.getEncomendas().values()){
            if(enc.getData().isAfter(inicio) && enc.getData().isBefore(fim) && enc.getIdTransportador().equals(e.getId())){
                Loja j = lojas.getLoja(enc.getIdLoja());
                Utilizador u= utilizadores.getUtilizador(enc.getIdComprador());
                total+= (custoTransporte(e, j,u));
            }
        }
        return  total;

    }

    //determinar a listagens dos 10 utilizadores que mais utilizam o sistema (em número de encomendas transportadas)
    public List<Utilizador> top10UtilizadoresAtivos(){
        Map<String, Utilizador> aux = utilizadores.getUtilizadores();
        List<Utilizador> uti = new ArrayList<>();
        int cont = 0;

       for(Utilizador u : aux.values()){
           uti.add(u);

       }
       uti.sort(new ComparatorEncomendasUtilizador());
       List<Utilizador> ret = new ArrayList<>();

       while(cont<10 && cont<uti.size()){

           ret.add(uti.get(cont));
           cont++;
       }
       return ret;

    }


    //determinar a listagens das 10 empresas transportadoras que mais utilizam o sistema (em
    //número de kms percorridos)
    //REVER
    public List<Empresa> top10Empresas() throws IdNotFoundException {
        Map<String, Empresa> aux = empresas.getEmpresas();

        Map<String, Float> kms = new HashMap<>();
        float cont = 0;

        for (Empresa e : aux.values()) {
            cont = 0;
            for (Encomenda enc : encomendas.getEncomendas().values()) {
                if (enc.getIdTransportador().equals(e.getId())) {
                    cont += distancia(e.getLongitude(), e.getLatitude(), lojas.getLoja(enc.getIdLoja()).getLat(), lojas.getLoja(enc.getIdLoja()).getLong(), utilizadores.getUtilizador(enc.getIdComprador()).getLatitude(), utilizadores.getUtilizador(enc.getIdComprador()).getLongitude());

                }

            }
            kms.put(e.getId(), cont);

        }
        TreeSet<Map.Entry<String, Float>> aux2 = new TreeSet<>(new Comparator<Map.Entry<String, Float>>() {
            public int compare(Map.Entry<String, Float> f1, Map.Entry<String, Float> f2) {
                return f2.getValue() > f1.getValue() ? 1 : -1;
            }
        });
        aux2.addAll(kms.entrySet());

        Iterator k = aux2.iterator();


        Map.Entry<String, Float> t;
        int i = 0;
        List<String> result = new ArrayList<>();

        while (k.hasNext() && i < 10) {
            t = (Map.Entry<String, Float>) k.next();
            result.add(t.getKey());
            i++;
        }
        List<Empresa> res = new ArrayList<>();
        for(String id : result){
            Empresa e = empresas.getEmpresa(id);
            res.add(e);
        }
        return  res;
    }




    //gravar o estado da aplicação em ficheiro, para que seja possível retomar mais tarde a execução
    public void guardaEstado(String nome) throws FileNotFoundException, IOException {
        File f = new File(nome);
        if(!f.exists()){
            f.createNewFile();
        }
        FileOutputStream fos = new FileOutputStream(f);

        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }
    
}
