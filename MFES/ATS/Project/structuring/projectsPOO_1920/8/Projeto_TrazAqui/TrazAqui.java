import java.io.*;
import java.sql.SQLOutput;
import java.time.LocalDate;
import java.util.*;

public class TrazAqui implements Serializable{

        private Map<String,User> utilizadores; //HashMap para todos os users
        private Map<String,Encomenda> encomendas; //HashMap com todas as encomendas
        private Map<String,Encomenda> encaceites; //HashMap com todas as encomendas aceites
        private Map<String,List<String>> classpendentes;
        private Map<String,Produto> produtos;
        private User userlogado = null;
        private boolean backupDataRead = false;

        /**
         * Construtor da TrazAqui
         */

        public TrazAqui(){
            this.utilizadores = new HashMap<>();
            this.encomendas = new HashMap<>();
            this.encaceites = new HashMap<>();
            this.classpendentes = new HashMap<>();
            this.produtos = new HashMap<>();
        }

    /**
     *         SETTERS and GETTERS
     */

    public Produto getProduto(String cod) {
        if (this.produtos.containsKey(cod)){
            return this.produtos.get(cod).clone();
        }
        return null;
    }

    public Map<String, User> getutilizadores()
    {
        Map<String, User> nova = new HashMap<>();
        for(String s: this.utilizadores.keySet())
        {
            nova.put(s, this.utilizadores.get(s).clone());
        }
        return nova;
    }


    public void setutilizadores(Map<String, User> utilizadores)
    {
        this.utilizadores = new HashMap<>();
        for(String s: utilizadores.keySet())
        {
            this.utilizadores.put(s, utilizadores.get(s).clone());
        }
    }


    public Map<String, Encomenda> getencomendas()
    {
        Map<String, Encomenda> nova1 = new HashMap<>();
        for(String s: this.encomendas.keySet())
        {
            nova1.put(s, this.encomendas.get(s).clone());
        }
        return nova1;
    }

    public Encomenda getEncomenda(String cod){
        Encomenda e = null;
        if(this.encomendas.containsKey(cod) && !this.encaceites.containsKey(cod)){
            e = this.encomendas.get(cod).clone();
        }
        return e;
    }


    public void setencomendas(Map<String, Encomenda> encomendas)
    {
        this.encomendas = new HashMap<>();
        for(String s: encomendas.keySet())
        {
            this.encomendas.put(s, encomendas.get(s).clone());
        }
    }


    public Map<String, Encomenda> getencaceites()
    {
        Map<String, Encomenda> nova2 = new HashMap<>();
        for(String s: this.encaceites.keySet())
        {
            nova2.put(s, this.encaceites.get(s).clone());
        }
        return nova2;
    }


    public void setencaceites(Map<String, Encomenda> encaceites)
    {
        this.encaceites = new HashMap<>();
        for(String s: encaceites.keySet())
        {
            this.encaceites.put(s, encaceites.get(s).clone());
        }
    }

        /**
         * Dá logout do utilizador
         */
        public void logout() {
            updateUser(this.userlogado);
            setUserlogado(null);
        }

        /**
         * Lê o ficheiro csv e faz o parse retornando o Objeto TrazAqui já populado com a informação que o ficheiro csv tiver
         * @param fileName
         * @return TrazAqui com a informação do ficheiro csv já parsed.
         */
        public static TrazAqui getDataFromBackupFile(String fileName,TrazAqui t) throws IOException {
            if(fileName == null) return null;
            List<String> dataString = t.readFromFile(fileName);
            for (String s : dataString){
                parseStringAndAddToData(t,s);
            }
            t.setUserlogado(null);
            return t;
        }

        /**
         * Dá parse da string tendo em conta o primeiro parâmetro do ficheiro csv
         * @param trazAqui
         * @param s
         */
        private static void parseStringAndAddToData(TrazAqui trazAqui, String s) {
            String[] typeString = s.split(":");
            switch(typeString[0]) {
                case "Utilizador":
                    addCliente(trazAqui,typeString[1]);
                    break;
                case "Voluntario":
                    addVoluntario(trazAqui,typeString[1]);
                    break;
                case "Loja":
                    addLoja(trazAqui,typeString[1]);
                    break;
                case "Encomenda":
                    addEncomenda(trazAqui,typeString[1]);
                    break;
                case "Aceite":
                    addAceite(trazAqui,typeString[1]);
                    break;
                case "Transportadora":
                    addTransportadora(trazAqui, typeString[1]);
                default:
                    break;
            }

        }

        /**
         * Adiciona o User à aplicação através dos parâmetros do ficheiro csv.
         * @param trazAqui
         * @param string
         */
        private static void addCliente(TrazAqui trazAqui, String string){
            String[] fields = string.split(",");
            if(fields.length == 4) {
                Coordenadas pos = new Coordenadas(Double.parseDouble(fields[2]),Double.parseDouble(fields[3]));
                Cliente c = new Cliente(fields[0],fields[1],pos);
                trazAqui.addUser(c);
            }
        }

        /**
         * Adiciona o Voluntario à aplicação através dos parâmetros do ficheiro csv.
         * @param trazAqui
         * @param string
         */
        private static void addVoluntario(TrazAqui trazAqui, String string) {
            String[] fields = string.split(",");
            if(fields.length == 5) {
                Coordenadas pos = new Coordenadas(Double.parseDouble(fields[2]),Double.parseDouble(fields[3]));
                Voluntario v = new Voluntario(fields[0],fields[1],pos,Double.parseDouble(fields[4]));
                trazAqui.addUser(v);
            }
        }

        /**
         * Adiciona o Empresa à aplicação através dos parâmetros do ficheiro csv.
         * @param trazAqui
         * @param string
         */
        private static void addTransportadora(TrazAqui trazAqui, String string) {
            String[] fields = string.split(",");
            if(fields.length == 7) {
                Coordenadas pos = new Coordenadas(Double.parseDouble(fields[2]),Double.parseDouble(fields[3]));
                Empresaentrega emp = new Empresaentrega(fields[0],fields[1],pos,fields[4],Double.parseDouble(fields[5]),Double.parseDouble(fields[6]));
                trazAqui.addUser(emp);
            }
        }

        /**
         * Adiciona o Loja à aplicação através dos parâmetros do ficheiro csv.
         * @param trazAqui
         * @param string
         */
        private static void addLoja(TrazAqui trazAqui, String string) {
            String[] fields = string.split(",");
            if(fields.length == 4) {
                Coordenadas pos = new Coordenadas(Double.parseDouble(fields[2]),Double.parseDouble(fields[3]));
                Loja l = new Loja(fields[0], fields[1],pos );
                List<Encomenda> fe = new ArrayList<>();
                for(Map.Entry<String,Encomenda> m : trazAqui.getencomendas().entrySet()){
                    if(m.getValue().getReferenciaLoj().equals(fields[0]) && !trazAqui.getencaceites().containsKey(m.getKey())){
                        fe.add(m.getValue().clone());
                    }
                }
                l.setFilaespera(fe);
                trazAqui.addUser(l);
            }
        }

        /**
         * Adiciona o Encomenda à aplicação através dos parâmetros do ficheiro csv.
         * @param trazAqui
         * @param string
         */

        private static void addEncomenda(TrazAqui trazAqui, String string) {
            String[] fields = string.split(",");
            List<Produto> lista = new ArrayList<>();
            int i = 4;
            while ( i < fields.length){
                String cod = fields[i];
                i++;
                String des = fields[i];
                i++;
                double qtd = Double.parseDouble(fields[i]);
                i++;
                double price = Double.parseDouble(fields[i]);
                i++;
                Produto p = new Produto(cod,des,qtd,price);

                trazAqui.produtos.put(cod,p.clone());
                lista.add(p.clone());
            }
            Encomenda enc = new Encomenda(fields[0],fields[1],fields[2],Double.parseDouble(fields[3]),lista);
            trazAqui.addEnc(enc);
            User u = trazAqui.utilizadores.get(fields[1]).clone();
            User lu = trazAqui.utilizadores.get(fields[2]).clone();
            if(u != null){
                Cliente c = (Cliente) u;
                c.addEnc(enc);
                trazAqui.updateUser(c);
            }
            if (lu != null){
                Loja l = (Loja) lu;
                l.addEncomenda(enc);
                trazAqui.updateUser(l);
            }
        }

        private static void addAceite(TrazAqui t, String string){
            Encomenda enc = t.encomendas.get(string);
            Cliente c = (Cliente) t.utilizadores.get(enc.getReferenciaUti()).clone();
            Loja l = (Loja) t.utilizadores.get(enc.getReferenciaLoj()).clone();

            for(Map.Entry<String,User> entry : t.utilizadores.entrySet()){
                User u = entry.getValue().clone();
                if (u instanceof Voluntario) {
                    Voluntario v = (Voluntario) u;
                    if (v.getPosicao().distancia_Coordenadas(l.getPosicao()) <= v.getRaio_acao() && l.getPosicao().distancia_Coordenadas(c.getPosicao()) <= v.getRaio_acao()) {
                        v.addEnc(enc);
                        t.add_pend(enc.getReferenciaUti(),v.getUsername());
                        t.updateUser(v);
                        break;
                    }
                }
                else if (u instanceof Empresaentrega){
                    Empresaentrega e = (Empresaentrega) u;
                    if (e.getPosicao().distancia_Coordenadas(l.getPosicao()) <= e.getRaio() && l.getPosicao().distancia_Coordenadas(c.getPosicao()) <= e.getRaio()){
                        e.addEnc(enc);
                        t.add_pend(enc.getReferenciaUti(),e.getUsername());
                        t.updateUser(e);
                        break;
                    }
                }
            }
            List<Encomenda> e = new ArrayList<>();
            l.setFilaespera(e);
            t.updateUser(l);
            t.addEncAceite(string);
        }

        public void addEncAceite(String ref){
            Encomenda e = this.encomendas.get(ref).clone();
            this.encaceites.put(e.getReferencia(),e);
        }

        public void addEnc(Encomenda e){
            encomendas.put(e.getReferencia(),e.clone());
        }


        public User getUser(String username){
            if (utilizadores.containsKey(username)) return utilizadores.get(username).clone();
            else return null;
        }

        /**
         * Adiciona um utilizador á aplicação
         *
         */
        public void addUser(User user){
            utilizadores.put(user.getUsername(),user.clone());
        }

        /**
         *  Metodo para dar update ao map dos users
         */
        public void updateUser (User user ) {
            utilizadores.put(user.getUsername(),user.clone());
        }

        /**
         * Verifica se já foi lida a data do ficheiro .bak
         * @return true se sim, falso se não
         */
        public boolean isBackupDataRead() {return this.backupDataRead;}

        /**
         * Mete a true se a data do ficheiro .bak já foi lida
         */
        public void setBackupDataRead() {this.backupDataRead = true;}

        /**
         * Verifica se há algum user logado
         * @return true, se sim, false se não
         */
        public boolean isUserLogado () {
            return (userlogado != null);
        }

        public void setUserlogado(User userlogado) {
            this.userlogado = userlogado;
        }

        public User getUserLogado(){
            return this.userlogado.clone();
        }

    /**
         * Faz login na aplicação confirmando o username e a password.
         * @param username
         * @param pass
         * @return true caso o login seja bem sucedido , false caso contrário.
         */
        public boolean login (String username, String pass) {
            boolean status = false;
            if(utilizadores.containsKey(username)){
                User aux = this.utilizadores.get(username);
                status = aux.getPassword().equals(pass);
                if(status) {
                    setUserlogado(aux);
                }
            }
            return status;
        }


        /**
         * Lê de um ficheiro para uma List<String>
         * @return List<String> que leu do ficheiro
         * @throws FileNotFoundException
         * @throws IOException
         */

        public List<String> readFromFile(String fileName) throws FileNotFoundException, IOException {
            List<String> linhas = new ArrayList<>();
            BufferedReader br = new BufferedReader(new FileReader(fileName));
            String linha;
            while ((linha = br.readLine()) != null) linhas.add(linha);
            br.close();
            return linhas;
        }

    /**
     * Recupera o estado da aplicação
     * @return TrazAqui
     */
        public static TrazAqui recoverState() {
            TrazAqui t = null;
                try {
                    FileInputStream fis = new FileInputStream("src/teste.tmp");
                    ObjectInputStream ois = new ObjectInputStream(fis);
                    t = (TrazAqui) ois.readObject();
                    System.out.println("Dados Lidos");
                    ois.close();
                } catch (FileNotFoundException e) {
                    System.out.println("Ficheiro de carregamento de dados não existe");
                } catch (Exception e) {

                }
            if (t == null) t = new TrazAqui();
            return t;
        }

        /**
         * Guarda o estado num object file
         */
        public void saveState() {
            try {
                FileOutputStream fos = new FileOutputStream("src/teste.tmp");
                ObjectOutputStream oos = new ObjectOutputStream(fos);
                oos.writeObject(this);
                System.out.println("Dados Gravados");
                oos.flush();
                oos.close();
            } catch (IOException e) {
                System.out.println(e.getMessage());
            }
        }

        /**
         *  Adiciona a encomendas feitas por um utilizador ao seu histórico:
         * @param username  o código do utilizador
         */

        public Cliente add_enc_hist_util(String username){
            for (Encomenda p: this.encaceites.values()) {
                if (p.getReferenciaUti().equals(username)) {
                    Encomenda res = p.clone();
                    Cliente c = (Cliente) this.utilizadores.get(username).clone();
                    List<Encomenda> lista = new ArrayList<>(c.getHistorico());
                    lista.add(res);
                    c.setHistorico(lista);
                    return c;
                }
            }
            return null;
        }

        /**
         * Adiciona tudo na hashMap de utilizadores
        */
        public void atualiza(){
            for (Map.Entry<String,User> p :this.utilizadores.entrySet()){
                Cliente c = add_enc_hist_util(p.getKey());
                if (c!=null) {
                    updateUser(c);
                }
            }
            for(Map.Entry<String,User> entry : this.utilizadores.entrySet()){
                if (entry.getValue() instanceof Loja){
                    Loja l = (Loja) entry.getValue().clone();
                    for(Map.Entry<String,Encomenda> m : this.getencomendas().entrySet()){
                        if(m.getValue().getReferenciaLoj().equals(l.getUsername()) && !this.encaceites.containsKey(m.getKey())){
                            l.addEncomenda(m.getValue().clone());
                        }
                    }
                    this.updateUser(l);
                }
            }
        }

    public void show_encomendas_pendentes(String username) {
        List <Encomenda> lista1= new ArrayList<>();
        for(Map.Entry<String,Encomenda> e : this.encomendas.entrySet()){
            if(!this.encaceites.containsKey(e.getKey())){
                Encomenda enc = e.getValue();
                if (enc.getReferenciaUti().equals(username)) {
                    lista1.add(enc.clone());
                }
            }
        }
        if(lista1.size() == 0) {
            System.out.print("De momento não tem encomendas pendentes para visualizar \n");
        }
        else
        {
            System.out.println(lista1.toString());
        }
    }

    public void show_empresas(){
            for (User u : this.utilizadores.values()) {
                if (u instanceof Empresaentrega) {
                    System.out.println("Username: " + u.getUsername());
                    System.out.println("Nome: " + u.getNome());
                }
            }
    }

    public List<Encomenda> show_encomendas(User u){
            List aux = new ArrayList<>();
            for (Encomenda e : this.encomendas.values()){
                if (!this.encaceites.containsKey(e.getReferencia()) && esta_no_raio((Empresaentrega) u,e) ){
                    aux.add(e.clone());
                }
            }
            return aux;
    }

    public boolean esta_no_raio(Empresaentrega t,Encomenda e) {
        return (t.getPosicao().distancia_Coordenadas(this.utilizadores.get(e.getReferenciaLoj()).getPosicao()) <= t.getRaio()
                && t.getPosicao().distancia_Coordenadas(this.utilizadores.get(e.getReferenciaUti()).getPosicao()) <= t.getRaio());
    }



    public Empresaentrega getEmpresa(String username){
        Empresaentrega e= null;
        for(Map.Entry<String,User>p : this.utilizadores.entrySet()){
            if(p.getKey().equals(username)) {
                e = (Empresaentrega)p.getValue().clone();
                return e;
            }
        }
        return e;
    }

    public void show_voluntarios(){
        for (User u : this.utilizadores.values()) {
            if (u instanceof Voluntario) {
                System.out.println("Username: " + u.getUsername());
                System.out.println("Nome: " + u.getNome());
            }
        }
    }

    public Voluntario getVoluntario(String username){
        Voluntario v = null;
        for(Map.Entry<String,User>p : this.utilizadores.entrySet()){
            if(p.getKey().equals(username)) {
                v = (Voluntario) p.getValue().clone();
                return v;
            }
        }
        return v;
    }

    public void show_Lojas(){
        for (User u : this.utilizadores.values()){
            if (u instanceof Loja) {
                System.out.println("Username: " + u.getUsername() + " -> " + u.getNome() +"\n");
            }
        }
    }

    /**
     * Funçaõ para alterar a disponibilidade
     * @param u
     * @return
     */

    public User altera_disp(User u){
        if (u instanceof Voluntario){
            if (((Voluntario) u).getDisponivel()){
                ((Voluntario) u).setDisponivel(false);
            }else{
                ((Voluntario) u).setDisponivel(true);
            }
        }else if (u instanceof Empresaentrega){
            if (((Empresaentrega) u).getProntaReceber()){
                ((Empresaentrega) u).setProntaReceber(false);
            }else{
                ((Empresaentrega) u).setProntaReceber(true);
            }
        }
        updateUser(u);
        return u;
    }

    /**
     * Funçao para alterar o visto médico.
     * @param u
     * @return this.userlogado
     */

    public User altera_visto(User u){
        if (u instanceof Voluntario){
            if (((Voluntario) u).getVerificado()){
                ((Voluntario) u).setVerificado(false);
            }else{
                ((Voluntario) u).setVerificado(true);
            }
        }else if (u instanceof Empresaentrega){
            if (((Empresaentrega) u).getVistoMedico()){
                ((Empresaentrega) u).setVistoMedico(false);
            }else{
                ((Empresaentrega) u).setVistoMedico(true);
            }
        }
        updateUser(u);
        return u;
    }

    public void mostra_Enc_pendentes(){
        List <Encomenda> lista1 = new ArrayList<>();
        for(Map.Entry<String,Encomenda> e : this.encomendas.entrySet()){
            if(!this.encaceites.containsKey(e.getKey())){
                Encomenda enc = e.getValue();
                lista1.add(enc.clone());
            }
        }
        if(lista1.size() == 0) {
            System.out.print("De momento não tem encomendas pendentes para visualizar \n");
        }
        else
        {
            System.out.println(lista1.toString());
        }
    }

    public void add_pend(String uti, String cod){
            List<String> temp = this.classpendentes.get(uti);
            if(temp == null){
                temp = new ArrayList<>();
            }
            if(!temp.contains(cod)){
                temp.add(cod);
            }
            this.classpendentes.put(uti,temp);
    }

    public void remove_pend(String uti, String cod){
            List<String> temp = this.classpendentes.get(uti);
            for(String s : temp){
                if (s.equals(cod)){
                    temp.remove(s);
                    break;
                }
            }
            this.classpendentes.put(uti,temp);
    }

    /**
     * Alterar o tempo de atendimento de uma loja
     */

    public void altera_atend(User l, double x){
        ((Loja) l).setTempoAtendimento(x);
    }

    public void aceitaEncomenda(Voluntario v, String codenc){
            Encomenda enc = this.encomendas.get(codenc).clone();
            if (enc == null){
                System.out.println("Nao existe essa encomenda.");
            }
            else{
                v.setDisponivel(false);
                String codLoja = enc.getReferenciaLoj();
                String codUti = enc.getReferenciaUti();
                Loja l = (Loja) this.utilizadores.get(codLoja).clone();
                Cliente c = (Cliente) this.utilizadores.get(codUti).clone();
                double distancia1 = v.getPosicao().distancia_Coordenadas(l.getPosicao());
                double distancia2 = v.getPosicao().distancia_Coordenadas(c.getPosicao());

                if (distancia1 >= v.getRaio_acao() || distancia2 >= v.getRaio_acao()){
                    System.out.println("Esta fora do seu raio de açao.");
                    v.setDisponivel(true);
                }
                else{
                    this.encaceites.put(enc.getReferencia(),enc.clone());
                    v.addEnc(enc);
                    c.addEnc(enc);
                    l.removeEncomenda(enc.clone());
                    add_pend(codUti,v.getUsername());
                }
            }
    }

    public List<String> get_classificoes_pendentes(String username){
        return this.classpendentes.get(username);
    }


    /**
     * método para obter o preço de uma encomenda
     */

    public double Def_preco(Empresaentrega u,String codenc){
        double caminho = 0;
        Encomenda enc = this.encomendas.get(codenc).clone();
        if (enc == null){
            System.out.println("Nao existe essa encomenda.");
        }else {
            double preco  = enc.calculaValorLinhaEnc();
            String codLoja = enc.getReferenciaLoj();
            String codUti = enc.getReferenciaUti();
            Loja l = (Loja) this.utilizadores.get(codLoja).clone();
            Cliente c = (Cliente) this.utilizadores.get(codUti).clone();
            double distancia1 = u.getPosicao().distancia_Coordenadas(l.getPosicao());
            double distancia2 = u.getPosicao().distancia_Coordenadas(c.getPosicao());
            double distancia3 = l.getPosicao().distancia_Coordenadas(c.getPosicao());

            if (distancia1 >= u.getRaio() || distancia2 >= u.getRaio()){
                u.setProntaReceber(true);
            }else{
                caminho = u.getTaxa() * (distancia1 + distancia3);
            }
        }
        return caminho;
    }

    /**
     * Total faturado por uma empresa num determinado periodo de tempo
     * @param e
     * @param d1
     * @param d2
     */

    public double total_faturado(Empresaentrega e , LocalDate d1, LocalDate d2){
        double ret = 0;
        for(Encomenda enc : e.getEncomendas()){
            if (enc.getData().isAfter(d1) && enc.getData().isBefore(d2)){
                ret += Def_preco(e,enc.getReferencia());
            }
        }
        return ret;
    }


    public void classificar(String username, double classificao){
            User u = this.utilizadores.get(username).clone();
            if (u == null){
                System.out.println("Nao escolheu um voluntario/empresa que esta por classificar.");
            }
            else{
                if (u instanceof Voluntario){
                    Voluntario v = (Voluntario) u;
                    v.updateClass(classificao);
                    this.utilizadores.put(v.getUsername(),v.clone());
                    remove_pend(this.userlogado.getUsername(),u.getUsername());
                }
                else if (u instanceof Empresaentrega){
                    Empresaentrega e = (Empresaentrega) u;
                    e.updateClass(classificao);
                    this.utilizadores.put(e.getUsername(),e.clone());
                    remove_pend(this.userlogado.getUsername(),u.getUsername());
                }
            }
    }

    /**
     * Função que retorna um Set do top10 de utilizadores
     * @return
     */

    public Set<User> top10_u(){
        List<User> aux = new ArrayList<>();
        Set<User> ret = new TreeSet<>(new ComparatorNumeroEncomendas());
        for(User u : this.utilizadores.values()){
            if (u instanceof Voluntario){
                Voluntario v = (Voluntario) u.clone();
                aux.add(v);
            }
            if (u instanceof Empresaentrega){
                Empresaentrega e = (Empresaentrega) u.clone();
                aux.add(e);
            }
        }
        for (User u : aux) {
            ret.add(u.clone());
        }
        return ret;
    }

    /**
     * Calcula a distancia percorrida por uma empresa
     * @param ee
     */
    public void distancia_total(Empresaentrega ee){
        double ret = 0;
        for(Encomenda e : ee.getEncomendas()){
            Loja l = (Loja) this.getutilizadores().get(e.getReferenciaLoj()).clone();
            Cliente c = (Cliente) this.getutilizadores().get(e.getReferenciaUti()).clone();
            double distancia1 = ee.getPosicao().distancia_Coordenadas(l.getPosicao());
            double distancia2 = l.getPosicao().distancia_Coordenadas(c.getPosicao());
            ret += distancia1 + distancia2;
        }
        ee.setDistancia(ret);
    }

    /**
     * Retorna um Set com o top10 de empresas com mais kilometros percorridos.
     * @return Set
     */
    public Set<Empresaentrega> top10_km(){
        List<Empresaentrega> aux = new ArrayList<>();
        for(User u : this.getutilizadores().values()){
            if (u instanceof Empresaentrega){
                distancia_total((Empresaentrega) u);
                aux.add((Empresaentrega) u.clone());
            }
        }
        Set<Empresaentrega> ret = new TreeSet<>(new ComparatorKm());
        int tam = 10;
        for(User u : aux){
            if (tam > 0){
                ret.add((Empresaentrega) u.clone());
                tam -= 1;
            }
        }
        return ret;
    }


    public Set<User> print_top10_Utilizadores(TrazAqui t){
        Set<User> s = t.top10_u();
        int tam = 10;
        for(User u : s){
            if (tam > 0){
                Voluntario v1 = u instanceof Voluntario ? (Voluntario) u : null;
                Empresaentrega e1 = u instanceof Empresaentrega ? (Empresaentrega) u: null;
                int s1 = v1 != null ? v1.getEntregas_feitas().size(): e1.getEncomendas().size();
                System.out.print("Empresa: " + u.getNome() + " -> Numero de encomendas: " + s1 + "\n");
                tam -= 1;
            }
        }
        return s;
    }

    public void print_top10_Empresas(TrazAqui t){
        int tam = 10;
        Set<Empresaentrega> se = t.top10_km();
        for(Empresaentrega e : se){
            if (tam > 0){
                double dist = e.getDistancia();
                System.out.println("Empresa: " + e.getNome() + " ----> Numero de km: " + dist);
                tam -= 1;
            }
        }
    }

    public void show_produtos(){
        for (Produto p : this.produtos.values()) {
            System.out.println("Codigo do produto: " + p.getreferencia());
            System.out.println("Nome: " + p.getdescricao());
        }
    }

    public int aceitaenc_empresa(TrazAqui t , User u , Encomenda enc){
        Empresaentrega et = (Empresaentrega) u;
        Cliente destino = (Cliente) t.getutilizadores().get(enc.getReferenciaUti()).clone();
        double caminho = t.Def_preco(et,enc.getReferencia());
        if (caminho > 0) {
            destino.addEnc_poraceitar(et.getUsername(), enc);
            t.updateUser(destino);
            return 1;
        }else{
            return 0;
        }
    }

    public void aceita_transportadora(TrazAqui t, User u , String codEmp){
        Cliente c = (Cliente) u;
        Encomenda enc = c.getEncomenda(codEmp);
        Loja l = (Loja) t.getutilizadores().get(enc.getReferenciaLoj()).clone();
        Empresaentrega et = (Empresaentrega) t.getutilizadores().get(codEmp).clone();
        c.addEnc(enc);                       //adiciona encomenda ao historico
        c.removeEnc(enc);                    //remove encomenda das encomendas por aceitar
        t.addEncAceite(enc.getReferencia()); //adiciona a encomenda ao hashmap das aceites em trazaqui
        t.add_pend(c.getUsername(),codEmp);  //adiciona a classificação por fazer
        et.addEnc(enc);                      //adiciona encomenda ao historico da empresa
        l.removeEncomenda(enc.clone());      //remove a encomenda da fila de espera
        t.addUser(et);                       //atualiza empresa
        t.updateUser(c);
        t.updateUser(l);
    }

    public List<Encomenda> util_Historico(Cliente u,LocalDate d1,LocalDate d2){
        List<Encomenda> aux = new ArrayList<>();
        for (Encomenda d : u.getHistorico()){
            if(d1.isBefore(d.getData()) && d2.isAfter(d.getData())){
                aux.add(d.clone());
            }
        }
        return aux;
    }
}
