/**
 * Escreva a descrição da classe Estado aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */
import java.util.Scanner;
import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.Callable;
import java.util.Random;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Scanner;
import java.util.TreeSet;

public class Estado implements Serializable{
    private Logins logins;
    private TreeSet<Cliente> top10_clientes;
    private TreeSet<Empresa> top10_empresas;
    private TreeSet<Voluntario> voluntarios;
    private TreeSet<Loja> lojas;
    private TreeSet<Encomenda> aceites;
    private TreeSet<Encomenda> encomendas;
    private TreeSet<Encomenda> pedidos;
    private TreeSet<Produto> produtos;
    
    /**
     * Construtores
     */
    public Estado(){
        this.logins = new Logins();
        this.top10_clientes = new TreeSet<>(new CompClientes());
        this.voluntarios = new TreeSet<>(new CompVoluntarios());
        this.top10_empresas = new TreeSet<>(new CompEmpresas());
        this.lojas = new TreeSet<>(new CompLojas());
        this.encomendas = new TreeSet<>(new CompEncomendas());
        this.aceites = new TreeSet<>(new CompEncomendas());
        this.pedidos = new TreeSet<>(new CompEncomendas());
        this.produtos = new TreeSet<>(new CompProdutos());
    }
    
    public Estado(Transporte transporte, Loja loja, Encomenda encomenda, Produto produto, Cliente cliente, Empresa empresa, Logins logins, TreeSet<Cliente> clientes, TreeSet<Voluntario> voluntarios, TreeSet<Empresa> empresas, TreeSet<Loja> lojas, TreeSet<Encomenda> pedidos, TreeSet<Encomenda> encomendas, TreeSet<Encomenda> aceites, TreeSet<Produto> produtos){
        this.logins = logins;
        this.top10_clientes = clientes;
        this.voluntarios = voluntarios;
        this.top10_empresas = empresas;
        this.lojas = lojas;
        this.encomendas = encomendas;
        this.aceites = aceites;
        this.pedidos = pedidos;
        this.produtos = produtos;
    }
    
    public Estado(Estado e){
        this.logins = getLogins();
        this.top10_clientes = e.getClientes();
        this.voluntarios = e.getVoluntarios();
        this.top10_empresas = e.getEmpresas();
        this.lojas = e.getLojas();
        this.encomendas = e.getPedidos();
        this.encomendas = e.getEncomendas();
        this.aceites = e.getAceites();
        this.produtos = e.getProdutos();
    }
    
    /**
     * Get's
     */
    public TreeSet<Cliente> getClientes(){
        TreeSet<Cliente> campos = new TreeSet<>(new CompClientes());
        for(Cliente c :this.top10_clientes){
           campos.add(c.clone()); 
        }
        return campos;
    }
    
    public TreeSet<Voluntario> getVoluntarios(){
        TreeSet<Voluntario> campos = new TreeSet<>(new CompVoluntarios());
        for(Voluntario v :this.voluntarios){
           campos.add(v.clone()); 
        }
        return campos;
    }
    
    public TreeSet<Empresa> getEmpresas(){
        TreeSet<Empresa> campos = new TreeSet<>(new CompEmpresas());
        for(Empresa e :this.top10_empresas){
           campos.add(e.clone()); 
        }
        return campos;
    }
    
    public TreeSet<Loja> getLojas(){
        TreeSet<Loja> campos = new TreeSet<>(new CompLojas());
        for(Loja l :this.lojas){
           campos.add(l.clone()); 
        }
        return campos;
    }
    
    public TreeSet<Encomenda> getPedidos(){
        TreeSet<Encomenda> campos = new TreeSet<>(new CompEncomendas());
        for(Encomenda e :this.pedidos){
           campos.add(e); 
        }
        return campos;
    }
    
    public TreeSet<Encomenda> getEncomendas(){
        TreeSet<Encomenda> campos = new TreeSet<>(new CompEncomendas());
        for(Encomenda e :this.encomendas){
           campos.add(e.clone()); 
        }
        return campos;
    }
    
    public TreeSet<Encomenda> getAceites(){
        TreeSet<Encomenda> campos = new TreeSet<>(new CompEncomendas());
        for(Encomenda e :this.aceites){
           campos.add(e.clone());
        }
        return campos;
    }
    
    public TreeSet<Produto> getProdutos(){
        TreeSet<Produto> copia = new TreeSet<>();
        for(Produto p :this.produtos){
           copia.add(p.clone()); 
        }
        return copia;
    }
    
    public Logins getLogins(){
        return this.logins.clone();
    }
    
    /**
     * Set's
     */
    public void setLogins(Logins logins){
        this.logins = logins;
    }
    
    public void setClientes(TreeSet<Cliente> clientes){
        this.top10_clientes = clientes;
    }
    
    public void setVoluntarios(TreeSet<Voluntario> voluntarios){
        this.voluntarios = voluntarios;
    }
    
    public void setEmpresas(TreeSet<Empresa> empresas){
        this.top10_empresas = empresas;
    }
    
    public void setLojas(TreeSet<Loja> lojas){
        this.lojas = lojas;
    }
    
    public void setPedidos(TreeSet<Encomenda> pedidos){
        this.pedidos = pedidos;
    }
    
    public void setEncomendas(TreeSet<Encomenda> encomendas){
        this.encomendas = encomendas;
    }
    
    
    public void setProdutos(TreeSet<Produto> produtos){
        this.produtos = produtos;
    }
    
    /**
     * clone
     */
    public Estado clone(){
        return (new Estado(this));
    }
    
    /**
     * Obtem a informação das encomendas apartir do código do transportador
     */
    public ArrayList<String> obteminfoencomendas(String codT){
        Transporte tra = getTransporte(codT);
        ArrayList<String> copia = new ArrayList<>();
        for (Encomenda e: this.pedidos){
            if (e.getTransporte().getCodT().equals(codT)){
                copia.add(e.getCodEncomenda());
            }
        }
        return copia;
    }
    
    
    public TreeSet<Cliente> top10cliente(){
        TreeSet<Cliente> top10 = new TreeSet<>(new CompClientes());
        int i = 0;
        for(Cliente c: this.top10_clientes){
            if (i <= 10){
                top10.add(c.clone());
            }
        }
        return top10;
    }
    
    public ArrayList<String> obtem_top10C(){
        ArrayList<String> top10 = new ArrayList<>();
        TreeSet<Cliente> top = top10cliente();
        for (Cliente c: top){
            top10.add(c.getNome());
        }
        return top10;
    }
    
    public ArrayList<String> obtem_top10E(){
        ArrayList<String> top10 = new ArrayList<>();
        TreeSet<Empresa> top = top10empresas();
        for (Empresa c: top){
            top10.add(c.getNome());
        }
        return top10;
    }
    
    public TreeSet<Empresa> top10empresas(){
        TreeSet<Empresa> top10 = new TreeSet<>(new CompEmpresas());
        int i = 0;
        for(Empresa e: this.top10_empresas){
            if (i <= 10){
                top10.add(e.clone());
            }
        }
        return top10;
    }
    
    /**
     * equals
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Estado l = (Estado) obj;
        return l.getClientes().equals(this.top10_clientes) &&
               l.getVoluntarios().equals(this.voluntarios) &&
               l.getEmpresas().equals(this.top10_empresas) &&
               l.getLojas().equals(this.lojas) &&
               l.getPedidos().equals(this.pedidos) &&
               l.getEncomendas().equals(this.encomendas) &&
               l.getProdutos().equals(this.produtos);
    }
    
    /**
     * toString
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Clientes: ").append(this.top10_clientes).append("\n");
        sb.append("Voluntarios: ").append(this.voluntarios).append("\n");
        sb.append("Empresas: ").append(this.top10_empresas).append("\n");
        sb.append("Lojas: ").append(this.lojas).append("\n");
        sb.append("Pedidos: ").append(this.pedidos).append("\n");
        sb.append("Encomendas: ").append(this.encomendas).append("\n");
        sb.append("Produtos: ").append(this.produtos).append("\n");
        return sb.toString();
    }
    
    /**
     * Adiciona uma lojas à lista de lojas
     */
    public void adicionaLoja(Loja loja){
        this.lojas.add(loja);
    }
    
    /**
     * Adiciona um transportador(empresa ou voluntário)
     */
    public void adicionaTransporte(Transporte t){
        if(t instanceof Empresa)this.top10_empresas.add((Empresa) t);
        if(t instanceof Voluntario)this.voluntarios.add((Voluntario) t);
    }
    
    /**
     * Adiciona cliente ao top 10 dos clientes
     */
    public void adicionaCliente(Cliente cliente){
        this.top10_clientes.add(cliente);
    }
    
    /**
     * Adiciona um pedido à lista de pedidos
     */
    public void adicionaPedido(Encomenda encomenda){
        this.pedidos.add(encomenda);
    }
    
    /**
     * Adiciona uma encomenda às encomendas aceites
     */
    public void adicionaAceite(Encomenda encomenda){
        this.aceites.add(encomenda);
    }
    
    /**
     * Adiciona uma encomenda às encomendas
     */
    public void adicionaEncomenda(Encomenda encomenda){
        this.encomendas.add(encomenda);
    }   
    
    /**
     * Obtém uma lista de lojas 
     */
    public ArrayList<String> obtemLojas (){
        ArrayList<String> campos = new ArrayList<>();
        for(Loja loja: this.lojas){
            campos.add(loja.getNome());
        }
        return campos;
    }
    
    /**
     * Adiciona um produto à loja, tendo em conta o preço, a referência, etc
     */
    public void add_Prod_Loja(String codL, String codP, String descricao, double valorUnitario, int quantidade, double pesounitario){
        Loja loja = getLojaCod(codL);
        lojas.remove(loja);
        loja.adicionaProduto(new Produto(codP, descricao, valorUnitario, quantidade, pesounitario));
        atualiza_Loja(loja);
    }
    
    /**
     * Obtem todos os produtos apartir do seu 
     */
    public ArrayList<String> obtemProdutos(String nomeL){
        Loja loja = getLoja(nomeL);
        ArrayList<String> campos = new ArrayList<>();
        for(Produto p: loja.getProdutos()){
            campos.add(p.getDescricao());
        }
        return campos;
    }
    
    public Loja getLoja(String nomeL){
        for(Loja loja: this.lojas){
            if (nomeL.equals(loja.getNome())){
                return loja.clone();
            }
        }
        return null;
    }
    
    public Loja getLojaCod(String codL){
        for(Loja loja: this.lojas){
            if (codL.equals(loja.getCodL())){
                return loja.clone();
            }
        }
        return null;
    }
    
    public int obtemQuantProd(String nomeL, String prod){
        Loja loja = getLoja(nomeL);
        for(Produto produto: loja.getProdutos()){
            if (produto.getDescricao().equals(prod)){
                return produto.getQuantidade();
            }
        }
        return 0;
    }
    
    public Encomenda getPedido(String codE){
        for(Encomenda e: this.pedidos){
            if (codE.equals(e.getCodEncomenda())){
                return e;
            }
        }
        return null;
    }
    /*
    public Cliente getCliente(String cliente){
        for(Cliente c: this.top10_clientes){
            if (cliente.equals(c.getNome())){
                return c;
            }
        }
        return null;
    }
    */
    public Cliente getCliente(String codC){
        for(Cliente c: this.top10_clientes){
            if (codC.equals(c.getCodC())){
                return c.clone();
            }
        }
        return null;
    }
    
    public Encomenda getEncomenda(String codE){
        for(Encomenda e: this.encomendas){
            if (codE.equals(e.getCodEncomenda())){
                return e.clone();
            }
        }
        return null;
    }
    
    public Encomenda getAceite(String codE){
        for(Encomenda e: this.aceites){
            if (codE.equals(e.getCodEncomenda())){
                return e.clone();
            }
        }
        return null;
    }
    
    public void atualiza_Loja(Loja escolhida){
        for(Loja loja: this.lojas){
            if (escolhida.equals(loja)){
                this.lojas.remove(loja);
            }            
        }
        this.lojas.add(escolhida);
    }
    
    public void atualiza_Empresa(Empresa escolhida){
        for(Empresa emp: this.top10_empresas){
            if (escolhida.equals(emp)){
                this.top10_empresas.remove(emp);
            }            
        }
        this.top10_empresas.add(escolhida);
    }
    
    public void atualiza_Cliente(Cliente cliente){
        for(Cliente c: this.top10_clientes){
            if (cliente.equals(c)){
                this.top10_clientes.remove(c);
            }
        }
        this.top10_clientes.add(cliente);
    }
    
    public void atualiza_Encomenda(Encomenda encomenda){
        for(Encomenda c: this.encomendas){
            if (encomenda.equals(c)){
                this.encomendas.remove(c);
            }
        }
        this.encomendas.add(encomenda);
    }
    
    public void atualiza_Aceites(Encomenda aceite){
        for(Encomenda c: this.aceites){
            if (aceite.equals(c)){
                this.aceites.remove(c);
            }
        }
        this.aceites.add(aceite);
    }
    
    public void atualiza_Pedidos(Encomenda pedido){
        for(Encomenda c: this.pedidos){
            if (pedido.equals(c)){
                this.pedidos.remove(c);
            }
        }
        this.pedidos.add(pedido);
    }
    
    public ArrayList<String> obteminfoencomendasCliente(String codC, LocalDateTime hora_1, LocalDateTime hora_2){
        Cliente c = getCliente(codC);
        ArrayList<String> copia = new ArrayList<>();
        for (Encomenda e: this.encomendas){
            if (e.getDestino().getCodC().equals(codC) && e.getHora().isAfter(hora_1) && e.getHora().isBefore(hora_2)){
                copia.add(e.getCodEncomenda());
            }
        }
        return copia;
    }
    
    public ArrayList<String> obteminfoencomendasL(String codL, LocalDateTime hora_1, LocalDateTime hora_2){
        Loja loja = getLoja(codL);
        ArrayList<String> lista_encomendas = new ArrayList<>();
        for (Encomenda e: this.encomendas){
            if (e.getVendedor().getCodL().equals(codL) && e.getHora().isAfter(hora_1) && e.getHora().isBefore(hora_2)){
                lista_encomendas.add(e.getCodEncomenda());
            }
        }
        return lista_encomendas;
    }
    
    public ArrayList<String> obteminfoPedidos(String codL){
        Loja loja = getLoja(codL);
        ArrayList<String> lista_encomendas = new ArrayList<>();
        for (Encomenda e: this.pedidos){
            if (e.getVendedor().getCodL().equals(codL)){
                lista_encomendas.add(e.getCodEncomenda());
            }
        }
        return lista_encomendas;
    }
    
    public void adiciona_no_pedido(String produto, int quantidade, Encomenda pedido, Loja loja){
        List<Produto> prods = loja.getProdutos();
        for (Produto prod: prods){
            if (produto.equals(prod.getDescricao()) && prod.getQuantidade() - quantidade >= 0){
                prod.setQuantidade(prod.getQuantidade()-quantidade);
                Produto novo = prod.clone();
                novo.setQuantidade(quantidade);
                pedido.adicionaProduto(novo);
            }
        }
        loja.setProdutos(prods);
    }
    
    public void atualizaCoordsT(String codT, double latitude, double longitude){
        Coordenadas nova = new Coordenadas(latitude,longitude);
        Transporte tra = getTransporte(codT);
        tra.setGps(nova);
        atualiza_Transporte(tra);
    }
    
    public void atualizaCoordsC(String codC, double latitude, double longitude){
        Coordenadas nova = new Coordenadas(latitude,longitude);
        Cliente cliente = getCliente(codC);
        cliente.setGps(nova);
        atualiza_Cliente(cliente);
    }
    
    public void aceita_encomendaT(String codT, String codE){
        Transporte tra = getTransporte(codT);
        Encomenda enc = getPedido(codE);
        enc.setHora(LocalDateTime.now());
        this.pedidos.remove(enc);
        List<Encomenda> transportadas = tra.getRegistoT();
        transportadas.add(enc);
        tra.setRegistoT(transportadas);
        tra.setKms(tra.getKms() + tra.getGps().distancia(enc.getVendedor().getGps()) + enc.getVendedor().getGps().distancia(enc.getDestino().getGps()));
        atualiza_Transporte(tra);
        this.aceites.add(enc);
    }
    
    public ArrayList<String> obtemTransportadas(String codT){
        Transporte t = getTransporte(codT);
        ArrayList<String> transportadas = new ArrayList<>();
        for (Encomenda e: t.getRegistoT()){
            transportadas.add(e.getCodEncomenda());
        }
        return transportadas;
    }
    
    public void aceita_encomenda(Encomenda enc, Cliente cliente){
        this.encomendas.add(enc);
        List<Encomenda> nova = cliente.getEncomenda();
        nova.add(enc);
        cliente.setencomenda(nova);
        cliente.setNenc(cliente.getNenc()+1);
        atualiza_Cliente(cliente);
        this.aceites.remove(enc);
    }
    
    public void classificar_transporte(String trans, int x){
        Transporte tra = getTransporteNome(trans);
        
        for(Encomenda e: this.encomendas){
            if (trans.equals(e.getTransporte().getNome())){
                tra.classifica(x);
            }
        }
        
        atualiza_Transporte(tra);
    }
    
    public void atualiza_Transporte(Transporte transporte){
        if (transporte instanceof Empresa){
            for(Empresa c: this.top10_empresas){
                if (transporte.equals(c)){
                    this.top10_empresas.remove(c);
                    this.top10_empresas.add((Empresa) transporte);
                }
            }
        }
        
        if(transporte instanceof Voluntario){
            for(Voluntario c: this.voluntarios){
                if (transporte.equals(c)){
                    this.voluntarios.remove(c);
                    this.voluntarios.add((Voluntario) transporte);
                }
            }
        }
    }
    
    public boolean loga_cliente(String cod, String pass){
        Cliente c = this.logins.logar(cod, pass);
        adicionaCliente(c);
        if (c != null){
            return true;
        }
        return false;
    }
    
    public boolean loga_loja(String cod, String pass){
        Loja l = this.logins.logar_loja(cod, pass);
        adicionaLoja(l);
        if (l != null){
            return true;
        }
        return false;
    }
    
    public void mudadisponivel(String codT, int n){
        Transporte tra = getTransporte(codT);
        if (n == 1){tra.setDisponivel(true);}
        if (n == 2){tra.setDisponivel(false);}
        atualiza_Transporte(tra);
    }
    
    public boolean loga_empresa(String cod, String pass){
        Empresa l = this.logins.logarempresa(cod, pass);
        adicionaTransporte(l);
        if (l != null){
            return true;
        }
        return false;
    }
    
    public boolean loga_voluntario(String codV, String pass){
        Voluntario l = this.logins.logarvoluntarios(codV, pass);
        adicionaTransporte(l);
        if (l != null){
            return true;
        }
        return false;
    }
    
    public void registaCliente(int num_encomendas, String codC, String nome, double lat, double lon, int classificacao, String pass){
        Cliente c = new Cliente(num_encomendas, codC, nome, new Coordenadas(lat,lon),classificacao, new ArrayList<Encomenda>());
        HashMap<Cliente, String> logs = this.logins.getlogins();
        logs.put(c,pass);
        adicionaCliente(c);
        this.logins.setlogins(logs);
    }
    
    public void registaLoja(String codL, String nome, double lat, double lon, String pass){
        Loja c = new Loja(codL, nome, new Coordenadas(lat,lon),new ArrayList<Produto>(), new ArrayList<Empresa>(), new ArrayList<Voluntario>(), new ArrayList<Encomenda>(), new ArrayList<Encomenda>(), new ArrayList<Cliente>());
        HashMap<Loja, String> logs = this.logins.getlogins_lojas();
        logs.put(c,pass);
        adicionaLoja(c);
        this.logins.setlogins_lojas(logs);
    }
    
    public void registaEmpresa(String codT, String nome, double lat, double lon, double raio, int nif, double preco_por_km, boolean aceita, String pass){
        Empresa c = new Empresa(codT, nome, 0, new Coordenadas(lat, lon), raio, 0, 0, true, new ArrayList<Encomenda>(), aceita, nif, preco_por_km, 0);
        HashMap<Empresa, String> logs = this.logins.getlogins_empresas();
        logs.put(c,pass);
        adicionaTransporte(c);
        this.logins.setlogins_empresas(logs);
    }
    
    public void registaVoluntario(String codT, String nome, double lat, double lon, double raio, String pass){
        Voluntario c = new Voluntario(codT, nome, 0, new Coordenadas(lat, lon), raio, 0, 0, true, new ArrayList<Encomenda>(), false);
        HashMap<Voluntario, String> logs = this.logins.getlogins_voluntarios();
        logs.put(c,pass);
        adicionaTransporte(c);
        this.logins.setlogins_voluntarios(logs);
    }
    
    public ArrayList<String> obteminfoFila(String codL){
        Loja loja = getLoja(codL);
        ArrayList<String> fila = new ArrayList<>();
        for (Encomenda e: this.pedidos){
            fila.add(e.getDestino().getNome());
        }
        return fila;
    }
    
    public ArrayList<String> lista_vol_raio(String codE){
        Encomenda encomenda = getPedido(codE);
        ArrayList<String> voluntario = new ArrayList<>();
        
        for (Voluntario n: this.voluntarios){
            if(n.getRaio() > encomenda.getVendedor().getGps().distancia(n.getGps()) && n.getRaio() > encomenda.getDestino().getGps().distancia(n.getGps()) && n.getDisponivel() && (!encomenda.getEncomendaMedica() || n.aceitoTransporteMedicamentos() == encomenda.getEncomendaMedica())){
                voluntario.add(n.getCodT());
            }
        }
        return voluntario;
    }
    
    public ArrayList<String> lista_empresa(String codE){
        Encomenda encomenda = getPedido(codE);
        ArrayList<String> empresa = new ArrayList<>();
        
        for (Empresa n: this.top10_empresas){
            if(n.getRaio() > encomenda.getVendedor().getGps().distancia(n.getGps()) && n.getRaio() > encomenda.getDestino().getGps().distancia(n.getGps()) &&  n.getDisponivel() && (!encomenda.getEncomendaMedica() || n.aceitoTransporteMedicamentos() == encomenda.getEncomendaMedica())){
                empresa.add(n.getCodT());
            }
        }
        return empresa;
    }
    
    public void gera_encomenda(String codT, String codE){
        Encomenda enc = getPedido(codE);
        Transporte tra = getTransporte(codT);
        double preco = enc.precototal();
        double peso = enc.pesototal();
        enc.setCodEncomenda(codE);
        enc.setTransporte(tra);
        enc.setPeso(peso);
        enc.setPreco(preco);
        if (tra instanceof Empresa){
            atualiza_Pedidos(enc);
        }
        if (tra instanceof Voluntario){
            atualiza_Encomenda(enc);
        }
    }
    
    public Transporte getTransporte(String codT){
        Transporte t = null;
        
        
        for(Empresa c: this.top10_empresas){
            if (codT.equals(c.getCodT())){
                t = c;
            }
        }
        
        
        
        for(Voluntario c: this.voluntarios){
            if (codT.equals(c.getCodT())){
                t = c;
            }
        }
        
        
        return t;
    }
    
    public Transporte getTransporteNome(String nome){
        Transporte t = null;
        
        
        for(Empresa c: this.top10_empresas){
            if (nome.equals(c.getNome())){
                t = c;
            }
        }
        
        
        
        for(Voluntario c: this.voluntarios){
            if (nome.equals(c.getNome())){
                t = c;
            }
        }
        
        
        return t;
    }
    
    public void fazPedido(String codC, String nomeL){
        
        
        int verifica = 1;
        
        Cliente cliente = getCliente(codC);
        
        Loja loja = getLoja(nomeL);
        
        System.out.println(loja.getNome());
        
        List<Produto> produtos = loja.getProdutos();
        
        System.out.println(produtos);
        
        Encomenda pedido = new Encomenda();
        
        while (verifica == 1 && produtos.size() > 0){
            System.out.println("Produtos disponíveis: ");
        
            for (Produto prod: produtos){
                System.out.println(prod.getDescricao()+": "+prod.getQuantidade());
            }
            
            System.out.println("Escolha o produto: ");
            
            Scanner sc = new Scanner(System.in);
            
            String produto = sc.next();
                        
            System.out.println("Escolha a quantidade: ");
                    
            int quantidade = sc.nextInt();

                        
            //tira a quantidade de um produto na loja escolhida
            //e adiciona esse produto na lista que está a ser criada
                        
            adiciona_no_pedido(produto, quantidade, pedido, loja);
                        
            System.out.println("Deseja mais algum?\n1. Sim\n2. Não");
                        
            verifica = sc.nextInt();
        }
        
        double preco = pedido.precototal();
        
        double peso = pedido.pesototal();
        
        System.out.println("Código da encomenda: ");
        
        Scanner sc = new Scanner(System.in);
        
        String encomenda = sc.next();
        
        pedido.setCodEncomenda(encomenda);
        
        System.out.println("Encomenda caracterizada como encomenda médica?\n1. Sim\n2. Não");
        
        int med = sc.nextInt();
        
        if (med == 1) pedido.setEncomendaMedica(true);
        else pedido.setEncomendaMedica(false);
        //obtem uma lista com os produtos todos que escolheu (pedido)
        
        pedido.setDestino(cliente);
        pedido.setPreco(preco);
        pedido.setVendedor(loja);
        pedido.setPeso(peso);
        
        
        loja.adicionaPedido(pedido);
        loja.adiciona_fila(cliente);
        
        adicionaPedido(pedido);
        atualiza_Loja(loja);
    }

    public double obteminfodinheiroT(String codT, LocalDateTime hora1, LocalDateTime hora2){
        Transporte t = getTransporte(codT);
        double x = 0;
        if (t instanceof Empresa){
            for (Encomenda enc : t.getRegistoT()){
                x += enc.getPreco();
            }
        }
        return x+((Empresa) t).getPrecoKm()*t.getRegistoT().size();
    }
    
    public void aceitaEncomendas(String codC){
        
        Cliente cliente = getCliente(codC);
        
        for(Encomenda e: this.aceites){
            if (e.getDestino().getCodC().equals(codC)){
                System.out.println(e.getCodEncomenda());   
            }
        }
        
        Scanner sc = new Scanner(System.in);
        
        String enc = sc.next();
                    
        Encomenda encomenda = getAceite(enc);
        
        System.out.println("Encomenda: "+encomenda.getCodEncomenda());
        System.out.println("Transportadora: "+encomenda.getTransporte().getNome());
        System.out.println("Preco: "+encomenda.getPreco());
                    
        System.out.println("Aceitar?\n1. Sim\n2. Não");
                    
        int aceitou = sc.nextInt();
                    
        if (aceitou == 1){
            aceita_encomenda(encomenda, cliente);
            System.out.println("Aceite com sucesso!");
        }
        else{
            System.out.println("Negada com sucesso!");
        }
    }
    
    public void mostraTransportadoras(String codC){
        for (Encomenda e: this.encomendas){
            if(e.getDestino().getCodC().equals(codC)){
                System.out.println(e.getTransporte().getNome());
            }
        }
    }
    
   /**
    * Efetua a leitura dos códigos dos utilizadores
    */
    public Estado parse(Estado estado){
        List<String> linhas = lerFicheiro("logs.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas){
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                case "Utilizador": 
                        Cliente u = parseCliente(linhaPartida[1]); // criar um Utilizador
                        estado.adicionaCliente(u);
                        break;
                case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        estado.adicionaTransporte(v);
                        break;
                case "Transportadora":
                        Empresa t = parseEmpresa(linhaPartida[1]);
                        estado.adicionaTransporte(t);
                        break; 
                case "Loja":
                        Loja l = parseLoja(linhaPartida[1]);
                        estado.adicionaLoja(l);
                        break;
                case "Encomenda":
                        Encomenda e = parseEncomenda(linhaPartida[1],estado);
                        estado.adicionaPedido(e);
                        break;
                case "Aceite":
                        estado.adicionaEncomenda(estado.getPedido(linhaPartida[1]));
                        break;
                default: 
                        System.out.println("Linha inválida.");
                        break;
                }
        }
        System.out.println("Feito!");
        return estado;
    }
   
  /**Utilizadores:
   * Utilizador:<CodUtilizador>,<Nome>,<GPS> 
  **/
  public Cliente parseCliente(String input){
        String[] campos = input.split(",");
        Random gerador = new Random();
        String codC = campos[0]; 
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas(gpsx,gpsy);
        int num_encomendas = getRandomNumberInRangeInt(0,10);
        int classificacao = getRandomNumberInRangeInt(0,5);
        List<Encomenda> encomenda = new ArrayList<>();
        return new Cliente(num_encomendas,codC,nome,gps,classificacao,encomenda);
    } 

    /**Voluntários 
     * Voluntario:<CodVoluntário>, <Nome>,<GPS> ,<Raio>
     **/
   public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        Random gerador = new Random();
        String codV = campos[0];
        String nome = campos[1];
        double kms =  gerador.nextDouble();
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas(gpsx,gpsy);
        double raio = Double.parseDouble(campos[4]);
        double mediaclassificacao = getRandomNumberInRangeDouble(0,5);
        double velocidade = getRandomNumberInRangeDouble(0,100);
        boolean disponivel = gerador.nextBoolean(); 
        List<Encomenda> registoT = new ArrayList<>();
        boolean certificado = gerador.nextBoolean();
        return new Voluntario(codV,nome,kms,gps,raio,mediaclassificacao,velocidade,disponivel,registoT,certificado);
   }
  
   /**Transportadoras 
    * Transportadora:<CodEmpresa>,<NomeEmpresa>,<GPS>,<NIF>,<raio>,<preco-por-km>
    **/
    public Empresa parseEmpresa(String input){
        String[] campos = input.split(",");
        Random gerador = new Random();
        String codE = campos[0];
        String nome = campos[1];
        double kms =  gerador.nextDouble();
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas gps = new Coordenadas(gpsx,gpsy);
        double raio = Double.parseDouble(campos[4]);
        double mediaclassificacao = getRandomNumberInRangeDouble(0,5);
        double velocidade = getRandomNumberInRangeDouble(0,100);
        boolean disponivel = gerador.nextBoolean(); 
        List<Encomenda> registoT = new ArrayList<>();
        boolean certificado = gerador.nextBoolean();
        Integer nif = Integer.parseInt(campos[4]);
        double preco_por_km = Double.parseDouble(campos[6]);
        int numero_de_encomendas = gerador.nextInt();
        return new Empresa(codE,nome,kms,gps,raio,mediaclassificacao,velocidade,disponivel,registoT,certificado,nif,preco_por_km,numero_de_encomendas);
   } 

   /**Lojas
    * Loja:<CodLoja>, <NomeLoja>
    **/
   public Loja parseLoja(String input){
        String[] campos = input.split(",");
        Random gerador = new Random();
        String codL = campos[0]; 
        String nome = campos[1];
        double gpsx = gerador.nextDouble();
        double gpsy = gerador.nextDouble();
        Coordenadas gps = new Coordenadas(gpsx,gpsy);
        ArrayList<Produto> produtos = new ArrayList<>();
        List<Empresa> empresas = new ArrayList<>();
        List<Voluntario> voluntarios = new ArrayList<>();
        List<Encomenda> enc_feitas = new ArrayList<>();
        List<Encomenda> pedidos = new ArrayList<>();
        List<Cliente> fila = new ArrayList<>();
        return new Loja(codL,nome,gps,produtos,empresas,voluntarios,enc_feitas,pedidos,fila);
   }
  
   /**Encomenda
    * Encomenda:<CodEncomenda>, <CodUtilizador>, <CodLoja>, <Peso>, <LinhaEncomenda>
    **/
    public Encomenda parseEncomenda(String input,Estado estado){
        String[] campos = input.split(",");
        Random gerador = new Random();
        String codE = campos[0];
            String codC = campos[1];
            String codL = campos[2];
            double peso = Double.valueOf(campos[3]);
            boolean encomendaMedica = gerador.nextBoolean();
            Cliente destino = estado.getCliente(codC);
            Loja vendedor = estado.getLoja(codL);
            LocalDateTime hora_transporte = LocalDateTime.now();
            Transporte transporte = new Empresa();
 
            ArrayList<Produto> linhaEncomenda = new ArrayList<>();
            int var = 0;
            int v = 4;
            int precototal = 0;
            while(v<campos.length){
                String codP = campos[v];
                String desc = campos[v+1];
                double quantidade = Double.parseDouble(campos[v+2]);
                double valorUnitario = Double.parseDouble(campos[v+3]);
                precototal += valorUnitario;
                double pesounitario = 0;
                Produto p = new Produto(codP,desc,valorUnitario,(int)quantidade,pesounitario);
                linhaEncomenda.add(p);
                v+=4;
            }
            return new Encomenda(precototal,destino,peso,linhaEncomenda,codE,encomendaMedica,vendedor,hora_transporte,transporte);
   }   

   private static int getRandomNumberInRangeInt(int min, int max) {
       if (min >= max) {
           throw new IllegalArgumentException("max must be greater than min");
        }
        Random r = new Random();
        return r.nextInt((max - min) + 1) + min;
   }

   private static double getRandomNumberInRangeDouble(double min, double max) {
      if (min >= max) {
          throw new IllegalArgumentException("max must be greater than min");
        }
            Random random = new Random();
            return random.nextDouble() * (max - min) + min;
   }
  
   /**
    * Lê ficheiro objeto
    */
   public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
   }
  
   /**
     * Lê objeto
     */
   public void lerObj(String nomeFich) throws IOException, ClassNotFoundException{
      ObjectInputStream nome = new ObjectInputStream(new FileInputStream(nomeFich));
        
      Estado novo = (Estado) nome.readObject();
      
      this.logins = novo.getLogins();
      this.top10_clientes = novo.getClientes();
      this.voluntarios = novo.getVoluntarios();
      this.top10_empresas = novo.getEmpresas();
      this.lojas = novo.getLojas();
      this.encomendas = novo.getPedidos();
      this.encomendas = novo.getEncomendas();
      this.aceites = novo.getAceites();
      this.produtos = novo.getProdutos();
      
      nome.close();
   }
  
  
   /**
    * Grava os campos da instância num ficheiro objeto.
    */
    public void gravaObj(String file) throws IOException {
        ObjectOutputStream oout = new ObjectOutputStream(new FileOutputStream(file));
        try {
            oout.writeObject(this);
        }
        catch (IOException e) {
            throw e;
        }
        oout.flush();
        oout.close();
   }

    /**
     * Método de recuperarEstado
     */
    //public UmCarroJa recuperarEstado(String filename) throws IOException, ClassNotFoundException {
        //FileInputStream fis = new FileInputStream(filename);
        //ObjectInputStream ois = new ObjectInputStream(fis);
        //UmCarroJa umCarroJa = (UmCarroJa) ois.readObject();
        //ois.close();
        //return umCarroJa;
    //}
}
