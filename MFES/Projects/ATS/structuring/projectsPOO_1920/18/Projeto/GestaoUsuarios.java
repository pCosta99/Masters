import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.util.List;
import java.util.ArrayList;
import java.io.*;
import java.util.TreeSet;
import java.util.Set;
import java.util.Iterator;

public class GestaoUsuarios{
    
    private Map<String,String> emails;
    private Map<String,Usuario> usuarios;
    
    public GestaoUsuarios(){
        this.usuarios=new HashMap<>();
        this.emails=new HashMap<>();
    }
    
    public GestaoUsuarios(Map<String,Usuario> l,Map<String,String> e){
        this.setUsuarios(l);
        this.setEmails(e);
    }
    
    public GestaoUsuarios(GestaoUsuarios gu){
        this.usuarios=gu.getUsuarios();
        this.emails=gu.getEmails();
    }
    
    public Map<String,Usuario> getUsuarios(){
        return this.usuarios.entrySet().stream().collect(Collectors.toMap(e->e.getKey(),e->e.getValue()));
    }
    
    public void setUsuarios(Map<String,Usuario> nu){
        this.usuarios=new HashMap<>();
        nu.entrySet().forEach(e -> {this.usuarios.put(e.getKey(),e.getValue());});
    }
    
    public Map<String,String> getEmails(){
        return this.emails.entrySet().stream().collect(Collectors.toMap(e->e.getKey(),e->e.getValue()));
    }
    
    public void setEmails(Map<String,String> n){
        this.emails=new HashMap<>();
        n.entrySet().forEach(e->{this.emails.put(e.getKey(),e.getValue());});
    }
    
    public GestaoUsuarios clone(){
        return new GestaoUsuarios(this);
    }
       
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoUsuarios ge = (GestaoUsuarios) obj;
        return ge.getUsuarios().equals(this.usuarios);
    }
    
    public String toString(){
        StringBuilder sb=new StringBuilder();
        sb.append("Gestão Usuarios:\n").append(this.usuarios).append("\n");
        return sb.toString();
    }  
    
    public void addUsuario (Usuario u){
        this.usuarios.put(u.getCodigo(),u);
    }
    
    public void removeUsuario (String cod){
        this.usuarios.remove(cod);
    }
    
    public void removeUsuario (Usuario u){
        this.usuarios.remove(u.getCodigo());
    }
    
    public boolean temUsuario(String cod){
        return this.usuarios.containsKey(cod);
    }
    
    public void addEmail(String mail,String codu){
        this.emails.put(mail,codu);
    }
    
    public boolean containsEmail(String mail){
        return this.emails.containsKey(mail);
    }
    
    public boolean passCorreta(String mail,String pass){
        return this.usuarios.get(this.emails.get(mail)).passCorreta(pass);
    }
    
    public String getCodigo(String mail){
        return this.emails.get(mail);
    }
    
    public Usuario getUsuario(String cod){
        return this.usuarios.get(cod);
    }
    
    public void setEmailPass(String cod,String mail, String pass){
        this.emails.put(mail,cod);
        this.usuarios.get(cod).setEmail(mail);
        this.usuarios.get(cod).setPassword(pass);
    }
    
    public String paraEstado(){
        StringBuilder sb=new StringBuilder();
        this.usuarios.values().stream().forEach(v->sb.append(v.paraEstado()).append("\n"));
        this.usuarios.values().stream().filter(v -> (v.getClass().equals(Loja.class) || v.getClass().equals(LojaComFila.class))).
        forEach(u->sb.append(u.getEstadoEncomendas()));
        return sb.toString();        
    }
    
    public String getEncomendasEfetuadas(){
        StringBuilder sb =new StringBuilder();
        this.usuarios.values().stream().filter(u -> u instanceof UtilizadorBasico).
                map(Usuario::getStrEncEfetuadas).forEach(u -> sb.append(u));
        return sb.toString();
    }    
     
    public String getDadosLogin(){
        StringBuilder sb=new StringBuilder();
        this.emails.entrySet().stream().forEach(u-> sb.append("Login:").                                            
                                            append(u.getValue()).append(",").
                                            append(u.getKey()).append(",").
                                            append(this.usuarios.get(u.getValue()).getPassword()).
                                            append("\n"));
        return sb.toString();
    }
    
    public void addDadosLogin(Map<String,String[]> dados){
        dados.entrySet().stream().forEach(u -> {
            this.usuarios.get(u.getKey()).setEmail(u.getValue()[0]);
            this.usuarios.get(u.getKey()).setPassword(u.getValue()[1]);
            this.emails.put(u.getValue()[0],u.getKey());
        });
    }
    
    public void addEncomendasEfetuadas(List<EncomendaEfetuada> encomendas){
        encomendas.stream().forEach(enc -> {
            this.usuarios.get(enc.getCodUtilizador()).addEncomendaEfetuada(enc);
            this.usuarios.get(enc.getCodLoja()).addEncomendaEfetuada(enc);
            this.usuarios.get(enc.getCodTransportador()).addEncomendaEfetuada(enc);});
    }
    
    public String getEncomendasAceites(){
        StringBuilder sb=new StringBuilder();
        this.usuarios.values().stream().filter(v->v instanceof Loja).map(v->(Loja) v).forEach(v->sb.append(v.getCodigos()));
        return sb.toString();        
    }
    
    public String getListaDeTransportadores(){
        StringBuilder sb=new StringBuilder();
        this.usuarios.values().stream().filter(v->v instanceof EmpresaTransportadora).map(v->(EmpresaTransportadora) v)
                                .forEach(v->sb.append("Empresa transportadora: codigo: ").append(v.getCodigo()).
                                append("; nome: ").append(v.getNome()).append(".\n"));
        this.usuarios.values().stream().filter(v->v instanceof Voluntario).map(v->(Voluntario) v)
                                .forEach(v->sb.append("Voluntario: codigo: ").append(v.getCodigo()).
                                append("; nome: ").append(v.getNome()).append(".\n"));
        return sb.toString();
    }
    
    public void addEncomenda(String cod,Encomenda enc){
        this.usuarios.get(cod).addEncomenda(enc);
    }
    
    public void solicitarEncomenda (Encomenda enc){
        
        ((Loja) this.usuarios.get(enc.getCodLoja())).addEncTransporteSolicitado(enc.getCodLoja());
    }
    
    public void transporteDeEncomendaAceite(Encomenda enc,String tra){
        List<Proposta> p=((UtilizadorBasico) this.usuarios.get(enc.getCodUtilizador())).aceitouEncomenda(enc.getCodEncomenda());
        ((Loja) this.usuarios.get(enc.getCodLoja())).removeEncomendaSolicitada(enc.getCodEncomenda());
        if (p!=null) p.stream().map(Proposta::getCod).
            forEach(v ->{((EmpresaTransportadora) this.usuarios.get(v)).removeEncAceite(enc.getCodEncomenda());});

    }
    
    public void transporteDeEncomendaAceite(String codU,String enc,String tra){
        ((UtilizadorBasico) this.usuarios.get(codU)).aceitouEncomenda(enc);
        ((Loja) this.usuarios.get(this.usuarios.get(codU).getEncomenda(enc).getCodLoja())).removeEncomendaSolicitada(enc);
        ((Transportador) this.usuarios.get(tra)).addEncomenda(((UtilizadorBasico) this.usuarios.get(codU)).getEncomenda(enc));
    }
    
    public String getPedidosDeEncomendas(){
        StringBuilder sb=new StringBuilder();
        this.usuarios.values().stream().filter(v->v instanceof Loja).
                    map(c -> (Loja) c).forEach(v->{
                        sb.append("Codigo da loja: ").append(v.getCodigo()).append(":\n");
                        v.getEncomendasSolicitadas().stream().forEach(a->sb.append("  Encomenda:").append(a).append("\n"));
                    });
        return sb.toString();
    }
    
    public String getStrEncomendasEfetuadas(String cod){
        return this.usuarios.get(cod).getEncomendasEfetuadas().toString();
    }
    
    public boolean contains(String cod){
        return this.usuarios.containsKey(cod);
    }
    
    public boolean containsTransportador(String cod){

        if (this.usuarios.containsKey(cod)){
            return (this.usuarios.get(cod) instanceof Transportador);
        }
        return false;
    }
    
    public boolean containsLoja(String cod){
        if (this.usuarios.containsKey(cod)){
            return this.usuarios.get(cod) instanceof Loja;
        }
        return false;
    }
    
    public void setClassificacao(String codtra,String cod,int i){
        ((Transportador) this.usuarios.get(codtra)).addClassificacao(cod,Double.valueOf(i));
    }
    
    public String getClassificacao(String cod){
        Double d = ((Transportador) this.usuarios.get(cod)).getValorClassificacao();
        if (d==0.0) return "Sem classificacao.\n";
        else return d.toString()+"\n";
    }
    
    public void encomendaEntregue(String codT,String codEncomenda,LocalDateTime t,float custo){
        String codU,codL;
        codU=this.usuarios.get(codT).getEncomenda(codEncomenda).getCodUtilizador();
        codL=this.usuarios.get(codT).getEncomenda(codEncomenda).getCodLoja();
        this.usuarios.get(codT).encomendaEntregue(codEncomenda,t,codT,custo);
        this.usuarios.get(codU).encomendaEntregue(codEncomenda,t,codT,custo);
        this.usuarios.get(codL).encomendaEntregue(codEncomenda,t,codT,custo);
    }
    
    public boolean usuarioSemEmail(String cod){
        String s = this.usuarios.get(cod).getEmail();
        return s==null;
    }

    public boolean eEncomendaSolicitada(String codL,String codEnc){
        return ((Loja) this.usuarios.get(codL)).encPressicaTransportador(codEnc);
    }
    
    public void aceitaTransportarEncomenda(String codT,String codL,String codEnc,float custo){
        String codU=this.usuarios.get(codL).getCodUtilizador(codEnc);
        ((UtilizadorBasico) this.usuarios.get(codU)).novaPropostaDeTransporte(codEnc,codT,custo);
    }
    
    public void aceitaTransportarEncomendaVoluntario(String codT,String codL,String codEnc){
        this.aceitaTransportarEncomenda(codT,codL,codEnc,0);
        this.transporteDeEncomendaAceite(this.usuarios.get(codL).getEncomenda(codEnc),codT);
    }
    
    public String getTabelaPrecos(){
        StringBuilder sb=new StringBuilder();
        sb.append("TABELA DE PRECOS\n");
        this.usuarios.values().stream().filter(v->v instanceof EmpresaTransportadora).map(e->(EmpresaTransportadora) e).
        forEach(v->sb.append(v.getPreco()));
        return sb.toString();
    }
    
    public boolean containsEncomenda(String codU,String codE){
        return this.usuarios.get(codU).containsEncomenda(codE);
    }
    
    public boolean jaFoiSinalizada(String codL,String codE){
        String utilizador=this.usuarios.get(codL).getEncomenda(codE).getCodUtilizador();
        return !(((UtilizadorBasico) this.usuarios.get(utilizador)).encomendaPronta(codE));
    }
    
    public void sinalizarEncomendaPronta(String codL,String codE){
        String utilizador=this.usuarios.get(codL).getEncomenda(codE).getCodUtilizador();
        ((UtilizadorBasico) this.usuarios.get(utilizador)).addEncomendaPronta(codE);
    }
    
    public void addEncomendas(List<Encomenda> encomendas, List<String> aceites){
        encomendas.stream().forEach(v -> 
            {this.usuarios.get(v.getCodLoja()).addEncomenda(v.clone());
             this.usuarios.get(v.getCodUtilizador()).addEncomenda(v.clone());
             if (aceites.contains(v.getCodEncomenda())){
                 ((UtilizadorBasico) this.usuarios.get(v.getCodUtilizador())).addEncComTransportador(v.getCodEncomenda());
                 ((Loja) this.usuarios.get(v.getCodLoja())).addEncComTransportador(v.getCodEncomenda());}
             else{((UtilizadorBasico) this.usuarios.get(v.getCodUtilizador())).addEncomendaPronta(v.getCodEncomenda());}});
    }
    
    public void novoPedidoDeEncomenda(String codU,String codLoja,List<LinhaEncomenda> l){
        ((Loja) this.usuarios.get(codLoja)).novoPedidoDeEncomenda(codU,l);
    }
    
    public String getGEncomendas (String cod){
        return this.usuarios.get(cod).getGEncomendas().toString();
    }
    
    public String informacao(String cod){
        return this.usuarios.get(cod).informacaoencomendas();
    }
    
    public String verTransportadoresnoRaio(String cod){
        GPS g=this.usuarios.get(cod).getGps();
        StringBuilder sb=new StringBuilder();
        this.usuarios.values().stream().filter(v -> v instanceof Transportador).map(e->(Transportador) e).
                filter(t->t.estaNoRaio(g,t.getRaio())).
                forEach(v->sb.append("Codigo transportador: ").append(v.getCodigo()).append("\n"));
        return sb.toString();
    }
    
    public boolean estanoraio(String codL,String codT){
        Loja l= (Loja) this.usuarios.get(codL);
        Transportador t= (Transportador) this.usuarios.get(codT);
        
        return l.estaNoRaio(t.getGps(),t.getRaio());
    }
    
    
    public String dezutilizadores(){
        //int j,maior;
        Usuario u;
        StringBuilder sb=new StringBuilder();
        TreeSet<UtilizadorBasico> ubs=new TreeSet <UtilizadorBasico>(new ComparatorUB());
        this.usuarios.values().stream().filter(v->v instanceof UtilizadorBasico).map(v->(UtilizadorBasico) v)
                  .forEach(v->ubs.add(v));
        Iterator<UtilizadorBasico> itr=ubs.iterator();
        sb.append("Lista dos dez utilizadores que mais usam:\n");
        for (int i=0;i<10 && itr.hasNext(); i++){
            u=itr.next();
            sb.append(i+1);
            sb.append(": Codigo utilizador: ").append(u.getCodigo()).append("; nome: ").append(u.getNome()).append("\n");
            
        }
        return sb.toString();
    }
    
    public String dezempresas(){
        //int j,maior;
        Usuario u;
        StringBuilder sb=new StringBuilder();
        Set<EmpresaTransportadora> ets=new TreeSet <>(new ComparatorET());
        this.usuarios.values().stream().filter(v->v instanceof EmpresaTransportadora).map(c->(EmpresaTransportadora) c)
                  .forEach(e->ets.add(e));
        Iterator<EmpresaTransportadora> itr=ets.iterator();
        sb.append("Lista dos dez empresas que mais usam:\n");
        for (int i=0;i<10 && itr.hasNext(); i++){
            u=itr.next();
            sb.append(i+1);
            sb.append(": Codigo empresa: ").append(u.getCodigo()).append("; nome: ").append(u.getNome()).append("\n");
            
        }
        return sb.toString();
    }
    
    public void mudarNome(String cod,String input){
        this.usuarios.get(cod).setNome(input);
    }
    
    public void mudargps(String cod,float la,float lo){
        this.usuarios.get(cod).setGps(new GPS(la,lo));
    }
    
    public void mudarEmail(String cod,String input){
        this.usuarios.get(cod).setEmail(input);
    }
    
    public void mudarPass(String cod,String input){
        this.usuarios.get(cod).setPassword(input);
    }

    public int solicitarTransporte(String codEnc, String codU){
        String codUB,codL;
        Encomenda e = this.usuarios.get(codU).getEncomenda(codEnc);
        codUB = e.getCodUtilizador();
        codL = e.getCodLoja();
        
        if (((Loja) this.usuarios.get(codL)).naoTemTransportador(codEnc)){
            ((Loja) this.usuarios.get(codL)).addEncTransporteSolicitado(codEnc);
        }else{return 1;}//Codigo de encomenda nao valido
        return 0;//sucesso
    }
    
}
